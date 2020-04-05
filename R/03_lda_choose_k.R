
library(tidyverse)

library(tidytext)

library(textmineR)

library(coda)

load("data_derived/20_newsgroups_formatted.RData")

# convert dtm back into text
docs <- Dtm2Docs(dtm)

docs <- tibble(
  id = names(docs),
  text = docs
  )

# use tidytext to unnest tokens
docs <- docs %>%
  unnest_tokens(
    output = word,
    input = text,
    token = "words",
    ) 

docs$rownum <- seq_len(nrow(docs))

# randomly sample tokens
train_lda <- sample(docs$rownum, round(nrow(docs) / 2))

docs <- docs %>%
  mutate(
    train_test = case_when(
      rownum %in% train_lda ~ "train", 
      ! rownum %in% train_lda ~ "test"
      )
    )

# assemble two dtms - train and test
dtm_lda1 <- docs %>%
  filter(train_test == "train") %>%
  select(id, word) %>%
  count(id, word) %>%
  cast_sparse(id, word, n)
  
dtm_lda2 <- docs %>%
  filter(train_test == "test") %>%
  select(id, word) %>%
  count(id, word) %>%
  cast_sparse(id, word, n)

save(dtm_lda1, dtm_lda2, file = "data_derived/lda_split_dtm.RData")

# random search over a range of k
# alternatively, use sigopt for search over k
# store coherence and likelihood

k_range <- sample(5:2000, 100)

# create a directory to store models for future reference if needed
if (! file.exists("data_derived/lda_choose_k")) {
  dir.create("data_derived/lda_choose_k")
}

alpha <- 0.01
beta <- colSums(dtm_lda1) / sum(dtm_lda1) * 135

model_eval <- parallel::mclapply(
  k_range,
  function(k){
    m <- FitLdaModel(
      dtm = dtm_lda1,
      k = k,
      iterations = 200,
      burnin = 175,
      alpha = alpha,
      beta = beta,
      optimize_alpha = FALSE,
      calc_likelihood = TRUE,
      calc_coherence = TRUE,
      calc_r2 = FALSE,
      cpus = 2
      )
    
    geweke <- geweke.diag(
      m$log_likelihood$log_likelihood[m$log_likelihood$iteration >= 100],
      frac1 = 0.5,
      frac2 = 0.5)
    
    converged <- abs(geweke$z) < 1.96
    
    coherence_in_sample <- mean(m$coherence)
    
    vocab_intersect <- intersect(colnames(m$phi), colnames(dtm_lda2))
    
    coherence_out_sample <- CalcProbCoherence(
      phi = m$phi[, vocab_intersect],
      dtm = dtm_lda2[, vocab_intersect]
    )
    
    coherence_out_sample <- mean(coherence_out_sample)
    
    likelihood_in_sample <- CalcLikelihood(
      dtm = dtm_lda1,
      phi = m$phi,
      theta = m$theta,
      cpus = 2
    )
    
    theta_hat <- predict(
      object = m,
      newdata = dtm_lda2,
      method = "dot",
    )
    
    theta_hat[rowSums(theta_hat) == 0, ] <- 1 / ncol(m$phi)
    
    likelihood_out_sample <- CalcLikelihood(
      dtm = dtm_lda2[, vocab_intersect],
      phi = m$phi[, vocab_intersect],
      theta = theta_hat,
      cpus = 2
    )
    
    perplexity_in_sample <- exp(-1 * likelihood_in_sample / sum(dtm_lda1))
    
    perplexity_out_sample <- exp(-1 * likelihood_out_sample / sum(dtm_lda2))
    
    # make a one-row eval tibble
    eval <- tibble(
      k = k,
      converged = converged,
      coherence_in = coherence_in_sample,
      coherence_out = coherence_out_sample,
      likelihood_in = likelihood_in_sample,
      likelihood_out = likelihood_out_sample,
      perplexity_in = perplexity_in_sample,
      perplexity_out = perplexity_out_sample
    )
    
    # save model
    filename <- paste0(
      "data_derived/lda_choose_k/lda_k_",
      k,
      ".RData")
    
    save(eval, m, file = filename)
    
    # return eval tibble
    return(eval)
    
  }, mc.cores = parallel::detectCores() - 1)

# combine the tibble eval

eval_lda <- do.call(rbind, model_eval)

save(eval_lda, file = "data_derived/eval_lda_choose_k.RData")
