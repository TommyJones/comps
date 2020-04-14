
library(tidyverse)

library(tidytext)

library(textmineR)

library(coda)

load("data_derived/20_newsgroups_formatted.RData")


# set random seed
set.seed(90210)

# random search over a range of k
k_range <- sample(5:2000, 100)

# create a directory to store models for future reference if needed
if (! file.exists("data_derived/lda_choose_k")) {
  dir.create("data_derived/lda_choose_k")
}


# function to evaluate outputs of model
calc_eval <- function(m, dtm_train, dtm_test) {
  
  # check convergence
  geweke <- geweke.diag(
    m$log_likelihood$log_likelihood[
      m$log_likelihood$iteration >= (max(m$log_likelihood$iteration) - 100)
      ],
    frac1 = 0.5,
    frac2 = 0.5
  )
  
  geweke <- geweke$z
  
  # vocabulary corrections of dtm
  dtm_train <- dtm_train[, colSums(dtm_train) > 0]
  
  dtm_test <- dtm_test[, colSums(dtm_test) > 0]
  
  # vocabulary correction of model
  m$phi <- m$phi[, colnames(dtm_train)]
  
  m$phi <- m$phi / rowSums(m$phi)
  
  # in sample coherence
  coherence_in <- mean(m$coherence)
  
  # in sample likelihood
  likelihood_in <- CalcLikelihood(
    dtm = dtm_train,
    phi = m$phi,
    theta = m$theta,
    cpus = 2
  )
  
  # in sample perplexity
  perplexity_in <- exp(-1 * likelihood_in / sum(dtm_train)) 
  
  # out-of-sample prediction
  theta_hat <- predict(m, newdata = dtm_test, method = "dot")
  
  theta_hat[rowSums(theta_hat) == 0, ] <- 1 / ncol(theta_hat)
  
  theta_hat <- theta_hat / rowSums(theta_hat)
  
  # intersecting vocab
  vocab_intersect <- intersect(colnames(m$phi), colnames(dtm_test))
  
  # out-of-sample coherence
  coherence_out <- mean(
    CalcProbCoherence(
      m$phi[, vocab_intersect], 
      dtm_test[, vocab_intersect]
    )
  )
  
  # out-of-sample likelihood
  phi_adjusted <- m$phi[, vocab_intersect]
  
  phi_adjusted <- phi_adjusted / rowSums(phi_adjusted)
  
  likelihood_out <- CalcLikelihood(
    dtm = dtm_test[, vocab_intersect],
    phi = phi_adjusted,
    theta = theta_hat,
    cpus = 2
  )
  
  # out-of-sample perplexity
  perplexity_out <- exp(-1 * likelihood_out / sum(dtm_test)) 
  
  # return result
  eval <- tibble(
    k = nrow(m$phi),
    geweke = geweke,
    coherence_in = coherence_in,
    coherence_out = coherence_out,
    likelihood_in = likelihood_in,
    likelihood_out = likelihood_out,
    perplexity_in = perplexity_in,
    perplexity_out = perplexity_out
  )
  
  eval
}

model_eval <- parallel::mclapply(
  k_range,
  function(k){
    
    # randomly sample 1,000 docs to train on 
    train_rows <- sample(1:nrow(dtm_bigram), 1000)
    
    dtm_lda1 <- dtm_bigram[train_rows, ]
    
    dtm_lda2 <- dtm_bigram[-train_rows, ]
    
    # need smaller vocabulary set to calculate likelihood without 
    # overflow/underflow error
    dtm_lda1 <- dtm_lda1[, colSums(dtm_lda1) > 0]
    
    dtm_lda2 <- dtm_lda2[, colSums(dtm_lda2) > 0]
    
    alpha <- 0.05
    
    beta <- 0.01
    
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
    

    # make a one-row eval tibble
    eval <- calc_eval(
      m = m,
      dtm_train = dtm_lda1,
      dtm_test = dtm_lda2
    )
    
    # save model
    filename <- paste0(
      "data_derived/lda_choose_k/lda_k_",
      k,
      ".RData")
    
    save(eval, m, train_rows, file = filename)
    
    # return eval tibble
    return(eval)
    
  }, mc.cores = parallel::detectCores() - 1)

# combine the tibble eval

eval_lda <- do.call(rbind, model_eval)

save(eval_lda, file = "data_derived/eval_lda_choose_k.RData")

beepr::beep(3)
