library(tidyverse)

library(textmineR)

library(coda)

# calc_eval <- function(m, dtm_train, dtm_test) {
#   
#   # check convergence
#   geweke <- geweke.diag(
#     m$log_likelihood$log_likelihood[
#       m$log_likelihood$iteration >= (max(m$log_likelihood$iteration) - 100)
#       ],
#     frac1 = 0.5,
#     frac2 = 0.5
#   )
#   
#   converged <- abs(geweke$z) < 1.96
#   
#   # vocabulary corrections of dtm
#   dtm_train <- dtm_train[, colSums(dtm_train) > 0]
#   
#   dtm_test <- dtm_test[, colSums(dtm_test) > 0]
#   
#   # vocabulary correction of model
#   m$phi <- m$phi[, colnames(dtm_train)]
#   
#   m$phi <- m$phi / rowSums(m$phi)
#   
#   # in sample coherence
#   coherence_in <- mean(m$coherence)
#   
#   # in sample likelihood
#   likelihood_in <- CalcLikelihood(
#     dtm = dtm_train,
#     phi = m$phi,
#     theta = m$theta,
#     cpus = 1
#   )
#   
#   # in sample perplexity
#   perplexity_in <- exp(-1 * likelihood_in / sum(dtm_train)) 
#   
#   # out-of-sample prediction
#   theta_hat <- predict(m, newdata = dtm_test, method = "dot")
#   
#   theta_hat[rowSums(theta_hat) == 0, ] <- 1 / ncol(theta_hat)
#   
#   theta_hat <- theta_hat / rowSums(theta_hat)
#   
#   # intersecting vocab
#   vocab_intersect <- intersect(colnames(m$phi), colnames(dtm_test))
#   
#   # out-of-sample coherence
#   coherence_out <- mean(
#     CalcProbCoherence(
#       m$phi[, vocab_intersect], 
#       dtm_test[, vocab_intersect]
#     )
#   )
#   
#   # out-of-sample likelihood
#   phi_adjusted <- m$phi[, vocab_intersect]
#   
#   phi_adjusted <- phi_adjusted / rowSums(phi_adjusted)
#   
#   likelihood_out <- CalcLikelihood(
#     dtm = dtm_test[, vocab_intersect],
#     phi = phi_adjusted,
#     theta = theta_hat,
#     cpus = 1
#   )
#   
#   # out-of-sample perplexity
#   perplexity_out <- exp(-1 * likelihood_out / sum(dtm_test)) 
#   
#   # return result
#   eval <- tibble(
#     k = nrow(m$phi),
#     converged = converged,
#     coherence_in = coherence_in,
#     coherence_out = coherence_out,
#     likelihood_in = likelihood_in,
#     likelihood_out = likelihood_out,
#     perplexity_in = perplexity_in,
#     perplexity_out = perplexity_out
#   )
#   
#   eval
# }


files <- list.files("data_derived/lda_choose_k", full.names = T)

files <- setdiff(files, "data_derived/lda_choose_k/archive")

# load("data_derived/20_newsgroups_formatted.RData")
# 
# eval_lda <- parallel::mclapply(
#   files, 
#   function(f) {
#     
#     load(f)
#     
#     eval <- calc_eval(
#       m = m,
#       dtm_train = dtm[train_rows, ],
#       dtm_test = dtm[-train_rows, ]
#     )
#     
#     eval
#     
#   },
#   mc.cores = parallel::detectCores() - 1)

load_eval <- function(f){
  load(f)
  
  eval
}


eval_lda <- lapply(
  files,
  load_eval
)

eval_lda <- do.call(rbind, eval_lda)

# files2 <- list.files("data_derived/lda_choose_k", full.names = T)
# 
# files2 <- setdiff(files2, c("data_derived/lda_choose_k/archive", files))
# 
# eval_lda <- rbind(
#   eval_lda,
#   do.call(
#     rbind,
#     lapply(files2, load_eval)
#   )
# )
# 
# files <- c(files, files2)

ggplot(eval_lda, mapping = aes(x = k, y = coherence_in)) + 
  geom_point(color = "#66c2a5") + 
  geom_smooth() + 
  geom_point(mapping = aes(x = k, y = coherence_out), color = "#fc8d62") +
  geom_smooth(mapping = aes(x = k, y = coherence_out))

ggplot(eval_lda, mapping = aes(x = k, y = perplexity_out)) +
  geom_point(color = "#66c2a5") +
  geom_smooth()


l <- loess(coherence_out ~ k, data = eval_lda)
l$x[which.max(l$fitted)]

l2 <- loess(perplexity_out ~ k, data = eval_lda)
l2$x[which.min(l2$fitted)]

l3 <- loess(likelihood_out ~ k, data = eval_lda)
l3$x[which.max(l3$fitted)]
