
library(tidyverse)

library(tidylda)

library(Matrix)

load("data_derived/20_newsgroups_formatted.RData")

# # set parallel options
# library(furrr)
# 
# plan(multiprocess)

# set random seed
set.seed(90210)

# subset the DTM so we get results in meaningful time
lda_rows <- sample(1:nrow(dtm), 1000)

d <- dtm[lda_rows, ]

d <- d[, colSums(d) > 0]

# declare additional global variables
alpha <- 0.01

beta <- colSums(d) / sum(d) * 135

k <- 565

# define three procedures for each that I am to run

proc1 <- function(d, k, alpha, beta) {
  # proc 1 builds a single model for 1000 iterations
  
  m <- tidylda(
    dtm = d,
    k = k,
    iterations = 1000,
    burnin = 900,
    alpha = alpha,
    beta = beta,
    optimize_alpha = FALSE,
    calc_likelihood = TRUE,
    calc_r2 = FALSE,
    return_data = FALSE
  )
  
  m
}

proc2 <- function(d, k, alpha, beta) {
  # proc 2 builds a model in two stages: 200 iterations then 800 iterations
  m <- tidylda(
    dtm = d,
    k = k,
    iterations = 200,
    burnin = -1,
    alpha = alpha,
    beta = beta,
    optimize_alpha = FALSE,
    calc_likelihood = TRUE,
    calc_r2 = FALSE,
    return_data = FALSE
  )
  
  m1_likelihood <- m$log_likelihood
  
  m <- refit(
    object = m,
    dtm = d,
    iterations = 800,
    burnin = 700,
    optimize_alpha = FALSE,
    calc_likelihood = TRUE,
    calc_r2 = FALSE,
    return_data = FALSE,
    additional_k = 0,
    phi_as_prior = FALSE
  )
  
  m2_likelihood <- m$log_likelihood %>%
    mutate(iteration = iteration + 200)
  
  list(
    m_final = m, 
    likelihoods = rbind(m1_likelihood, m2_likelihood)
  )
}

proc3 <- function(d, k, alpha, beta) {
  # proc 3 builds a model in fice stages: 200 iterations each
  
  m <- tidylda(
    dtm = d,
    k = k,
    iterations = 200,
    burnin = -1,
    alpha = alpha,
    beta = beta,
    optimize_alpha = FALSE,
    calc_likelihood = TRUE,
    calc_r2 = FALSE,
    return_data = FALSE
  )
  
  likelihoods <- vector(
    mode = "list", 
    length = 5
    )
  
  likelihoods[[1]] <- m$log_likelihood
  
  for (j in 2:4) {
    
    m <- refit(
      object = m,
      dtm = d,
      iterations = 200,
      burnin = -1,
      optimize_alpha = FALSE,
      calc_likelihood = TRUE,
      calc_r2 = FALSE,
      return_data = FALSE,
      additional_k = 0,
      phi_as_prior = FALSE
    )
    
    likelihoods[[j]] <- m$log_likelihood
  }
  
  m <- refit(
    object = m,
    dtm = d,
    iterations = 200,
    burnin = 100,
    optimize_alpha = FALSE,
    calc_likelihood = TRUE,
    calc_r2 = FALSE,
    return_data = FALSE,
    additional_k = 0,
    phi_as_prior = FALSE
  )
  
  likelihoods[[5]] <- m$log_likelihood
  
  for (j in 2:length(likelihoods)) {
    likelihoods[[j]]$iteration <- likelihoods[[j - 1]]$iteration + 200
  }
  
  list(
    m_final = m,
    likelihoods = do.call(rbind, likelihoods)
  )
}

# compile procedures into a list
# then run in parallel
procs <- list(
  proc1,
  proc2,
  proc3
)

result <- parallel::mclapply(
  procs, 
  function(p){
    p(
      d = d,
      k = k, 
      alpha = alpha,
      beta = beta
    )
  },
  mc.cores = length(procs)
)

save(result, lda_rows, file = "data_derived/lda_retrain.RData")

