
library(tidyverse)

library(tidylda)

library(textmineR)

library(cluster)

# get a cluster summary matrix
load("data_derived/cluster_eval.RData")

load("data_derived/clustering_distances.RData")

load("data_derived/cluster_solution.RData")

load("data_derived/20_newsgroups_formatted.RData")

cluster_summary <- tibble(
  cluster = seq_len(ncol(cluster_tm$theta)),
  num_docs = colSums(cluster_tm$theta),
  num_tokens = (rowSums(dtm_bigram) %*% cluster_tm$theta)[1, ],
  coherence = CalcProbCoherence(cluster_tm$phi, dtm_bigram)
)

tt <- GetTopTerms(cluster_tm$phi, 3)

tt <- apply(tt, 2, function(x) paste(c(x, "..."), collapse = ", "))

cluster_summary$top_terms <- tt

silh <- silhouette(clustering, cdist)

silh <- summary(silh)

cluster_summary$silhouette <- silh$clus.avg.widths

save(cluster_summary, file = "data_derived/cluster_summary.RData")

rm(list = c("csim", "cdist")) # free up some memory

# declare a function to get cosine similarity between two matrices
get_hellinger <- function(mat1, mat2) {
  
  # in case we are comparing two models
  rownames(mat1) <- paste0(rownames(mat1), ".1")
  
  # combine matrices
  sim_mat <- rbind(mat1, mat2)
  
  # get hellinger dist
  sim_mat <- CalcHellingerDist(sim_mat)
  
  # get result
  output <- sim_mat[1:nrow(mat1),
                    (nrow(mat1) + 1):nrow(sim_mat)]
  
}

# get_csim <- function(mat1, mat2) {
# 
#   # in case we are comparing two models
#   rownames(mat1) <- paste0(rownames(mat1), ".1")
# 
#   # normalize rows
#   mat1 <- mat1 / sqrt(rowSums(mat1 * mat1))
# 
#   mat1[is.na(mat1)] <- 0
# 
#   mat2 <- mat2 / sqrt(rowSums(mat2 * mat2))
# 
#   mat2[is.na(mat2)] <- 0
# 
#   # return result
#   mat1 %*% t(mat2)
# }

# compare the lda models
# using the transpose of theta
# rationale: this should be cleaner than phi or gamma because 
# (a) it doesn't have as many categories and 
# (b) no expectation that it would be a power law distribution
load("data_derived/lda_retrain.RData")

lda_baseline_dist <- CalcHellingerDist(
  t(result[[1]]$theta)
)

lda_12_dist <- get_hellinger(
  t(result[[1]]$theta),
  t(result[[2]]$models[[2]]$theta)
)

lda_13_dist <- get_hellinger(
  t(result[[1]]$theta),
  t(result[[3]]$models[[5]]$theta)
)

lda_23_dist <- get_hellinger(
  t(result[[2]]$models[[2]]$theta),
  t(result[[3]]$models[[5]]$theta)
)

# compare lda to clustering
lda_clust_dist <- get_hellinger(
  t(result[[1]]$theta),
  t(cluster_tm$theta[lda_rows, ])
)

# get lda and clustering into the same 2d space
vocab_intersect <- intersect(
  colnames(result[[1]]$phi),
  colnames(cluster_tm$phi)
  )

# cluster_topic_dist <- rbind(
#   result[[1]]$phi[, vocab_intersect],
#   cluster_tm$phi[, vocab_intersect]
# )

cluster_topic_dist <- rbind(
  t(result[[1]]$theta),
  t(cluster_tm$theta[lda_rows, ])
)


cluster_topic_dist <- CalcHellingerDist(cluster_topic_dist)

cluster_topic_layout <- cmdscale(d = cluster_topic_dist)

topic_layout <- cmdscale(d = lda_baseline_dist)

save(
  cluster_topic_dist,
  cluster_topic_layout,
  lda_baseline_dist,
  topic_layout,
  file = "data_derived/model_layouts.RData"
)

save(lda_baseline_dist,
     lda_12_dist,
     lda_13_dist,
     lda_23_dist,
     lda_clust_dist,
     cluster_topic_dist,
     file = "data_derived/compare_models.RData"
     )

# compare models across runs for model 3
model_list <- result[[3]]$models

comp_indices <- list(c(1,2),
                     c(2,3),
                     c(3,4),
                     c(4,5),
                     c(1,5))

comp_dists <- 
  parallel::mclapply(
    comp_indices,
    function(x) {
      get_hellinger(
        t(model_list[[x[1]]]$theta),
        t(model_list[[x[2]]]$theta)
      )
    },
    mc.cores = length(comp_indices)
  )

comp_topic_349 <- lapply(model_list, function(x) x$phi[349, ])

comp_topic_349 <- do.call(cbind, comp_topic_349)

comp_topic_349 <- t(comp_topic_349[
  c("image", "processing", "images", "analysis", "data", 
    "image_processing", "tools", "tool", "user", "display"), 
])

comp_topic_349 <- 
  comp_topic_349 %>%
  as_tibble() %>%
  mutate(iterations = c(200, 400, 600, 800, 1000)) %>%
  pivot_longer(
    cols = -iterations,
    names_to = "token",
    values_to = "value"
  )

save(
  comp_topic_349,
  comp_dists,
  model_list,
  file = "data_derived/compare_transfer.RData"
)

beepr::beep(0)
