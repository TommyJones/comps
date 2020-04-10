
library(tidyverse)

library(tidylda)

library(textmineR)

library(cluster)

# get a cluster summary matrix
load("data_derived/cluster_eval.RData")

load("data_derived/clustering_distances.RData")

load("data_derived/20_newsgroups_formatted.RData")

clustering <- cutree(h, k = 1000)

cluster_tm <- Cluster2TopicModel(
  dtm = dtm_bigram,
  clustering = clustering
  )

cluster_summary <- tibble(
  cluster = seq_len(ncol(cluster_tm$theta)),
  num_docs = colSums(cluster_tm$theta),
  num_tokens = (rowSums(dtm_bigram) %*% cluster_tm$theta)[1, ],
  coherence = CalcProbCoherence(cluster_tm$phi, dtm)
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
load("data_derived/lda_retrain.RData")

lda_baseline_dist <- CalcHellingerDist(
  result[[1]]$phi
)

lda_12_dist <- get_hellinger(
  result[[1]]$phi,
  result[[2]]$m_final$phi
)

lda_13_dist <- get_hellinger(
  result[[1]]$phi,
  result[[3]]$m_final$phi
)

lda_23_dist <- get_hellinger(
  result[[2]]$m_final$phi,
  result[[3]]$m_final$phi
)

# compare lda to clustering
lda_clust_dist <- get_hellinger(
  result[[1]]$phi,
  cluster_tm$phi[, colnames(result[[1]]$phi)]
)

save(lda_baseline_dist,
     lda_12_dist,
     lda_13_dist,
     lda_23_dist,
     lda_clust_dist,
     file = "data_derived/compare_models.RData"
     )

beepr::beep(9)