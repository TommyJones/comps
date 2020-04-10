# does clustering based off samples to validate with sampled LDA result


# load libraries
library(textmineR)

library(cluster)

library(tidyverse)

# set random seed
set.seed(90210)

# load necessary data
load("data_derived/20_newsgroups_formatted.RData")

# sample 100 batches of 2000 documents
batches <- lapply(
  1:100,
  function(b){
    sample(1:nrow(dtm_bigram), 2000)
  }
)

# for each bactch of documents, sample 20 numbers of clusters between 5 and 1995
batches <- lapply(
  batches,
  function(b){
    k_sample <- sample(5:1995, 20)
    
    list(doc_sample = b, k_sample = k_sample)
  })

# for each batch...
# calculate TF-IDF
# hierarchically cluster
# for each number of sampled clusters in the batch...
# calculate evaluation metrics
eval_cluster <- parallel::mclapply(
  batches,
  function(b){
    # calculate tf-idf
    d <- dtm_bigram[b$doc_sample, ]
    
    d <- d[, colSums(d) > 0]
    
    tf <- TermDocFreq(d)
    
    # calculate cosine similarity/distance
    csim <- t(d) * tf$idf
    
    csim <- t(csim)
    
    csim <- csim / sqrt(rowSums(csim * csim)) # normaize
    
    csim <- csim %*% t(csim)
    
    csim <- as.matrix(csim)
    
    diag(csim) <- 1 # correct rounding errors
    
    cdist <- as.dist(1 - csim)
    
    # hierarchically cluster
    h <- hclust(cdist, "ward.D")
    
    # for the sampled range of cluster numbers
    eval <- lapply(
      b$k_sample,
      function(k){
        try({
        clust <- cutree(h, k)
        
        # calculate coherence
        ctm <- Cluster2TopicModel(dtm = d, clustering = clust, cpus = 1)
        coh <- CalcProbCoherence(phi = ctm$phi, dtm = d)
        
        # calculate distance from every point to every cluster
        clust_pts <- lapply(
          unique(clust), 
          function(cl){
            which(clust == cl)
          })
        
        clust_dists <- lapply(
          clust_pts, 
          function(x){
            if (length(x) > 1) {
              colMeans(1 - csim[x, ])
            } else {
              1 - csim[x, ]
            }
          })
        
        clust_dists <- do.call(cbind, clust_dists)
        
        # get average within-cluster distances
        avg_dist <- sapply(
          1:ncol(clust_dists),
          function(cl){
            mean(clust_dists[clust == cl, cl])
          }
        )
        
        # get silhouette scores
        silh <- silhouette(clust, dmatrix = csim)
        
        silh <- summary(silh)
        
        silh <- silh$avg.width

        # return a result
        tibble(
          k = k, 
          avg_silh = mean(silh), 
          coherence = mean(coh),
          avg_within_dist = mean(avg_dist)
        )
        
        })
        
      })
    
    # return final results
    do.call(rbind, eval)
    
  },
  mc.cores = parallel::detectCores() - 1
)

eval_cluster <- do.call(rbind, eval_cluster)

save(eval_cluster, file = "data_derived/eval_cluster_samples.RData")
  
beepr::beep(8)

