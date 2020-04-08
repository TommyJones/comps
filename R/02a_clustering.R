# does clustering on the whole dataset. Hope you have RAM!


library(textmineR)

library(cluster)

load("data_derived/20_newsgroups_formatted.RData")

# get term frequencies and idf vector
tf <- TermDocFreq(dtm)

# calculate cosine similarity/distance off of tfidf
csim <- t(dtm) * tf$idf

csim <- t(csim)

csim <- csim / sqrt(rowSums(csim * csim)) # normaize

csim <- csim %*% t(csim)

csim <- as.matrix(csim)

cdist <- as.dist(1 - csim)

# hierarchical clustering
h <- hclust(cdist, "ward.D")

# plot(h, labels = rep(".", nrow(dtm)))

clust_range <- seq(5, 2000, by = 5) 

# randomize order for more parallel efficiency
clust_range <- clust_range[sample(seq_along(clust_range), length(clust_range))]

eval_mat <- parallel::mclapply(
  clust_range[sample(seq_along(clust_range), 100)], # random sample to do fewer runs
  function(k){
    
    clust <- cutree(h, k)
    
    # calculate coherence
    ctm <- Cluster2TopicModel(dtm = dtm, clustering = clust, cpus = 1)
    coh <- CalcProbCoherence(phi = ctm$phi, dtm = dtm)
    
    # calculate distance from every point to every cluster
    clust_pts <- lapply(
      unique(clust), 
      function(cl){
        names(clust)[clust == cl]
      })
    
    clust_dists <- lapply(
      clust_pts, 
      function(x){
        rowMeans(1 - csim[, x])
      })
    
    clust_dists <- do.call(cbind, clust_dists)
    
    # get silhouette scores
    silh <- numeric(length(clust))
    
    for (j in seq_along(silh)) {
      
      a <- clust_dists[j, clust[j]]
      
      b <- min(clust_dists[j, -clust[j]])
      
      silh[j] <- (b - a) / max(b, a)
      
    }
    
    # get average within-cluster distances
    avg_dist <- sapply(
      1:ncol(clust_dists),
      function(cl){
        mean(clust_dists[clust == cl, cl])
      }
    )
    
    # return a result
    data.frame(
      k = k, 
      avg_silh = mean(silh), 
      coherence = mean(coh),
      avg_within_dist = mean(avg_dist),
      stringsAsFactors = FALSE
      )
    
  },
  mc.cores = parallel::detectCores() - 1)

eval_mat <- do.call(rbind, eval_mat)

# ggplot(data = eval_mat) + geom_line(mapping = aes(x = k, y = coherence))

save(tf, file = "data_derived/term_freq_all.RData")

save(csim, cdist, file = "clustering_distances.RData")

save(eval_mat, file = "data_derived/cluster_eval.RData")
