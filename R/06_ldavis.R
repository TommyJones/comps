# run LDAvis for better depth

library(textmineR)

library(tidylda)

library(LDAvis)

load("data_derived/lda_retrain.RData")

load("data_derived/20_newsgroups_formatted.RData")

# create json files for each model

models <- list(
  result[[1]],
  result[[2]]$models[[2]],
  result[[3]]$models[[5]]
)

json <- parallel::mclapply(
  models,
  function(m) {
    createJSON(
      phi = m$phi, 
      theta = m$theta, 
      doc.length = rowSums(dtm_bigram[lda_rows, ]), 
      vocab = colnames(m$phi), 
      term.frequency = colSums(dtm_bigram[, colnames(m$phi)])
    )
  },
  mc.cores = 3
)

save(
  json,
  file = "data_derived/ldavis_json.RData"
)

# serVis(json[[1]], out.dir = 'vis', 
#        open.browser = TRUE)

