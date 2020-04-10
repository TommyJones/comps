################################################################################
# This script splits data into two training sets and a test set
################################################################################

rm(list = ls())

### Load necessary libraries ----
library(textmineR)

library(stringr)

library(tidyverse)

### load 20 newsgroups data and build dtm ----
if (! file.exists("data_raw")) { # if statement so I don't duplicate download etc.
  download.file("http://www.cs.cmu.edu/afs/cs.cmu.edu/project/theo-20/www/data/data_raw.tar.gz",
                destfile = "data_raw.tar.gz")
  
  untar(tarfile = "data_raw.tar.gz",
        exdir = "data_raw/")
}



docnames <- list.files("data_raw/20_newsgroups", full.names = TRUE, recursive = TRUE)

docs <- parallel::mclapply(docnames, function(d){
  doc <- scan(d, what = "character", sep = "\n")
  
  doc <- paste(doc, collapse = "\n")
  
  doc
}, mc.cores = 8) %>% 
  unlist() %>%
  stringr::str_conv("UTF-8")

names(docs) <- docnames

doc_class <- stringr::str_split(docnames, pattern = "/") %>%
  sapply(function(x) x[3])


# create a dtm of unigrams
dtm_unigram <- CreateDtm(doc_vec = docs)

tf_unigram <- as_tibble(
  TermDocFreq(dtm_unigram)
)

# filter based on words... 
# appearing in 5 or more documents, and
# appearing in less than every document
# note this caps minimum term frequency at 5
dtm_unigram <- dtm_unigram[, tf_unigram$doc_freq >= 5 &
                             tf_unigram$doc_freq < nrow(dtm_unigram) &
                             tf_unigram$term_freq >= 10]

# filtered term frequency mat
tf_unigram_filtered <- as_tibble(
  TermDocFreq(dtm_unigram)
)

# create dtm of unigrams and bigrams
# follow same procs above
dtm_bigram <- CreateDtm(doc_vec = docs, ngram_window = c(1,2))

tf_bigram <- as_tibble(
  TermDocFreq(dtm_bigram)
)

dtm_bigram <- dtm_bigram[, tf_bigram$doc_freq >= 5 &
                           tf_bigram$doc_freq < nrow(dtm_bigram) & 
                           tf_bigram$term_freq >= 10]

tf_bigram_filtered <- as_tibble(
  TermDocFreq(dtm_bigram)
)

### save the things we need for future use ----
save(
  docs, 
  doc_class, 
  dtm_unigram,
  tf_unigram,
  tf_unigram_filtered,
  dtm_bigram,
  tf_bigram,
  tf_bigram_filtered,
  file = "data_derived/20_newsgroups_formatted.RData"
  )




