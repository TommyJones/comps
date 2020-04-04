################################################################################
# This script splits data into two training sets and a test set
################################################################################

rm(list = ls())

### Load necessary libraries ----
library(textmineR)

library(stringr)

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

dtm <- CreateDtm(doc_vec = docs) # stopwords English and SMART

dim(dtm)

# prune vocab so that only words appearing in 3 or more documents are included
dtm <- dtm[, colSums(dtm > 0) >= 3]

dim(dtm)

### sample rows into three groups ----

idx <- seq_len(nrow(dtm))

train1 <- sample(idx, 6665)

test <- setdiff(idx, c(train1))

### save the things we need for future use ----
save(docs, dtm, train1, test, doc_class, file = "data_derived/20_newsgroups_formatted.RData")




