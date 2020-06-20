library(tidylda)

library(textmineR)

library(tidyverse)

topics_by_class <- rowMeans(dtm_bigram) * big_theta

topics_by_class <- by(
  data = topics_by_class,
  INDICES = doc_class,
  FUN = function(x){
    if(is.null(nrow(x))) {
      x / sum(x)
    } else {
      y <- colSums(x)
      y / sum(y)
    }
  }
)

topics_by_class <- do.call(rbind, topics_by_class)

topics_by_class <- as_tibble(data.frame(class = rownames(topics_by_class), topics_by_class))

topics_by_class <- topics_by_class %>% pivot_longer(-class, names_to = "topic")

topics_by_class$topic <- as.numeric(str_replace_all(topics_by_class$topic, "X", ""))



ggplot(topics_by_class %>% filter(class == "alt.atheism")) +
  geom_bar(aes(x = topic, y = value), stat = "identity")


