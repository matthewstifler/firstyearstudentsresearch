#devtools::install_github("bmschmidt/wordVectors")
require(dplyr)
require(stringr)
require(wordVectors)

compare.text2kw <- function (text, vspace, kwvector, average=TRUE) {
  words <- unlist(strsplit(text, " "))
  words <- words[words %in% rownames(vspace)]
  return(as.vector(cosineSimilarity(vspace[[words, average=average]], kwvector)))
}

stopwords <- readLines("data/stoplist")

writeLines(posts.df.no.empty.posts$text %>% tm::removeWords(readLines("data/stoplist")), "data/posts-vec")
model.self <- train_word2vec("data/posts-vec", "data/model.self.bin", threads = 8, window = 8, min_count = 2, vectors = 200, iter = 15, force = T)
