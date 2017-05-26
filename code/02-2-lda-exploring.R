require(mallet)
require(dplyr)

#running LDA on different number of topics to save the topics and look for the relevant keywords
num.topics <- c(10, 25, 37, 50, 75, 100, 200, 300)

mallet.instances <- mallet.import(as.character(posts.df.subset.edited.da.hw$id), as.character(posts.df.subset.edited.da.hw$text), "data/stoplist", token.regexp = "[\\p{L}\\p{N}-]*\\p{L}+") #as.character so that there is no error (important!)
topic.labels <- list()

for (i in num.topics) {
  topic.model <- MalletLDA(num.topics = i) # количество тем
  topic.model$loadDocuments(mallet.instances) 
  topic.model$setAlphaOptimization(20, 50)
  topic.model$train(2000)
  topic.model$maximize(25)
  
  topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
  
  topic.labels[[i]] <- list()
  for (k in 1:nrow(topic.words)) {
    topic.labels[[i]][[k]] <- mallet.top.words(topic.model, topic.words[k,], 6)$words
  }
  names(topic.labels[[i]]) <- as.character(1:nrow(topic.words))
}

#-----------a sample of the consequential analysis of the LDA topic.labels-----------

##topic #6 is about the city
wordVectors::nearest_to(model.self, model.self[[topic.labels[[10]][6] %>% unlist ]], n = 50)  
#taking a look at some of the words to see if it makes sense to include them in the keywords
posts.df.no.empty.posts$text %>% str_detect("разночинный") %>% summary

#we can also see if locals use some word more or less than non-locals by comparing subsetted by the word text 
#localness distribution to the general distribution
posts.df.no.empty.posts$is.local %>% table() %>% `/`(sum(.)) #58 vs 42
posts.df.no.empty.posts$is.local[posts.df.no.empty.posts$text %>% str_detect("ул")] %>% table() %>% `/`(sum(.)) #60 vs 40
