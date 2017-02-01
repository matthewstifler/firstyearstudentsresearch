require(devtools)
install_github("bmschmidt/wordVectors")
require(wordVectors)

#downloading data
students.ids <- students.data %>%
  lapply(function(x)
    sapply(x, function(y)
        y$uid
      )
    )

students.walls <- lapply(students.ids, function(x)
    lapply(x, function(y){
      Sys.sleep(0.3) # VK API restrictions
      paste0("https://api.vk.com/method/", "wall.get", 
             "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
             "&owner_id=", as.character(y),
             "&filter=", "owner",
             "&count=", "100") %>% 
      GET() %>% 
      content(as = "parsed") %>%
      .$response %>% 
      `[`(-1) #no count
      })
  )

#jesus, I had to subset it before, obviously
#subsetting walls by thise who have schools in their profiles
students.with.schools.walls <- lapply(students.walls, function(x) {
  lapply(x, function(y) {
    if(length(y) > 0) { #if there is at least one post
      if(y[[1]]$from_id %in% students.with.shools.ids) return(y) #if the id is in list, return wall 
    }
  })
})

#let's see how many there are copy posts from our sample
students.with.schools.walls %>% sapply(function(x) sapply(x, function(y) sapply(y, function(z) z$post_type))) %>% unlist %>% as.factor() %>% summary() #61.6% of copy posts!


#building post-id list
students.with.schools.posts <- lapply(students.with.schools.walls, function(x) {
  lapply(x, function(y) {
    if (length(y) > 0) { #if we picked this wall, unpicked walls are left as elements with length 0 (WHY)
      lapply(y, function(z) {
        return(
          list(text = z$text,
               id = z$to_id,
               type = z$post_type,
               date = z$date,
               coord = ifelse(is.null(z$geo$coordinates), NA, z$geo$coordinates)))
      }) 
    }
  })
})

#unfolding post-id list into a data frame
posts.df <- lapply(students.with.schools.posts, function(x) {
  lapply(x, function(y) {
    y %>% lapply(as.data.frame) %>% rbind.fill
  }) %>% rbind.fill() 
}) %>% rbind.fill()

#adding city variable to the posts.df (look up every id from posts.df in a id-city df, built previously from data on students)
posts.df$city <- sapply(posts.df$id, 
                        function(x) students.ids.cities.df[students.ids.cities.df$id %in% x, 2]) %>% 
  as.factor()

posts.df$is.local <- posts.df$city == "2"
posts.df$text <- posts.df$text %>% as.character()

#ISSUE: 100 last posts give questionable distribution, increasing greatly towards 2017
#let's see how the dates of the first post are distributed
first.post.dates <- by(posts.df, posts.df$id, function(x) x$date %>% `[`(length(x$date)), simplify = T)  %>% as.numeric()

#for how many first post was after finishing school?
#!st June
(first.post.dates > 1464739200) %>% summary #18%! :(
#???
#1st September
(first.post.dates > 1472688000) %>% summary #11%! :(


posts.df.subset <- posts.df[posts.df$date > 1464739200,] #only posts after 1st June 2016

#----------working with text----------
model  <- read.vectors("data/web.model.bin")

compare.text2kw <- function (text, vspace, kwvector, stopwords, average=TRUE) {
  words <- unlist(strsplit(text, " "))
  words <- words[!words %in% stopwords]
  words <- words[words %in% rownames(vspace)]
  
  if (length(words)>0) {
    return(as.vector(wordVectors::cosineSimilarity(vspace[[words, average=average]], kwvector)))
  } else {
    return(NA)
  }
}

