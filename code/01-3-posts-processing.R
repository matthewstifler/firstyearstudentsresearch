require(dplyr)
require(plyr)

#for each wall, for each post take five elements, combine them into a dataframe, combine for all posts, for all walls
posts.df <- lapply(students.walls.v2, function(x) {
  lapply(x, function(y) {
    if (!is.null(y)) {
      return(
        as.data.frame(list(text = y$text,
                           user.id = y$to_id,
                           type = y$post_type,
                           date = y$date,
                           coord = ifelse(is.null(y$geo$coordinates), NA, y$geo$coordinates)))
      )
    }
  }) %>% rbind.fill() 
}) %>% rbind.fill()

posts.df <- students.ids.cities.df %>%
  left_join(posts.df, by = "user.id")

posts.df$user.id <- as.factor(posts.df$user.id)
posts.df$text <- as.character(posts.df$text)

#text processing
posts.df$text.raw <- posts.df$text
posts.df$text <- gsub("(https://)(\\S*)", " ", posts.df$text) %>% 
  gsub("ё", "е", .) %>%
  gsub("Ё", "Е", .) %>%
  gsub("[^А-Яа-я]", " ", .) %>%
  stringr::str_to_lower() %>% 
  trimws() %>%
  gsub("\\s{2,}", " ", .) #remove 2+ spaces

writeLines(posts.df$text, "data/posts-vec")
posts.df$text <- system("mystem -cld data/ideas-vec | cat", intern = T) %>% 
  #TODO: Get rid of mediate file (ideas-vec)
  gsub("[^А-Яа-я]", " ", .) %>% 
  gsub("\\s{2,}", " ", .) %>% 
  trimws()

posts.df.no.empty.posts <- posts.df[!(posts.df$text == ""),]