require(stringr)
#-----------Getting emojis from all posts----------

#Overall there are 34238 emoji occurences
posts.df.no.empty.posts$text.raw %>% str_extract_all(pattern = "[\U1f300-\U1F6FF]") %>% unlist %>% length
