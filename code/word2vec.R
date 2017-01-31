require(devtools)
install_github("bmschmidt/wordVectors")

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
students.with.schools.walls <- lapply(students.walls, function(x) {
  lapply(x, function(y) {
    if(length(y) > 0) { #if there is at least one post
      if(y[[1]]$from_id %in% students.with.shools.ids) return(y) #if the id is in list, return wall 
    }
  })
})

students.with.schools.posts <- lapply(students.with.schools.walls, function(x) {
  lapply(x, function(y) {
    if (length(y) > 0) { #if we picked this wall, unpicked walls are left as elements with length 0 (WHY)
      lapply(y, function(z) {
        return(list(text = z$text, id = z$to_id))
      }) 
    }
  })
})

posts.df <- lapply(students.with.schools.posts, function(x) {
  lapply(x, function(y) {
    y %>% lapply(as.data.frame) %>% rbind.fill
  }) %>% rbind.fill() 
}) %>% rbind.fill()

posts.df$city <- sapply(posts.df$id, function(x) students.ids.cities.df[students.ids.cities.df$id %in% x,2])
