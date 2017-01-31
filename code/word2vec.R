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

#jesus, I had to subset in before, obviously