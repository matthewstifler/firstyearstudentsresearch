require(dplyr)
require(plyr)
require(httr)

#----------Gathering users data----------

# In this part, universities in the city are picked for the sample, then
# data for all users who stated they study at either of the universities is gathered

#Getting id and name for all universities in Saint Petersburg
unis.spb <- paste0("https://api.vk.com/method/", "database.getUniversities", 
                   "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                  "&q=", "",
                  "&country=", 1,
                  "&city_id=", 2,
                  "&offset=", 0,
                  "&count=", 1000
                  ) %>% 
  GET() %>% 
  content(as = "parsed") %>%
  .$response %>% 
  `[`(-1)

unis.spb.names <- sapply(unis.spb, function(x) x[2][[1]])[-1]

#this step was done manually according to an online rating
unis.spb <- unis.spb[c(1, 51, 45, 63, 48, 30, 55, 3, 61, 44, 27, 12, 56, 60, 50, which(stringr::str_detect(unis.spb.names, "ГЭУ")), 49, 52, 36, 29, 288, 37, 14, 38, 84, 24, 32, 42)]

#downloading data for students from the selected univerisites
students.data <- lapply(unis.spb, function(x) {
  Sys.sleep(0.3) # VK API restrictions
  paste0("https://api.vk.com/method/", "users.search", 
         "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
         "&university=", as.character(x$id),
         "&school_year=", "2016",
         "&count=", "1000",
         "&age_to=", "21",
         "&fields=", "education,schools,universities") %>% 
    GET() %>% 
    content(as = "parsed") %>%
    .$response %>% 
    `[`(-1) #no count
})

students.data <- unlist(students.data, recursive = F)
names(students.data) <- sapply(students.data, function(x) x$uid)

#deleting people with >1 university
students.data <- students.data[!sapply(students.data, function(x) length(x$universities) > 1)]

#how many students graduated each year?
sapply(students.data, function(y) { #take each person
           y$schools[[length(y$schools)]]$year_graduated #get graduation year for the last (length(y$schools)) school
         }) %>%
  unlist %>%
  as.factor %>%
  summary

#turn list into an id-city data frame
students.ids.cities.df <- sapply(students.data, function(x) {
  if(length(x$schools) > 0) return(as.data.frame(list(id = y$uid, city = y$schools[[length(y$schools)]]$city), stringsAsFactors = FALSE))
}) %>% rbind.fill()