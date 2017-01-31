require(assertthat)
require(devtools)
require(dplyr)
require(plyr)
require(httr)

#----------Misc----------
findTextElementInVectorByPattern <- function(vector, pattern) {
  indices <- grepl(pattern, vector) %>%
    `[`(1:length(vector), .)
  return(setNames(indices, vector[indices]))
}

#----------VK API stuff----------
source_gist("4b515fa4d35f253dddea1dcf4ec0afe0") #https://gist.github.com/matthewstifler/4b515fa4d35f253dddea1dcf4ec0afe0
vk <- get.vk.connector(code = "1d1380c6fb8c790166")

database.getUniversities <-function(vk.get_data, q, country_id, city_id, offset, count = 1000) {
  method = "database.getUniversities"
  return(vk.get_data(method = method, q = q, country_id = country_id, city_id = city_id, offset = offset, count = count))
}

#----------Work----------
unis.spb <- database.getUniversities(vk.get_data, q = "", country = 1, city_id = 2, offset = 0)
unis.spb.no.count <- unis.spb[-1]
unis.spb.ids <- sapply(unis.spb, function(x) x[[1]])[-1] #first field is count, thus [-1]
unis.spb.names <- sapply(unis.spb, function(x) x[2][[1]])[-1]

#top N universities by the rate of "quality" of enrolling students (higher rate -- more attractive)
unis.selected.by.quality <- unis.spb.no.count[c(1, 14, 48, 49, 38, 37, 75, findTextElementInVectorByPattern(unis.spb.names, "ГЭУ"), 39, 63, 51, 44, 45, 61, 30, 24, 32, 52, 60, 3, 29, 288, 84, 55, 27, 12, 56, 36)]

#top N universities by the number of state-paid places
#12, 56: two ids for the same university, they are pretty equal
#function call: 4 ids for a single uni
unis.selected.by.number.of.places <- unis.spb.no.count[c(1, 51, 45, 63, 48, 30, 55, 3, 61, 44, 27, 12, 56, 60, 50, findTextElementInVectorByPattern(unis.spb.names, "ГЭУ"), 49, 52, 36, 29, 288, 37, 14, 38, 84, 24, 32, 42)]
#WARNING: they are almost the same, 29 common ids by intersect 

#wow there is no need for a function for each method! kewl!
#vk.get_data(method = "users.search", count = 1, university = unis.selected.by.number.of.places[[1]]$id)
#it doesn't work, no one knows why

#so, I have to use this (NOTE: work this issue out)
#first try:
raw.spbu <- paste0("https://api.vk.com/method/", "users.search", 
       "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
       "&university=", "1",
       "&school_year=", "2016",
       "&count=", "1000",
       "&fields=", "education,schools") %>% 
  GET() %>% 
  content(as = "parsed") %>%
  .$response

lapply(raw.spbu[-1], as.data.frame) %>% rbind.fill %>% .$schools.city %>% as.factor %>% summary() 
#15% def from out of city, ca. 29% -- def. from the city, 55% didn't state
#thus, final sample on non-locals should be ca. 3650, and ca. 7475 for locals

students.data <- lapply(unis.selected.by.number.of.places, function(x) {
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

#deleting people with >1 university
subset.list <- sapply(students.data, function(x) sapply(x, function(y) length(y$universities) > 1))

for (i in 1:length(students.data)) {
  students.data[[i]] <- students.data[[i]][!subset.list[[i]]]
}

#problem of people with more than 1 school
#ALSO: this line can be modified to retrieve other description statistics
sapply(students.data, 
       function(x) { #take each university 
         sapply(x, function(y) { #take each person
           y$schools[[length(y$schools)]]$year_graduated #get graduation year for the last (length(y$schools)) school
           })
         }) %>%
  unlist %>%
  as.factor %>%
  summary
#those who graduated from their last school earlier than 2016 are just 4% of those who mention schools (32%), and 1.2% of the entire sample, so we can 

#faulty df, but still something
students.data %>% lapply(function(x) x  %>% lapply(as.data.frame) %>% rbind.fill)  %>% rbind.fill()  %>% dim()


#TODO
#0. Outsource all the functions definitions in a separate file, from all the scripts