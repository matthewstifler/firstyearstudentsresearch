require(httr)

#---------Loading all needed posts----------

# For each person: get 100 posts from the wall; 
# Check the date of the last one;
# If date > july.first, then load 100 more, if date < july.first, then compare all posts of the last batch dates to july.first
# Get the ones that are TRUE for date > july.first

july.first <- 1467320400
students.walls.v2 <- list()

for (i in i:length(students.with.shools.ids)) {
  Sys.sleep(1)
  message(Sys.time())
  message(paste0("Profile number ", i, " is being processed"))
  
  students.walls.v2[[i]] <- list()
  
  count <- content(GET(paste0("https://api.vk.com/method/", "wall.get", 
                              "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                              "&owner_id=", as.character(students.with.shools.ids[i]),
                              "&filter=", "owner",
                              "&count=", "1")), as = "parsed")$response[[1]]
  
  if (!is.null(count)) {
    if (count > 0) {
      for (j in 1:(count %/% 100)) {
        Sys.sleep(1)
        current.data <- list()
        current.data <- content(GET(paste0("https://api.vk.com/method/", "wall.get", #get a batch and store it in according slots
                                           "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                                           "&owner_id=", as.character(students.with.shools.ids[i]),
                                           "&filter=", "owner",
                                           "&offset=", ((j - 1) * 100),
                                           "&count=", "100")), as = "parsed")$response[-1]
        
        if (length(current.data) > 0) {
          if (j == 1 & !is.null(current.data[[1]]$is_pinned)) {
            if (current.data[[1]]$date < july.first) {
              current.data <- current.data[-1]
            }
          }
        }
        
        data.dates <- sapply(current.data, function(x) x$date)
        
        if (any(data.dates < july.first) || j == (count %/% 100)) {
          current.data <- current.data[!(data.dates < july.first)] #subset to only those after the date
          if (length(current.data) > 0) {
            students.walls.v2[[i]][((j - 1)*100 + 1):((j - 1)*100 + length(current.data))] <- current.data 
          }
          break
        }
        else {
          students.walls.v2[[i]][((j - 1)*100 + 1):((j - 1)*100 + length(current.data))] <- current.data
        }
      } 
    }
  }
  
  message(paste0(length(students.walls.v2[[i]]), " posts were downloaded"))
  message(paste0("Overall, ", sum(sapply(students.walls.v2, length)), " posts were downloaded"))
  message("============================")
}


