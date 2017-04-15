require(httr)

#---------Loading all needed posts----------

july.first <- 1467320400

#I need, for each person: get 100 posts from the wall; check the date of the last one;
#if date > july.first, then load 100 more, if date < july.first, then compare all posts of the last batch dates to july.first
#get the ones that are TRUE for date > july.first
#NOTE: nice chance to utilize recursion

wall.get.posts.recursive <- function(id, date, data) {
   current.data <- content(GET(paste0("https://api.vk.com/method/", "wall.get", #get a batch and store it in according slots
                     "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                     "&owner_id=", as.character(id),
                     "&filter=", "owner",
                     "&offset=", ((counter - 1) * 100),
                     "&count=", "100")), as = "parsed")$response[-1] #no 'count' field
  
  #if pinned post is earlier than date, it has to be excluded, otherwise the terminating condition will fire prematurely
  if (counter == 1 & !is.null(current.data[[1]]$is_pinned)) {
    if (current.data[[1]]$date < date) {
      current.data <- current.data[-1]
    }
  }
  
  data.dates <- sapply(current.data, function(x) x$date)
  
  #terminating condition
  if (any(data.dates < date) || counter == count / 100) {
    current.data <- current.data[!(data.dates < date)] #subset to only those after the date
    data[((counter - 1)*100 + 1):((counter - 1)*100 + length(current.data))] <- current.data
    return(data)
  }
  else {
    counter <<- counter + 1
    data[((counter - 1)*100 + 1):((counter - 1)*100 + length(current.data))] <- current.data
    return(wall.get.posts.recursive(id, date, data))
  }
}

wall.get.posts.since.date <- function(id, date) { #returns a list of all posts till specified date
  count <- content(GET(paste0("https://api.vk.com/method/", "wall.get", 
                              "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                              "&owner_id=", as.character(id),
                              "&filter=", "owner",
                              "&count=", "1")), as = "parsed")$response[[1]]
  counter <- 1
  return(wall.get.posts.recursive(id, date, list()))
}

