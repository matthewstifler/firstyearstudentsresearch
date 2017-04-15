require(httr)

#---------Loading all needed posts----------

july.first <- 1467320400

#I need, for each person: get 100 posts from the wall; check the date of the last one;
#if date > july.first, then load 100 more, if date < july.first, then compare all posts of the last batch dates to july.first
#get the ones that are TRUE for date > july.first
#NOTE: nice chance to utilize recursion

#Testing

wall.get.posts.recursive <- function(id, date, data) {
  data[((counter - 1)*100 + 1):(counter*100)] <- content(GET(paste0("https://api.vk.com/method/", "wall.get", #get a batch and store it in according slots
                     "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                     "&owner_id=", id,
                     "&filter=", "owner",
                     "&offset=", ((counter - 1) * 100),
                     "&count=", "100")), as = "parsed")$response[-1] #no 'count' field
  
  #if pinned post is 
  if (counter == 1 & !is.null(data[[1]]$is_pinned)) {
    if (data[[1]]$date < date) {
      data <- data[-1]
    }
  }
  
  if (any(data.date < date) || counter == counter)
    return()
  else
    function
}

wall.get.posts.since.date <- function(id, date) { #returns a list of all posts till specified date
  count <- content(GET(paste0("https://api.vk.com/method/", "wall.get", 
                              "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                              "&owner_id=", id,
                              "&filter=", "owner",
                              "&count=", "1")), as = "parsed")$response[[1]]
  counter <- 1
  
}

posts.since.july