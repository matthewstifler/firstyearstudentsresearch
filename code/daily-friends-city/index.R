require(devtools)
require(assertthat)

community.types <- c("law", "econ", "orient", "soc", "manag", "pol", "log", "gmu", "phil")
## no history
community.sources <- c("hseius2016", "economhse16", "as2016hse", "hsesociology2016",
             "hse_spb_management_2016_2020", "clubpolitologi2016", "logistician2016",
             "hse_spb_2016", "hse_spb_philology")


source_gist("ac8122dff994da6d9287") #https://gist.github.com/mekkatya/ac8122dff994da6d9287

groups.getMembers.plus <- function(vk.get_data, group_id, offset=0, count=20) {
  print("Waiting...")
  Sys.sleep(1)
  method="groups.getMembers"
  assert_that(!is.null(group_id))
  assert_that(length(group_id)==1)
  return(vk.get_data(method=method, group_id=group_id, offset=offset, count=count))
}

friends.get <-function(vk.get_data, user_id, fields = "") {
  method = "friends.get"
  return(vk.get_data(method = method, user_id = user_id, fields = fields))
}

vk.get_data <- get.vk.connector(code = "cd9b1006da9c0e46a6")

#1.
#setNames gives us named list, "[[2]]" notation makes sure only the ids are returned
community.members <- sapply(setNames(community.sources, community.types), function(x) groups.getMembers.plus(vk.get_data, x, 0, 1000)[[2]])

#2.
community.members.cities <- lapply(community.members, function(x) {
  lapply(users.get(vk.get_data, create_user_ids(x), "city"), function(x) x$city)
})

#3.
community.members.friends.cities <- lapply(community.members, function(x) {
  friends.raw <- lapply(x, function(y) friends.get(vk.get_data, as.character(y), "city"))
  lapply(friends.raw, function(z) {
    subset <- sapply(z, function(a) is.null(a$deactivated))
    return(z[subset])
  })
})

#Roadmap
#1. Get all sample IDs
#2. Get all sample IDs' cities
#3. Get all sample IDs' friends' IDs & their cities
#4. Create an id-special df (id, city, type: from sample -> type / out of sample)
#5. Create a friendship matrix  
#6. Divide code as code required for daily running / other
#7. Divide daily running code into actual running script and scripts to source() in the main one

#Tasks
#1. Write friends.get function

#Optional goals
#1.Rewrite VK functions so that they don't do two requests when there is need of a count (result.count)