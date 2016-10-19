require(assertthat)
require(devtools)
require(dplyr)

#vk methods loading
source_gist("ac8122dff994da6d9287") #https://gist.github.com/mekkatya/ac8122dff994da6d9287

#updated groups.getMembers method
groups.getMembers.plus = function(vk.get_data, group_id, offset=0, count=20) {
  print("Waiting...")
  Sys.sleep(1)
  method="groups.getMembers"
  assert_that(!is.null(group_id))
  assert_that(length(group_id)==1)
  return(vk.get_data(method=method, group_id=group_id, offset=offset, count=count))
}

#http://wardoctor.nosoc.io/public/paulokopny/vkauth/karepin.html
vk.get_data <- get.vk.connector(code = "f472f40818b1b12224")


#DL members of 1st year students' group HSE Sociology
members.list = groups.getMembers.plus(vk.get_data, "hsesociology2016", 0, 1000)

#getting data on origin by school city
members.info = lapply(member.list$users, FUN = function(x) users.get(vk.get_data, as.character(x), fields = c("city,connections,schools")))

#subsetting

