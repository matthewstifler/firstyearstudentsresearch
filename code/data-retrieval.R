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

students.df = data.frame(id = unlist(members.list$users)) #to store all we know

#getting data on members to learn origin
members.info = lapply(member.list$users, FUN = function(x) users.get(vk.get_data, as.character(x), fields = c("city,connections,schools")))

#extracting data from the city field
students.df$city = lapply(members.info, function(x) x[[1]]$city) %>% unlist %>% as.factor
#many might change it to 2 after moving! thus:

#extracting data about school's city
for (i in 1:nrow(students.df)){
  tryCatch({city.school[i] = tryCatch(members.info[[i]][[1]]$schools[[1]]$city, error = function(err) NA)}, error = function(err) NA)
}

#works for now

students.df$city.school = city.school

#Dasha solution, mihgt replace all of the upper stuff
tmp = lapply(members.info, as.data.frame) #doesn't work yet
tmp = plyr::rbind.fill(tmp)

#subset df by students who changed their city to saint petersburg
subset.v = students.df$city.school != students.df$city
subset.v[is.na(subset.v)] = FALSE
students.df[subset.v & students.df$city == 2,]
