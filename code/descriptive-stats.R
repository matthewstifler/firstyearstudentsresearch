require(httr)

#Full pipe to supply data about users to an existing df
students.ids.cities.df <- lapply(students.ids.cities.df$id, function(x){
  Sys.sleep(1)
  user.data <- content(GET(paste0("https://api.vk.com/method/", "users.get", 
                     "?access_token=", "24f3f52000d221fb9d47c9039134fb3623108c4cf67d24dae0b772702d1b9ab6750c220e3ff7989cd3a17", 
                     "&user_ids=", as.character(x),
                     "&fields=", "sex,bdate,country")), as = "parsed")$response
  message(paste0("Processing user ", x))
  return(data.frame(gender = ifelse(!is.null(user.data[[1]]$sex), user.data[[1]]$sex, NA),
                    dob = ifelse(!is.null(user.data[[1]]$bdate), as.Date(user.data[[1]]$bdate, format = "%d.%m.%Y"), NA),
                    country = ifelse(!is.null(user.data[[1]]$country), user.data[[1]]$country, NA)))
}) %>%
  pryr::rbind.fill %>%
  cbind(students.ids.cities.df, .)
