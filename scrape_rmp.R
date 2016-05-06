library(rvest)
library(RSelenium)
library(httr)
library(XML)

final = data.frame(NULL)
for ( i in 1:100){
  url <- paste0("http://www.ratemyprofessors.com/find/professor/?department&institution=Duke%20University&page=",i,
                "&query&queryoption=TEACHER&queryBy=schoolId&sid=1350&sortBy")
  
  data<-fromJSON(url)
  
  dept = NULL
  first_name = NULL
  last_name = NULL
  num_rating = NULL
  rating_class = NULL
  rating = NULL
  
  for(k in 1:20){
    dept[k]         = data$professors[[k]][[1]]
    first_name[k]   = data$professors[[k]][[4]]
    last_name[k]    = data$professors[[k]][[6]]
    num_rating[k]   = data$professors[[k]][[8]]
    rating_class[k] = data$professors[[k]][[9]]
    rating[k]       = data$professors[[k]][[12]]
  }
  temp = data.frame(dept, first_name, last_name, num_rating, rating_class, rating, stringsAsFactors = FALSE)
  final = rbind(final, temp)
}