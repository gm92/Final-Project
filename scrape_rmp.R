library(rvest)
library(httr)
library(XML)
library(jsonlite)

get_data <- function(school){
  final <- data.frame(NULL)
  
  #we take the first 80 pages for each school
  for (i in 1:80){
    url <- paste0("http://www.ratemyprofessors.com/find/professor/?department&institution=Duke%20University&page=",i,
                  "&query&queryoption=TEACHER&queryBy=schoolId&sid=",school,"&sortBy")

    data<-fromJSON(url)

    dept = NULL
    first_name = NULL
    last_name = NULL
    num_rating = NULL
    rating_class = NULL
    overall_rating = NULL
    
    #there are 20 teacher entries for each page 
    for(k in 1:20){
      dept[k]           = data$professors[[k]][[1]]
      first_name[k]     = data$professors[[k]][[4]]
      last_name[k]      = data$professors[[k]][[6]]
      num_rating[k]     = strtoi(data$professors[[k]][[8]])
      rating_class[k]   = data$professors[[k]][[9]]
      overall_rating[k] = as.numeric(data$professors[[k]][[12]])
    }
    
    #make data frame for page
    temp = data.frame(dept, first_name, last_name, num_rating, rating_class, overall_rating, stringsAsFactors = FALSE)
    #add these rows to total school data frame
    final <- rbind(final, temp)
  }
  #return school data frame 
  return (final)
}

#make data frames for each school
school_data <- lapply(list(Duke=1350, UNC=1232), function(x) get_data(x))
