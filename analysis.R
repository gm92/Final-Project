#data cleanup
#look at sort(unique(school_data$dept)) for Duke and UNC to determine if some department categories should be combined
school_data$Duke[school_data$Duke$dept == "Art History", ]$dept <- "Art & Art History"
school_data$Duke[school_data$Duke$dept == "Art  Art History", ]$dept <- "Art & Art History"
school_data$Duke[school_data$Duke$dept == "Physical Ed", ]$dept <- "Physical Education"
school_data$Duke[school_data$Duke$dept == "Athletics", ]$dept <- "Physical Education"
school_data$Duke[school_data$Duke$dept == "Women's Studies", ]$dept <- "Womens Studies"

school_data$UNC[school_data$UNC$dept == "Environmental Science", ]$dept <- "Environmental Sci & Eng"
school_data$UNC[school_data$UNC$dept == "Biological Sciences", ]$dept <- "Biology"
school_data$UNC[school_data$UNC$dept == "Physical Ed", ]$dept <- "Physical Education"
school_data$UNC[school_data$UNC$dept == "Exercise  Sport Science", ]$dept <- "Exercise & Sport Science"
school_data$UNC[school_data$UNC$dept == "Art", ]$dept <- "Art & Art History"
school_data$UNC[school_data$UNC$dept == "Art History", ]$dept <- "Art & Art History"
school_data$UNC[school_data$UNC$dept == "Health Policy & Management", ]$dept <- "Health Administration"
school_data$UNC[school_data$UNC$dept == "Piano", ]$dept <- "Music"


#average rating per department
#multiply overall rating by number of ratings to get total rating, then divide by total number of ratings
#the trend is
duke_dept_avg <- lapply(unique(school_data$Duke$dept), 
                      function(x) 
                        sum(school_data$Duke[school_data$Duke$dept == x, ]$overall_rating * 
                        school_data$Duke[school_data$Duke$dept == x, ]$num_rating)/
                        sum(school_data$Duke[school_data$Duke$dept == x, ]$num_rating))
names(duke_dept_avg) <- unique(school_data$Duke$dept)
duke_dept_avg <- sort(unlist(duke_dept_avg),decreasing=TRUE)
duke_dept_avg

unc_dept_avg <- lapply(unique(school_data$UNC$dept), 
                     function(x) 
                       sum(school_data$UNC[school_data$UNC$dept == x, ]$overall_rating * 
                             school_data$UNC[school_data$UNC$dept == x, ]$num_rating)/
                       sum(school_data$UNC[school_data$UNC$dept == x, ]$num_rating))
names(unc_dept_avg) <- unique(school_data$UNC$dept)
unc_dept_avg<-sort(unlist(unc_dept_avg),decreasing=TRUE)
unc_dept_avg

#number of ratings per department
#the trend is  
duke_num_ratings <- lapply(unique(school_data$Duke$dept), 
                      function(x) 
                        sum(school_data$Duke[school_data$Duke$dept == x, ]$num_rating))
names(duke_num_ratings) <- unique(school_data$Duke$dept)
duke_num_ratings <- sort(unlist(duke_num_ratings),decreasing=TRUE)
duke_num_ratings

unc_num_ratings <- lapply(unique(school_data$UNC$dept), 
                           function(x) 
                             sum(school_data$UNC[school_data$UNC$dept == x, ]$num_rating))
names(unc_num_ratings) <- unique(school_data$UNC$dept)
unc_num_ratings <- sort(unlist(unc_num_ratings),decreasing=TRUE)
unc_num_ratings

#Find number of ratings per rating class
#the trend is that students tend to give more ratings to the "better" professors
duke_class_num <- lapply(unique(school_data$Duke$rating_class), 
                           function(x) 
                             sum(school_data$Duke[school_data$Duke$rating_class == x, ]$num_rating))
names(duke_class_num) <- unique(school_data$Duke$rating_class)
duke_class_num <- sort(unlist(duke_class_num),decreasing=TRUE)
duke_class_num

unc_class_num <- lapply(unique(school_data$UNC$rating_class), 
                         function(x) 
                           sum(school_data$UNC[school_data$UNC$rating_class == x, ]$num_rating))
names(unc_class_num) <- unique(school_data$UNC$rating_class)
unc_class_num <- sort(unlist(unc_class_num),decreasing=TRUE)
unc_class_num

#out of the professors with at least 30 ratings, sort by best rated
#looks like best rated profs at Duke are STEM classes and best rated profs at UNC are history/humanities
valid_duke_profs <- school_data$Duke[school_data$Duke$num_rating >= 30, ]
valid_unc_profs <- school_data$UNC[school_data$UNC$num_rating >= 30, ]
valid_duke_profs[order(-valid_duke_profs[,6]), ][1:10,]
valid_unc_profs[order(-valid_unc_profs[,6]), ][1:10,]

#what is the most common first name of professors lol
#top 10 are basically the same for each school
dukenames<-count(school_data$Duke, 'first_name')
dukenames[order(-dukenames[,2]), ][1:10,]
uncnames<-count(school_data$UNC, 'first_name')
uncnames[order(-uncnames[,2]), ][1:10,]

#make some comparisons between Duke and UNC with tests or just comparing the above

t.test(school_data$Duke$overall_rating, school_data$UNC$overall_rating)
t.test(school_data$Duke$num_rating, school_data$UNC$num_rating) #hey this is significant!
