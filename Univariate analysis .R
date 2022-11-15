# Clean workspace
if(!is.null(dev.list())) dev.off() # for plots
rm(list=ls()) #for datasets

#Initialize dataset: can also be done with setwd (set working directory)
listings_D3_V2 
df<-listings_D3_V2

—————————————————-


#'* 1. Checking if all variables are considered correctly by R*
summary(df)
#here: check if all your variables are actually considered numeric
sapply(df, class) #either find it here
class(df$maximum_nights) #or do it individually
# in case you need to change them, this is how you do it
df$host_acceptance_rate <- as.numeric(df$host_acceptance_rate)


#'*(As we have imputed all NA's, the part on NA's has been removed, is available in drive)*

---------------------
  
#'*Now we go through every variable and make the plots*

library(ggplot2)
  
#'[1. Host_since]
  
#'*CATEGORICAL VARIABLE:*

ggplot(df, aes(x=reorder(host_since, host_since, function(x)-length(x)), group = host_since) ) +
  geom_bar(fill='steelblue', width = 0.5) +
  ylim(0, 2500) +
  ggtitle("host_since")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "host_since") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 2)
  
--------------------------------------------------------------
#'[2. Host_location]
  
#'*CATEGORICAL VARIABLE:*

ggplot(df, aes(x=reorder(host_location, host_location, function(x)-length(x)), group = host_location) ) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 17000) +
  ggtitle("host_location")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "host_location") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4)
--------------------------------------------------------------
#'[3. Host_response-time]
  
#'*CATEGORICAL VARIABLE:*

ggplot(df, aes(x=reorder(host_response_time, host_response_time, function(x)-length(x)), group = host_response_time) ) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 10000) +
  ggtitle("host_response_time")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "host_since") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4)

--------------------------------------------------------------
#'[4. Host_response-rate]

#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("host_response_rate")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("host_response_rate")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="host_response_rate")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$host_response_rate)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("host_response_rate SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="host_response_rate SQRT")

--------------------------------------------------------------
#'[5. Host_acceptance_rate]

#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("host_acceptance_rate")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("host_acceptance_rate")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="host_acceptance_rate")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$host_acceptance_rate)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("host_acceptance_rate SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="host_acceptance_rate SQRT")

--------------------------------------------------------------
#'[6. Host_is_superhost]

#'*CATEGORICAL VARIABLE:*
  
ggplot(df, aes(x=reorder(host_is_superhost, host_is_superhost, function(x)-length(x)), group = host_is_superhost) ) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 15000) +
  ggtitle("host_is_superhost")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "host_is_superhost") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4)


--------------------------------------------------------------
#'[7. Host_total_listings_count]



#'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$host_total_listings_count, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("host_total_listings_count")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() +
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter)


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("host_total_listings_count")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="host_total_listings_count")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$host_total_listings_count)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("host_total_listings_count SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="host_total_listings_count SQRT")



--------------------------------------------------------------
#'[8. Host_has_profile_pic]

#'*CATEGORICAL VARIABLE:*

ggplot(df, aes(x=reorder(host_has_profile_pic, host_has_profile_pic, function(x)-length(x)), 
               group = host_has_profile_pic) ) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 18000) +
  ggtitle("host_has_profile_pic")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "host_has_profile_pic") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4)


--------------------------------------------------------------
#'[9. Host_identity_verified]

#'*CATEGORICAL VARIABLE:*

ggplot(df, aes(x=reorder(host_identity_verified, host_identity_verified, function(x)-length(x)), 
               group = host_identity_verified) ) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 15000) +
  ggtitle("host_identity_verified")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "host_identity_verified") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4)

--------------------------------------------------------------
#'[10. neighbourhood_group_cleansed]

  #'*CATEGORICAL VARIABLE:*

ggplot(df, aes(x=reorder(neighbourhood_group_cleansed, neighbourhood_group_cleansed, function(x)-length(x)), 
               group = neighbourhood_group_cleansed) ) + 
  theme(text = element_text(size=14),
    axis.text.x = element_text(angle=90, hjust=1)) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 7000) +
  ggtitle("host_identity_verified")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "host_identity_verified") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4)

--------------------------------------------------------------
#'[11. latitude]

#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("latitude")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("latitude")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="latitude")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$latitude)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("latitude SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="latitude SQRT")


--------------------------------------------------------------
#'[12. longitude]

#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("longitude")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("longitude")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="longitude")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$longitude)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("longitude SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="longitude SQRT")

--------------------------------------------------------------
#'[13. Property type]

    #'*CATEGORICAL VARIABLE:*
  
ggplot(df, aes(x=reorder(property_type, property_type, function(x)-length(x)), 
               group = property_type) ) + theme(text = element_text(size=5),
    axis.text.x = element_text(angle=90, hjust=1)) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 15000) +
  ggtitle("property_type")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "property_type") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 1)



--------------------------------------------------------------
#'[14. Room type]

    #'*CATEGORICAL VARIABLE:*

  ggplot(df, aes(x=reorder(room_type, room_type, function(x)-length(x)), 
               group = room_type) ) + theme(text = element_text(size=14),
    axis.text.x = element_text(angle=90, hjust=1)) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 12000) +
  ggtitle("room_type")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "room_type") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3)    

--------------------------------------------------------------
#'[15. Accomodates]

  

#'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

  
iter <- c("accommodates")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) +
  xlim(0, 20)


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("accommodates")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="accommodates")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$accommodates)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("accommodates SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="accommodates SQRT")
  
  
--------------------------------------------------------------
#'[16. bedrooms]
  
#'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

  
iter <- c("bedrooms")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("bedrooms")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="bedrooms")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$bedrooms)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("bedrooms SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="bedrooms SQRT")
  
  
--------------------------------------------------------------
#'[17. beds]
  
  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("beds")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 



#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("beds")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="beds")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$bedrooms)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("beds SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="beds SQRT")

--------------------------------------------------------------
#'[18. price]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("price")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("price")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="price")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$price)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("price SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="price SQRT")

--------------------------------------------------------------
#'[19. minimum_nights]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("minimum_nights")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("minimum_nights")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="minimum_nights")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$minimum_nights)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("minimum_nights SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="minimum_nights SQRT")

--------------------------------------------------------------
#'[20. maximum_nights]


#'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("maximum_nights")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("maximum_nights")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="maximum_nights")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$maximum_nights)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("maximum_nights SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="maximum_nights SQRT")
  
  
--------------------------------------------------------------
#'[has_availability]

#'*CATEGORICAL VARIABLE:*

  ggplot(df, aes(x=reorder(has_availability, has_availability, function(x)-length(x)), 
               group = has_availability) ) + 
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 17000) +
  ggtitle("has_availability")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "room_type") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3) 


--------------------------------------------------------------
#'[availability_30]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("availability_30")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("availability_30")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="availability_30")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$availability_30)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("availability_30 SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="availability_30 SQRT")


--------------------------------------------------------------
#'[availability_365]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("availability_365")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("availability_365")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="availability_365")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$availability_365)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("availability_365 SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="availability_365 SQRT")


--------------------------------------------------------------
#'[number_of_reviews]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("number_of_reviews")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("number_of_reviews")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="number_of_reviews")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$number_of_reviews)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("number_of_reviews SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="number_of_reviews SQRT")

--------------------------------------------
#'[number_of_reviews_ltm]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("number_of_reviews_ltm")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("number_of_reviews_ltm")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="number_of_reviews_ltm")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$number_of_reviews_ltm)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("number_of_reviews_ltm SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="number_of_reviews_ltm SQRT")


--------------------------------------------
#'[number_of_reviews_l30d]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("number_of_reviews_l30d")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("number_of_reviews_l30d")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="number_of_reviews_l30d")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$number_of_reviews_l30d)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("number_of_reviews_l30d SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="number_of_reviews_l30d SQRT")


--------------------------------------------------------------
#'[first_review]

#'*CATEGORICAL VARIABLE:*

  ggplot(df, aes(x=reorder(first_review, first_review, function(x)-length(x)), 
               group = first_review) ) + 
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 17000) +
  ggtitle("first_review")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "first_review") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3) 

--------------------------------------------------------------
#'[last_review]

#'*CATEGORICAL VARIABLE:*

  ggplot(df, aes(x=reorder(last_review, last_review, function(x)-length(x)), 
               group = last_review) ) + 
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 17000) +
  ggtitle("last_review")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "last_review") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3) 





--------------------------------------------------------------
#'[31. review_scores_rating]

  
#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("review_scores_rating")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("review_scores_rating")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_rating")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$review_scores_rating)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("review_scores_rating SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_rating SQRT")


--------------------------------------------------------------
#'[32. review_scores_accuracy]

  
#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("review_scores_accuracy")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("review_scores_accuracy")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_accuracy")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$review_scores_accuracy)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("review_scores_accuracy SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_accuracy SQRT")

--------------------------------------------------------------
#'[33. review_scores_cleanliness]

#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("review_scores_cleanliness")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot*

df$new_column <- sqrt(df$review_scores_cleanliness)


iter <- c("new_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="new_column")

--------------------------------------------------------------
#'[34. review_scores_checkin]

#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("review_scores_checkin")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot normal*

iter <- c("review_scores_checkin")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_checkin")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$review_scores_checkin)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("review_scores_checkin SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_checkin SQRT")

--------------------------------------------------------------
#'[35. review_scores_communication]

#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("review_scores_communication")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot normal*

iter <- c("review_scores_communication")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_communication")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$review_scores_communication)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("review_scores_communication SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_communication SQRT")

--------------------------------------------------------------
#'[36. review_scores_location]

  
#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("review_scores_location")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot normal*

iter <- c("review_scores_location")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_location")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$review_scores_location)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("review_scores_location SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_location SQRT")

--------------------------------------------------------------
#'[37. review_scores_value]


  #'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("review_scores_value")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot normal*

iter <- c("review_scores_value")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_value")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$review_scores_value)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("review_scores_value SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="review_scores_value SQRT")





--------------------------------------------------------------
#'[license]

#'*CATEGORICAL VARIABLE:*

  ggplot(df, aes(x=reorder(license, license, function(x)-length(x)), 
               group = license) ) + 
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 17000) +
  ggtitle("license")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "license") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3) 



--------------------------------------------------------------
#'[instant_bookable]

#'*CATEGORICAL VARIABLE:*

  ggplot(df, aes(x=reorder(instant_bookable, instant_bookable, function(x)-length(x)), 
               group = instant_bookable) ) + 
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 17000) +
  ggtitle("instant_bookable")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "instant_bookable") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 3) 


--------------------------------------------
#'[calculated_host_listings_count]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("calculated_host_listings_count")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("calculated_host_listings_count")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$calculated_host_listings_count)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("calculated_host_listings_count SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count SQRT")



--------------------------------------------
#'[calculated_host_listings_count_entire_homes]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("calculated_host_listings_count_entire_homes")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("calculated_host_listings_count_entire_homes")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count_entire_homes")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$calculated_host_listings_count_entire_homes)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("calculated_host_listings_count_entire_homes SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count_entire_homes SQRT")



--------------------------------------------
#'[calculated_host_listings_count_private_rooms]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("calculated_host_listings_count_private_rooms")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("calculated_host_listings_count_private_rooms")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count_private_rooms")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$calculated_host_listings_count_entire_homes)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("calculated_host_listings_count_private_rooms SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count_private_rooms SQRT")


--------------------------------------------
#'[calculated_host_listings_count_shared_rooms]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("calculated_host_listings_count_shared_rooms")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("calculated_host_listings_count_shared_rooms")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count_shared_rooms")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$calculated_host_listings_count_shared_rooms)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("calculated_host_listings_count_shared_rooms SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="calculated_host_listings_count_shared_rooms SQRT")



--------------------------------------------------------------
#'[36. reviews_per_month]

  
#'*NUMERICAL CONTINOUS: Density Plot*

iter <- c("reviews_per_month")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_density() +
  ggtitle(paste(iter," density function"))+
 theme(plot.title = element_text(hjust = 0.5))+
  labs(x="percentage")

#'*ALL NUMERICAL VARIABLES: Make a boxplot normal*

iter <- c("reviews_per_month")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="reviews_per_month")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$reviews_per_month)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("reviews_per_month SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="reviews_per_month SQRT")




--------------------------------------------
#'[phone_verification]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("phone_verification")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("phone_verification")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="phone_verification")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$phone_verification)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("phone_verification SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="phone_verification SQRT")



--------------------------------------------
#'[email_verification]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("email_verification")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("email_verification")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="email_verification")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$email_verification)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("email_verification SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="email_verification SQRT")



--------------------------------------------
#'[work_email_verification]

  #'*NUMERICAL DISCRETE VARIABLES: Histogram (NA's ignored)*
#table(df$accommodates, useNA = "ifany") #see how many <NA> we have and how many

iter <- c("work_email_verification")
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))

ggplot(df, aes(x = column)) + geom_histogram() + geom_bar(width=1)+
  ggtitle(paste(iter,"histogram"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=iter) 


#'*ALL NUMERICAL VARIABLES: Make a boxplot*

iter <- c("work_email_verification")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste(iter," boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="work_email_verification")

#'*ALL NUMERICAL VARIABLES: Make a boxplot with the square root if it looks better*

df$temporary_sqrt_column <- sqrt(df$work_email_verification)

iter <- c("temporary_sqrt_column")  #Pau's automatic version
column <- df[iter]
column <- suppressWarnings(as.numeric(unlist(column)))
ggplot(df, aes(x = column)) + geom_boxplot() +
  ggtitle(paste("work_email_verification SQRT boxplot function"))+
  theme(plot.title = element_text(hjust = 0.5))+
labs(x="work_email_verification SQRT")




--------------------------------------------
#'[type_bath]

ggplot(df, aes(x=reorder(type_bath, type_bath, function(x)-length(x)), 
               group = type_bath) ) + theme(text = element_text(size=5),
    axis.text.x = element_text(angle=90, hjust=1)) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 15000) +
  ggtitle("type_bath")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "type_bath") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 1)




--------------------------------------------
  #for all the amenities: just add whatever amenity you would like to see here
#'[wifi]

ggplot(df, aes(x=reorder(wifi, wifi, function(x)-length(x)), 
               group = wifi) ) + theme(text = element_text(size=5),
    axis.text.x = element_text(angle=90, hjust=1)) +
  geom_bar(fill='steelblue', width = 0.7) +
  ylim(0, 15000) +
  ggtitle("wifi")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "count", x = "wifi") +
  geom_text(aes(label = ..count..), stat= "count",  colour = "black", position = position_dodge(width = 0.5),
            vjust = -0.5, size = 1)

hairdryer, aircon, heating and so on

