census <- read.csv('us_census.csv')
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'),]

N <- 50

chosen_counties <- sample(1:nrow(contiguous), N)

centers <- matrix(0, nrow=N, ncol=2)

centers[,1] = contiguous$latitude[chosen_counties]
centers[,2] = contiguous$longitude[chosen_counties]

centers_df = contiguous[chosen_counties, 3:4]

#print(contiguous[1,])
#print(centers[1,])

dist_sq <- function(county, center){
  deltax <- county[1,'latitude'] - center[1]
  deltay <- county[1,'longitude'] - center[2]
  deltax^2 + deltay^2
}

belongs_to <- rep(0, nrow(contiguous))

number_of_reps <- 3

while(number_of_reps >= 1){
  
  for(county in 1:nrow(contiguous)){
    closest_center <- 1
    closest_distance <- dist_sq(contiguous[county,], centers[1,])
    
    for(cluster in 2:N){
      d <- dist_sq(contiguous[county,], centers[cluster,])
      if(d < closest_distance){
        closest_distance <- d
        closest_center <- cluster
      }
    }
    belongs_to[county] <- closest_center
  }
  
  
  plot(contiguous$longitude, contiguous$latitude, type='p', col=belongs_to)
  
  print(centers)
  
  for(num1 in 1:N){
    clust_of_interest <- contiguous[belongs_to==num1,]
    total_pop <- sum(clust_of_interest$population)
    
    new_latitude <- sum(clust_of_interest$latitude * clust_of_interest$population) / total_pop
    new_longitude <- sum(clust_of_interest$latitude * clust_of_interest$population) / total_pop
    
    centers[num1,1] <- new_latitude
    centers[num1,2] <- new_longitude
  }
  number_of_reps <- number_of_reps - 1
}
