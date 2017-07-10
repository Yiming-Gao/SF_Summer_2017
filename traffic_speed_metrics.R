setwd("/san-data/usecase/magnet_g/misc/PCA_DATA_VIS/") ## set working directory

library(plyr)
library(zoo)
library(tidyr) # %>% 
library(leaflet)
library(colorRamps) #legend of leaflet
library(RcppRoll) #roll_meanr
library(grDevices) #obtain color Palettes (rainbow())
library(plotly)
library(readr)
library(data.table)
library(dplyr)
library(RColorBrewer)
library(ggmap)
library(ggthemes)
library(caret)

# new predictors
# estimated traffic density relative to all roads
# estimated traffic density relative to the roads average traffic density
# estimated average speed
# estimated speed limit

latitude_min = 39.9524 # 39.80000
latitude_max = 40.1190
longitude_min = -83.1432
longitude_max = -82.8884
#defines the borders of the map

based_off_start = FALSE 
#true if you want to base off of starting location, false if off ending location
both_start_end = FALSE


start_time = 19.00  
end_time = 22.00
#sets the time that will be analyzed
#times should be entered in military format (24 hour time)
#hour + fraction represented by minutes/60


test_data <- fread("Timed_big_data.csv", nrows = 5000000) %>%
  filter(latitude > -99999 & longitude > -9999) %>%
  dplyr::select(
    trip_number,
    timestmp_local,
    latitude,
    longitude,
    stop_ind,
    stop_grp_cnt,#Indicates if this second is the first second in a unique stop
    latG,
    lonG,
    speed,
    ang_speed_gyro, #the change in orientation of the car in that second, measured in degrees/second,angular speed for gyroscope
    lon_delta, #Jerk (derivative of acceleration) in the longitudinal direction at this second
    inc_mileage #How far the car traveled in this second.
  ) %>%
  mutate(ratio = latG * ang_speed_gyro) %>%
  na.omit() %>%
  filter(abs(ratio) < 100) %>%
  mutate(avg_latG = c(rollapply(latG, width = 10, mean, fill = NA))) %>%
  mutate(avg_latG_mag = c(rollapply(abs(latG), width = 5, mean, fill = NA)))
  
datas1 = separate(test_data,
                    timestmp_local,
                    c('Hour_editing_needed', 'Minute', 'Second'),
                    sep = ':',
                    fill = 'right',
                    remove = FALSE) %>%
  mutate(Hour = as.numeric(substr(Hour_editing_needed,
                                  nchar(Hour_editing_needed)-1 ,
                                  nchar(Hour_editing_needed))),
         Minute = as.numeric(Minute),
         Second = as.numeric(Second),
         Date = as.Date(substr(as.character(timestmp_local), 1, 9)))
#modifies the timestamp from one variable to a collection of variables: Hour, Minute, Second, Date


#Turning algorithm
datas1$algorithm <- NA
datas1$algorithm[abs(datas1$ang_speed_gyro) > 10 & abs(datas1$latG) > .07] <- "Normal Turn"
datas1$algorithm[datas1$latG >= -.07 & datas1$latG < .07] <- "Normal Driving"
datas1$algorithm[abs(datas1$ang_speed_gyro) < 10 & abs(datas1$latG) > .07 & abs(datas1$avg_latG) < .055] <- "Lane Change"
datas1$algorithm[abs(datas1$ang_speed_gyro) < 10 & abs(datas1$latG) > .07 & abs(datas1$avg_latG) > .055] <- "Curve in the road"


temp <- datas1 %>% dplyr::select(longitude, latitude, trip_number, speed, stop_ind) %>% 
  transmute(longitude = round(.$longitude, 4), latitude = round(.$latitude, 4), trip_number = trip_number, 
            speed = speed, stop_ind = stop_ind) %>%
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude > longitude_min)
#stores the location, trip number, speed, turning information into the temp dataset


trip_ends <- temp[!duplicated(temp$trip_number, fromLast =TRUE),] %>% 
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude > longitude_min)

trip_starts <- temp[!duplicated(temp$trip_number),] %>% 
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude> longitude_min)
# removes any duplications and checks to see if the trip starts and ends within the location parameters inputed earlier


location_start <- subset(datas1, trip_number %in% trip_starts$trip_number)
location_end <- subset(datas1, trip_number %in% trip_ends$trip_number)
#filters the original data down to the desired location parameters inputed earlier

trip_both <- bind_rows(trip_starts, trip_ends) %>%
  .[.$trip_number %in% names(table(.$trip_number))[table(.$trip_number) > 1], ] %>%
  .[!duplicated(.$trip_number), ]
#combines start and end points of eligible trips

location_both <-subset(datas1, trip_number %in% trip_both$trip_number)
#filters the original data down to those which have eligible trips based on location



if(both_start_end){
  location <-location_both
}else{
  if(based_off_start){
    location <-location_start
  }
  else{
    location <-location_end
  } 
}
#stores eligible trips based on location into a new variable location

location$latitude <- round(location$latitude, digits = 4)
location$longitude <- round(location$longitude, digits = 4)
#rounds the locations to three decimal places

location <- location %>% distinct(trip_number, longitude, latitude)
location <- location %>% dplyr::select(longitude, latitude, trip_number) %>%
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude> longitude_min)
# filter out duplicates and keep only the location and trip number data
# location contains points that each trip_number passes


### Very fancy plot for visualize traffic density
leaflet(location) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, radius = 3, color = "red", 
                   clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-';  
    if (childCount > 20) {  
      c += 'large';  
    } else if (childCount > 5) {  
      c += 'medium';  
    } else { 
      c += 'small';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }")))%>%
  addLegend("bottomright", colors = "red", labels = "Traffic Density") 


# a data frame
points = location %>% distinct(longitude, latitude) 

################# add counts for each unique point (i. traffic density relative to all roads) ######################

traffic_den = location %>%
  group_by(longitude, latitude) %>%
  summarise(traffic_den = n())


# merge two datasets
points = left_join(points, traffic_den, by = c("longitude" = "longitude", "latitude" = "latitude")) 
rm(traffic_den)

# visualize traffic density in Columbus
pal <- colorNumeric(
  palette = "Reds",
  domain = points$traffic_den)

leaflet(points) %>% addTiles() %>%
  addCircles(lng = ~points$longitude, lat = ~points$latitude, weight = 1,  color = ~pal(traffic_den),
             radius = 7, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values = sort(unique(points$traffic_den)), title = "Traffic Density", opacity = 1)

####################################################################################################################



######################### add average speed for each unique point (iii. average speed) #############################
# exclude stopping point by looking at stop indicator, makes more sense

avg_speed = temp %>%
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude> longitude_min) %>%
  group_by(longitude, latitude) %>%
  summarise(avg_speed = mean(speed[speed != 0]))

# merge two datasets
points = left_join(points, avg_speed, by = c("longitude" = "longitude", "latitude" = "latitude")) 
rm(avg_speed)

# temp1 is the data set filtered from temp by those variable ranges
temp1 = temp %>%  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude > longitude_min)

temp1 = left_join(temp1, points, by = c("longitude" = "longitude", "latitude" = "latitude")) 
  # left_join() function is much faster than for loop


# visualize the speed data
# Create a continuous palette function
pal <- colorNumeric(
  palette = "Reds",
  domain = points$avg_speed)

leaflet(points) %>% addTiles() %>%
  addCircles(lng = ~points$longitude, lat = ~points$latitude, weight = 1,  color = ~pal(avg_speed),
             radius = sqrt(points$avg_speed) * 5, fillOpacity = .8) %>%
  addLegend("bottomright", pal = pal, values = points$avg_speed, title = "Average Speed", opacity = 1)

#####################################################################################################################



################################# Validate traffic & speed variables ################################################
points$traffic = character(nrow(points))
points[which(points$avg_speed == 0), ]$traffic = "Stopped"
points[which(points$avg_speed > 0 & points$avg_speed <= 30), ]$traffic = "Slow"
points[which(points$avg_speed > 30 & points$avg_speed <= 45), ]$traffic = "Medium"
points[which(points$avg_speed > 45 & points$avg_speed <= 70), ]$traffic = "Fast"
points[which(points$avg_speed > 70), ]$traffic = "Very fast"

# Visualize traffic situations by category: http://morpc.ms2soft.com/tcds/tsearch.asp?loc=Morpc&mod=
factpal = colorFactor(topo.colors(5), points$traffic)

leaflet(points) %>% addTiles() %>%
  addCircles(lng = ~points$longitude, lat = ~points$latitude, weight = 1,  color = ~factpal(traffic),
             radius = 30, fillOpacity = .8) %>%
  addLegend("bottomright", pal = factpal, values = points$traffic, title = "Traffic", opacity = 1)





############## The following code are based on each trip #############################

temp2 = datas1 %>% dplyr::select(longitude, latitude, trip_number, speed, Hour_editing_needed, Minute, Second, algorithm) %>% 
  transmute(longitude = round(.$longitude, 4), latitude = round(.$latitude, 4), trip_number = trip_number, 
            speed = speed, Time = as.numeric(substr(.$Hour_editing_needed, 11, 12)) + Minute / 60 + Second / 3600, turning = algorithm) %>%  
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude > longitude_min)

temp2 = left_join(temp2, points, by = c("longitude" = "longitude", "latitude" = "latitude")) 

# calculate relative speed for each row
temp2$relativity = ifelse(temp2$speed != 0, (temp2$speed - temp2$avg_speed) / temp2$avg_speed, NA)


##################################### (iv.) Speed Limit ###################################

# Method 1: add speed limit only based on temporary road type to temp2
temp2$speed_lim1 = ifelse(temp2$road_type == "Freeway", 70, ifelse(temp2$road_type == "Business District", 45, 30))
points$speed_lim1 = ifelse(points$road_type == "Freeway", 70, ifelse(points$road_type == "Business District", 45, 30))


# Method 2: based on weighted road type & avg_speed
# reference: http://www.speed-limits.com/ohio.htm
quan_speed = temp %>%
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude> longitude_min) %>%
  group_by(longitude, latitude) %>%
  summarise(quan_speed = quantile(speed, 0.5))

# merge two datasets
points = left_join(points, quan_speed, by = c("longitude" = "longitude", "latitude" = "latitude")) 
rm(quan_speed)

points$speed_lim2 = ifelse(points$speed_lim1 * 0.5 + points$quan_speed * 0.5 <= (20 + 35) / 2, 20,
                           ifelse(points$speed_lim1 * 0.5 + points$quan_speed * 0.5 <= (35 + 55) / 2, 35, 
                                  ifelse(points$speed_lim1 * 0.5 + points$quan_speed * 0.5 <= (55 + 65) / 2, 55,
                                         ifelse(points$speed_lim1 * 0.5 + points$quan_speed * 0.5 <= (65 + 70) / 2, 65, 70))))

temp2 = left_join(temp2, points[, c(1, 2, 6)], by = c("longitude" = "longitude", "latitude" = "latitude"))
temp2$speed_lim2 = ifelse(temp2$speed_lim1 * 0.5 + temp2$quan_speed * 0.5 <= (20 + 35) / 2, 20,
                          ifelse(temp2$speed_lim1 * 0.5 + temp2$quan_speed * 0.5 <= (35 + 55) / 2, 35, 
                                 ifelse(temp2$speed_lim1 * 0.5 + temp2$quan_speed * 0.5 <= (55 + 65) / 2, 55,
                                        ifelse(temp2$speed_lim1 * 0.5 + temp2$quan_speed * 0.5 <= (65 + 70) / 2, 65, 70))))

# visualize speed_lim2 in Columbus City
factpal = colorFactor(topo.colors(5), points$speed_lim2)

leaflet(points) %>% addTiles() %>%
  addCircles(lng = ~points$longitude, lat = ~points$latitude, weight = 1,  color = ~factpal(speed_lim2),
             radius = 8, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = factpal, values = sort(unique(points$speed_lim2)), title = "Speed Limit", opacity = 1)


