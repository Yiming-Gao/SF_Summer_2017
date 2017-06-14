### 

# plot one trip
plot_data = datas2[datas2$trip_number == "00A5B2C4E9254172BE08387115D195FA00", ]
plot_data$actual_speed_lim = c(rep(35, nrow(plot_data) - 675 + 1))


leaflet(plot_data) %>% addTiles() %>%
  addCircles(lng = ~plot_data$longitude, lat = ~plot_data$latitude, 
             weight = 1,  color = "dodgerblue",
             radius = 7, fillOpacity = 1)


# visualize speed_lim2 in Columbus
factpal = colorFactor(topo.colors(5), plot_data$speed_lim2)

leaflet(plot_data) %>% addTiles() %>%
  addCircles(lng = ~plot_data$longitude, lat = ~plot_data$latitude, weight = 1,  color = ~factpal(speed_lim2),
             radius = 10, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = factpal, values = sort(unique(plot_data$speed_lim2)), title = "Speed Limit", opacity = 1)


##################################### sample size  ###########################
# set some intolerance
accuracy = function(actual, predicted) {
  mean((predicted <= actual + 5) & (predicted >=  actual - 5))
}

## sample 1: traffic density >= 15
sample1 = points[points$traffic_den >= 15, ] %>%
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude > longitude_min)

actual_speed_lim1 = c(rep(45, 2), rep(20, nrow(sample1) - 2))

acc1 = accuracy(actual_speed_lim1, sample1$speed_lim2) # 90.5%


## sample 2: 10 <= traffic density <= 15
sample2 = points[points$traffic_den < 15 & points$traffic_den >= 10, ] %>%
  filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude > longitude_min)

sample2 = sample2[order(sample2$latitude),]

actual_speed_lim2 = c(rep(20, 24), rep(45, 10), rep(25, 11), rep(45, 10), 70, rep(45, 7), rep(70, 2), 50)

acc2 = accuracy(actual_speed_lim2, sample2$speed_lim2) # 59.1 %


### Assume we at least need traffic density = 15
# Run the top few lines in plot_function.R first
options(scipen = 999)
temp_acc = round(temp2 %>%
                   group_by(traffic_den) %>% 
                   summarise(count = n(), prop = n()/ nrow(points)), 6)

# There are 
sum(temp_acc[temp_acc$traffic_den >= 15, ]$count) / sum(temp_acc$count)
# 0.25% satisfies
# Tried several times, always around 0.25%

# One car (trip) has
mean((temp2 %>% group_by(trip_number) %>% summarise(count = n()))$count)
# 394 obs in temp2 on average


factpal = colorFactor(colors()[c(257, 91, 507 , 452, 129)], points[points$traffic_den >= 5, ]$speed_lim2)
# factpal = colorFactor(topo.colors(3), points[points$traffic_den >= 5, ]$road_type)

leaflet(points[points$traffic_den >= 5, ]) %>% addTiles() %>%
  addCircles(lng = ~points[points$traffic_den >= 5, ]$longitude, lat = ~points[points$traffic_den >= 5, ]$latitude, weight = 1,  color = ~factpal(speed_lim2),
             radius = ~points[points$traffic_den >= 5, ]$speed_lim2 * 2, fillOpacity = 1, group = "Speed Limit") %>%
  addLegend("bottomright", pal = factpal, values = points[points$traffic_den >= 5, ]$speed_lim2, title = "Speed Limit", opacity = 1)
# This plot is very similar to actual speed limit plot


############################# Validate with DIS Contextual Data ##############################
# 
library(clue)
setwd("/san-data/usecase/magnet_g/misc/PCA_DATA_VIS/Yiming/Accuracy/")
temp_all = fread("Timed_big_data.csv") %>%
  filter(latitude > -99999 & longitude > -9999) %>%
  dplyr::select(
    trip_number,
    latitude,
    longitude,
    stop_ind,
    latG,
    speed,
    ang_speed_gyro #the change in orientation of the car in that second, measured in degrees/second,angular speed for gyroscope
  ) %>%
  mutate(ratio = latG * ang_speed_gyro,
         longitude = round(.$longitude, 4),
         latitude = round(.$latitude, 4)) %>%
  na.omit() %>%
  filter(abs(ratio) < 100) 


one_trip_acc <- function(trip_number) {
  
  temp_data = datas1[datas1$trip_number == trip_number, ] %>% 
    dplyr::select(longitude, latitude, trip_number, speed, stop_ind) %>% 
    transmute(longitude = round(.$longitude, 4), latitude = round(.$latitude, 4), trip_number = trip_number, 
              speed = speed, stop_ind = stop_ind) %>% distinct(longitude, latitude) 
  # add traffic density
  temp_data = left_join(temp_data, left_join(temp_data, temp_all %>% 
                                               group_by(longitude, latitude) %>%
                                               summarise(traffic_den = n()), by = c("longitude" = "longitude", "latitude" = "latitude")), 
                        by = c("longitude" = "longitude", "latitude" = "latitude"))
  # add average speed
  temp_data = left_join(temp_data, temp_all[temp_all$longitude %in% temp_data$longitude & temp_all$latitude %in% temp_data$latitude, ] %>% 
                          group_by(longitude, latitude) %>%summarise(avg_speed = mean(speed[speed != 0])),
                        by = c("longitude" = "longitude", "latitude" = "latitude"))
  # add quantile speed
  temp_data = left_join(temp_data, temp_all[temp_all$longitude %in% temp_data$longitude & temp_all$latitude %in% temp_data$latitude, ] %>% 
                          group_by(longitude, latitude) %>% summarise(quan_speed = quantile(speed, 0.5)),
                        by = c("longitude" = "longitude", "latitude" = "latitude"))
  # add predicted road type
  temp_data$road_type = ifelse(cl_predict(kmeans, temp_data[, c(3, 4)]) == 1, "Business District", 
                               ifelse(cl_predict(kmeans, temp_data[, c(3, 4)]) == 2, "Residential Road", "Freeway"))
  
  # add speed limit 1
  temp_data$speed_lim1 = ifelse(temp_data$road_type == "Freeway", 70, ifelse(temp_data$road_type == "Business District", 45, 30))
  
  # add speed limit 2
  temp_data$speed_lim2 = ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (20 + 35) / 2, 20,
                            ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (35 + 55) / 2, 35, 
                                   ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (55 + 65) / 2, 55,
                                          ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (65 + 70) / 2, 65, 70))))
  # actual speed
  actual_data = fread(paste(trip_number, "csv", sep = ".")) %>% 
    dplyr::select(longitude, latitude, trip_numbe, route_type, speed_cat) %>% 
    mutate(longitude = round(.$longitude, 4), latitude = round(.$latitude, 4)) %>%
    distinct(longitude, latitude, speed_cat) 
  
  # add actual speed information
  temp_data = left_join(temp_data, actual_data[actual_data$longitude %in% temp_data$longitude & actual_data$latitude %in% temp_data$latitude, c("longitude", "latitude", "speed_cat")], 
                                     by = c("longitude" = "longitude", "latitude" = "latitude"))
  
  # calculate accuracy for this trip
  cat(paste("Trip Number:", trip_number), 
      paste("Number of valid observations:", nrow(temp_data)), 
      paste("Prediction accuracy:", round(mean(na.omit((temp_data$speed_cat == 1 & temp_data$speed_lim2 > 80) | 
                                                         (temp_data$speed_cat == 2 & temp_data$speed_lim2 > 65 & temp_data$speed_lim2 <= 80) |
                                                         (temp_data$speed_cat == 3 & temp_data$speed_lim2 >= 55 & temp_data$speed_lim2 <= 65) |
                                                         (temp_data$speed_cat == 4 & temp_data$speed_lim2 > 41 & temp_data$speed_lim2 <= 55) |
                                                         (temp_data$speed_cat == 5 & temp_data$speed_lim2 > 31 & temp_data$speed_lim2 <= 41) |
                                                         (temp_data$speed_cat == 6 & temp_data$speed_lim2 >= 20 & temp_data$speed_lim2 <= 31) |
                                                         (temp_data$speed_cat == 7 & temp_data$speed_lim2 > 6 & temp_data$speed_lim2 <= 20) |
                                                         (temp_data$speed_cat == 8 & temp_data$speed_lim2 <= 6))), 4)), sep = "\n")
}

# one_trip_acc("11311AE898644124AF2B9630DDCC308900"), one_trip_acc("021E2989291744F1A11A6C6AF53336E000")
# one_trip_acc("0BDFB94CCF4C4C4390972435809504DF00"), one_trip_acc("09A74154FBDD45DABA0E34996B2D022500")


ggplot(data.frame("Obs" = c(4495, 3199, 2206, 617, 438, 107, 1369, 288, 236),
                  "Accuracy" = c(0.9413, 0.832, 0.764, 0.705, 0.6575, 0.4872, 0.5245, 0.6982, 0.9655)), 
       aes(x = Obs, y = Accuracy)) + geom_point() + theme(panel.background = element_rect(fill = 'gray93')) + 
  geom_line(linetype = "dashed", color = "dodgerblue2", arrow = arrow(angle = 15, type = "closed")) + 
  xlab("Number of Obs") + geom_vline(xintercept = 2206, colour = "orangered1", linetype = "twodash", size = 0.8) + 
  annotate("text", x = 1750, y = 0.79, label = "# Obs = 2206", colour = "orangered1", size = 4.5) + 
  annotate("text", x = 1750, y = 0.76, label = "Stable?", colour = "orangered1", size = 4.5)
