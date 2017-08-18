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


############################# sample size  ###########################
# set some intolerance
accuracy = function(actual, predicted) {
  (predicted <= actual + 5) & (predicted >=  actual - 5)
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


# most frequent value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

points$speed_lim3 = (points %>% mutate(longitude = round(longitude, 3), latitude = round(latitude, 3)) %>%
  group_by(longitude, latitude) %>%
  mutate(speed_lim3 = Mode(speed_lim2)))$speed_lim3

points$speed_lim3 = c(rollapply(points$speed_lim2, width = 200, Mode, fill = NA))
points = na.omit(points)

factpal = colorFactor(colors()[c(257, 91, 507 , 452, 129)], points[points$traffic_den >= 4, ]$speed_lim3)
# factpal = colorFactor(topo.colors(3), points[points$traffic_den >= 5, ]$road_type)

leaflet(points[points$traffic_den >= 4, ]) %>% addTiles() %>%
  addCircles(lng = ~points[points$traffic_den >= 4, ]$longitude, lat = ~points[points$traffic_den >= 4, ]$latitude, weight = 1,  color = ~factpal(speed_lim3),
             radius = ~sqrt(points[points$traffic_den >= 4, ]$speed_lim3) * 10, fillOpacity = 1) %>%
  addLegend("bottomright", pal = factpal, values = points[points$traffic_den >= 4, ]$speed_lim3, title = "Speed Limit", opacity = 1)
# This plot is very similar to actual speed limit plot


############################### Validate with DIS Contextual Data ##############################
# 
library(clue)
setwd("/san-data/usecase/magnet_g/misc/PCA_DATA_VIS/Yiming/Accuracy/")
all_data = fread("Timed_big_data.csv") %>%
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
  
  temp_data = all_data[all_data$trip_number == trip_number, ] %>% 
    dplyr::select(longitude, latitude, trip_number, speed, stop_ind) %>% 
    transmute(longitude = round(.$longitude, 4), latitude = round(.$latitude, 4), trip_number = trip_number, 
              speed = speed, stop_ind = stop_ind) %>% distinct(longitude, latitude) 
  # add traffic density
  temp_data = left_join(temp_data, left_join(temp_data, all_data %>% 
                                               group_by(longitude, latitude) %>%
                                               dplyr::summarise(traffic_den = n()), by = c("longitude" = "longitude", "latitude" = "latitude")), 
                        by = c("longitude" = "longitude", "latitude" = "latitude"))
  # add average speed
  temp_data = left_join(temp_data, all_data[all_data$longitude %in% temp_data$longitude & all_data$latitude %in% temp_data$latitude, ] %>% 
                          group_by(longitude, latitude) %>% dplyr::summarise(avg_speed = mean(speed[speed != 0])),
                        by = c("longitude" = "longitude", "latitude" = "latitude"))
  # add quantile speed
  temp_data = left_join(temp_data, all_data[all_data$longitude %in% temp_data$longitude & all_data$latitude %in% temp_data$latitude, ] %>% 
                          group_by(longitude, latitude) %>% dplyr::summarise(quan_speed = quantile(speed, 0.5)),
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
  acc = round(mean(na.omit((temp_data$speed_cat == 1 & temp_data$speed_lim2 > 80) |
                             (temp_data$speed_cat == 2 & temp_data$speed_lim2 > 65 & temp_data$speed_lim2 <= 80) |
                             (temp_data$speed_cat == 3 & temp_data$speed_lim2 >= 55 & temp_data$speed_lim2 <= 65) |
                             (temp_data$speed_cat == 4 & temp_data$speed_lim2 > 41 & temp_data$speed_lim2 <= 55) |
                             (temp_data$speed_cat == 5 & temp_data$speed_lim2 > 31 & temp_data$speed_lim2 <= 41) |
                             (temp_data$speed_cat == 6 & temp_data$speed_lim2 >= 20 & temp_data$speed_lim2 <= 31) |
                             (temp_data$speed_cat == 7 & temp_data$speed_lim2 > 6 & temp_data$speed_lim2 <= 20) |
                             (temp_data$speed_cat == 8 & temp_data$speed_lim2 <= 6))), 4)
  # calculate accuracy for this trip
  cat(paste("Trip Number:", trip_number),
      paste("Number of valid observations:", nrow(temp_data)),
      paste("Prediction accuracy:", acc), sep = "\n")
  
  # list(obs = nrow(temp_data), accuracy = acc)
}

# one_trip_acc("11311AE898644124AF2B9630DDCC308900"), one_trip_acc("021E2989291744F1A11A6C6AF53336E000")
# one_trip_acc("0BDFB94CCF4C4C4390972435809504DF00"), one_trip_acc("09A74154FBDD45DABA0E34996B2D022500")

obs = c(4495, 3199, 2206, 617, 438, 107, 1369, 288, 236, 308, 
        119, 564, 1966, 101, 1391, 412, 413, 8941, 132, 399, 
        72, 641, 71, 19, 3114, 707, 690, 132, 3311, 359,
        142, 19679, 1035, 229, 777, 124, 211, 92, 312, 2446,
        4907, 4934, 1081, 265, 391, 325, 3, 209, 2081, 394,
        896, 625, 6175, 10558, 2453, 126, 2244, 2482, 585)

acc = c(0.9413, 0.832, 0.764, 0.705, 0.6575, 0.4872, 0.5245, 0.6982, 0.9655, 0.7361, 
        0.513, 0.7555, 0.5885, 0.5545, 0.9303, 0.3383, 0.5811, 0.8943, 0.4211, 0.7293, 
        0.5833, 0.5531, 0.6053, 0.1111, 0.6329, 0.4651, 0.7083, 0.6515, 0.8855, 0.5419,
        0.4727, 0.6476, 0.1798, 0.7249, 0.6494, 0.7561, 0.8104, 0.4103, 0.4231, 0.7186,
        0.6271, 0.3933, 0.4433, 0.3269, 0.9054, 0.7006, 1, 0.8578, 0.7544, 0.7868,
        0.8103, 0.9808, 0.6345, 0.8299, 0.8092, 0.3509, 0.7288, 0.9544, 0.6874)

# all 60 trips
ggplot(data.frame("Obs" = obs, "Accuracy" = acc), 
       aes(x = Obs, y = Accuracy)) + geom_point() + theme(panel.background = element_rect(fill = 'gray93')) + 
  geom_smooth(aes(y = Accuracy), colour = "dodgerblue2", span = 0.5, se = FALSE) + # span controls wiggliness
  scale_x_continuous(name = "Number of Obs", breaks = seq(0, 20000, 2000)) + 
  scale_y_continuous(name = "Accuracy", breaks = seq(0, 1, 0.1)) 

# trips with obs > 500
ggplot(data.frame("Obs" = obs[which(obs > 500)],
                  "Accuracy" = acc[which(obs > 500)]), 
       aes(x = Obs, y = Accuracy)) + geom_point() + theme(panel.background = element_rect(fill = 'gray93')) + 
  geom_smooth(aes(y = Accuracy), colour = "dodgerblue2", span = 0.45, se = FALSE) + # span controls wiggliness
  scale_x_continuous(name = "Number of Obs", breaks = seq(0, 20000, 2000)) + 
  scale_y_continuous(name = "Accuracy", breaks = seq(0, 1, 0.1)) + 
  geom_vline(xintercept = 2000, colour = "orangered1", linetype = "twodash", size = 0.7) +
  geom_hline(yintercept = 0.7, colour = "#009E73", linetype = "dashed", size = 0.7) +
  annotate("text", x = 1000, y = 0.85, label = "Stable?", colour = "orangered1", size = 4.5)



################################### at a location level ##############################
# actual_data contains 60 trips' information
# could compare 3 vs 4 decimals
actual_data = fread("DIS_trips.csv")
actual_data$V1 <- NULL
actual_data$trip_number = actual_data$trip_numbe
actual_data$trip_numbe <- NULL

temp_data = all_data[all_data$trip_number %in% actual_data$trip_number, ] %>% 
  dplyr::select(longitude, latitude, trip_number, speed, stop_ind) %>% 
  transmute(longitude = round(.$longitude, 4), latitude = round(.$latitude, 4), trip_number = trip_number, 
            speed = speed, stop_ind = stop_ind) %>% distinct(trip_number, longitude, latitude) 

# add traffic density
temp_data = left_join(temp_data, left_join(temp_data, all_data %>% 
                                             group_by(longitude, latitude) %>%
                                             dplyr::summarise(traffic_den = n()), by = c("longitude" = "longitude", "latitude" = "latitude")), 
                      by = c("longitude" = "longitude", "latitude" = "latitude"))
# add average speed
temp_data = left_join(temp_data, all_data[all_data$longitude %in% temp_data$longitude & all_data$latitude %in% temp_data$latitude, ] %>% 
                        group_by(longitude, latitude) %>% dplyr::summarise(avg_speed = mean(speed[speed != 0])),
                      by = c("longitude" = "longitude", "latitude" = "latitude"))
# add quantile speed
temp_data = left_join(temp_data, all_data[all_data$longitude %in% temp_data$longitude & all_data$latitude %in% temp_data$latitude, ] %>% 
                        group_by(longitude, latitude) %>% dplyr::summarise(quan_speed = quantile(speed, 0.5)),
                      by = c("longitude" = "longitude", "latitude" = "latitude"))
# add predicted road type
temp_data$road_type = ifelse(cl_predict(kmeans0, temp_data[, c(5, 6)]) == 1, "Residential Road", 
                             ifelse(cl_predict(kmeans0, temp_data[, c(5, 6)]) == 2, "Freeway", "Business District"))

# add speed limit 1
temp_data$speed_lim1 = ifelse(temp_data$road_type == "Freeway", 70, ifelse(temp_data$road_type == "Business District", 45, 30))

# add speed limit 2
temp_data$speed_lim2 = ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (20 + 35) / 2, 20,
                              ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (35 + 55) / 2, 35, 
                                     ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (55 + 65) / 2, 55,
                                            ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (65 + 70) / 2, 65, 70))))

temp_data$speed_lim3 = (temp_data %>% mutate(speed_lim_temp = c(rollapply(speed_lim2, width = 15, mean, fill = NA))) %>%
                          mutate(speed_lim3 = ifelse(speed_lim_temp <= (20 + 35) / 2, 20,
                                                     ifelse(speed_lim_temp <= (35 + 55) / 2, 35, 
                                                            ifelse(speed_lim_temp <= (55 + 65) / 2, 55,
                                                                   ifelse(speed_lim_temp <= (65 + 70) / 2, 65, 70))))))$speed_lim3
temp_data$trip_number = temp_data$trip_number.x
temp_data$trip_number.x <- temp_data$trip_number.y <- NULL

# add actual speed information
temp_data = left_join(temp_data, actual_data[actual_data$longitude %in% temp_data$longitude & actual_data$latitude %in% temp_data$latitude, c("longitude", "latitude", "speed_cat")], 
                      by = c("longitude" = "longitude", "latitude" = "latitude"))

# add comparison
temp_data$comp = (temp_data$speed_cat == 1 & temp_data$speed_lim3 > 80) |
  (temp_data$speed_cat == 2 & temp_data$speed_lim3 >= 60 & temp_data$speed_lim3 <= 80) |
  (temp_data$speed_cat == 3 & temp_data$speed_lim3 >= 50 & temp_data$speed_lim3 <= 65) |
  (temp_data$speed_cat == 4 & temp_data$speed_lim3 >= 35 & temp_data$speed_lim3 <= 55) |
  (temp_data$speed_cat == 5 & temp_data$speed_lim3 >= 25 & temp_data$speed_lim3 <= 40) |
  (temp_data$speed_cat == 6 & temp_data$speed_lim3 >= 15 & temp_data$speed_lim3 <= 30) |
  (temp_data$speed_cat == 7 & temp_data$speed_lim3 >= 5 & temp_data$speed_lim3 <= 20) |
  (temp_data$speed_cat == 8 & temp_data$speed_lim3 <= 6)

acc_df = na.omit(temp_data %>% 
                   group_by(traffic_den) %>% dplyr::summarise(acc = mean(na.omit(comp)), n = n())) %>%
  filter(.$n >= 10)


ggplot(acc_df[acc_df$acc != 0, ], aes(x = traffic_den, y = acc)) + geom_point() + theme(panel.background = element_rect(fill = 'gray93')) + 
  geom_smooth(aes(y = acc), colour = "dodgerblue2", span = 0.4, se = FALSE) + # span controls wiggliness
  scale_x_continuous(name = "Number of Cars", breaks = seq(1, 44, 2)) + 
  scale_y_continuous(name = "Accuracy", breaks = seq(0, 1, 0.1), limits = c(0, 1)) + 
  labs(title = "Accuracy vs # of Cars (4 decimals)")


### Comparison with 3 decimals
actual_data_3_decimals = actual_data %>% mutate(longitude = round(.$longitude, 3), latitude = round(.$latitude, 3))


temp_data = all_data[all_data$trip_number %in% unique(actual_data_3_decimals$trip_number), ] %>% 
  dplyr::select(longitude, latitude, trip_number, speed, stop_ind) %>% 
  transmute(longitude = round(.$longitude, 3), latitude = round(.$latitude, 3), trip_number = trip_number, 
            speed = speed, stop_ind = stop_ind) %>% distinct(trip_number, longitude, latitude) 

# add traffic density
temp_data = left_join(temp_data, left_join(temp_data, all_data %>% 
                                             mutate(longitude = round(.$longitude, 3), latitude = round(.$latitude, 3)) %>% 
                                             group_by(longitude, latitude) %>%
                                             dplyr::summarise(traffic_den = n()), by = c("longitude" = "longitude", "latitude" = "latitude")), 
                      by = c("longitude" = "longitude", "latitude" = "latitude"))

# add average speed
temp_data = left_join(temp_data, all_data[all_data$longitude %in% temp_data$longitude & all_data$latitude %in% temp_data$latitude, ] %>% 
                        mutate(longitude = round(.$longitude, 3), latitude = round(.$latitude, 3)) %>% 
                        group_by(longitude, latitude) %>% dplyr::summarise(avg_speed = mean(na.omit(speed[speed != 0]))),
                      by = c("longitude" = "longitude", "latitude" = "latitude"))
# add quantile speed
temp_data = left_join(temp_data, all_data[all_data$longitude %in% temp_data$longitude & all_data$latitude %in% temp_data$latitude, ] %>% 
                        mutate(longitude = round(.$longitude, 3), latitude = round(.$latitude, 3)) %>% 
                        group_by(longitude, latitude) %>% dplyr::summarise(quan_speed = quantile(speed, 0.5)),
                      by = c("longitude" = "longitude", "latitude" = "latitude"))

# add predicted road type
temp_data$road_type = ifelse(cl_predict(kmeans, temp_data[, c("traffic_den", "avg_speed")]) == 1, "Business District", 
                             ifelse(cl_predict(kmeans, temp_data[, c("traffic_den", "avg_speed")]) == 2, "Residential Road", "Freeway"))

# add speed limit 1
temp_data$speed_lim1 = ifelse(temp_data$road_type == "Freeway", 70, ifelse(temp_data$road_type == "Business District", 45, 30))

# add speed limit 2
temp_data$speed_lim2 = ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (20 + 35) / 2, 20,
                              ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (35 + 55) / 2, 35, 
                                     ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (55 + 65) / 2, 55,
                                            ifelse(temp_data$speed_lim1 * 0.5 + temp_data$quan_speed * 0.5 <= (65 + 70) / 2, 65, 70))))

# add actual speed information
temp_data = left_join(temp_data, actual_data[actual_data_3_decimals$longitude %in% temp_data$longitude & actual_data_3_decimals$latitude %in% temp_data$latitude, c("longitude", "latitude", "speed_cat")], 
                      by = c("longitude" = "longitude", "latitude" = "latitude"))

# add comparison
temp_data$comp = (temp_data$speed_cat == 1 & temp_data$speed_lim2 > 80) |
  (temp_data$speed_cat == 2 & temp_data$speed_lim2 >= 60 & temp_data$speed_lim2 <= 80) |
  (temp_data$speed_cat == 3 & temp_data$speed_lim2 >= 50 & temp_data$speed_lim2 <= 65) |
  (temp_data$speed_cat == 4 & temp_data$speed_lim2 >= 35 & temp_data$speed_lim2 <= 55) |
  (temp_data$speed_cat == 5 & temp_data$speed_lim2 >= 25 & temp_data$speed_lim2 <= 40) |
  (temp_data$speed_cat == 6 & temp_data$speed_lim2 >= 15 & temp_data$speed_lim2 <= 30) |
  (temp_data$speed_cat == 7 & temp_data$speed_lim2 >= 5 & temp_data$speed_lim2 <= 20) |
  (temp_data$speed_cat == 8 & temp_data$speed_lim2 <= 6)

acc_df_3_decimals = na.omit(temp_data %>% 
                              group_by(traffic_den) %>% dplyr::summarise(acc = mean(na.omit(comp)), n = n())) %>%
  filter(.$n >= 10)


ggplot(acc_df_3_decimals[acc_df_3_decimals$acc != 0, ], aes(x = traffic_den, y = acc)) + geom_point() + theme(panel.background = element_rect(fill = 'gray93')) + 
  geom_smooth(aes(y = acc), colour = "dodgerblue2", span = 0.4, se = FALSE) + # span controls wiggliness
  scale_x_continuous(name = "Number of Cars", breaks = seq(0, 330, 10)) + 
  scale_y_continuous(name = "Accuracy", breaks = seq(0, 1, 0.1), limits = c(0, 1)) + 
  labs(title = "Accuracy vs # of Cars (3 decimals)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot part of points
ggplot(acc_df_3_decimals[acc_df_3_decimals$traffic_den <= 250 & acc_df_3_decimals$acc != 0, ], aes(x = acc_df_3_decimals[acc_df_3_decimals$traffic_den <= 250 & acc_df_3_decimals$acc != 0, ]$traffic_den)) + 
  scale_x_continuous(name = "Number of Cars", breaks = seq(0, 250, 10)) + 
  scale_y_continuous(name = "Accuracy", breaks = seq(0, 1, 0.1), limits = c(0, 1), sec.axis = sec_axis(~.*1500, name = "Number of points with this traffic density")) + 
  geom_point(aes(y = acc_df_3_decimals[acc_df_3_decimals$traffic_den <= 250 & acc_df_3_decimals$acc != 0, ]$acc, colour = "Accuracy")) + 
  geom_point(aes(y = acc_df_3_decimals[acc_df_3_decimals$traffic_den <= 250 & acc_df_3_decimals$acc != 0, ]$n / 1500, colour = "Number of points with this traffic density")) + 
  theme(panel.background = element_rect(fill = 'gray93'), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.2)) + 
  geom_smooth(aes(y = acc, colour = "Accuracy"), span = 0.4, se = FALSE) + # span controls wiggliness
  geom_smooth(aes(y = n / 1500, colour = "Number of points with this traffic density"), span = 0.4, se = FALSE) + 
  labs(title = "Accuracy vs # of Cars (3 decimals)")



plot_data = na.omit(temp_data)

factpal = colorFactor(colors()[c(257, 91, 507 , 452, 129)], plot_data$speed_lim2)

leaflet(plot_data) %>% addTiles() %>%
  addCircles(lng = ~plot_data[plot_data$comp == TRUE, ]$longitude, lat = ~plot_data[plot_data$comp == TRUE, ]$latitude, weight = 1,  color = ~factpal(speed_lim2),
             radius = 7, fillOpacity = .8, group = "Correct") %>%
  addCircles(lng = ~plot_data[plot_data$comp == FALSE, ]$longitude, lat = ~plot_data[plot_data$comp == FALSE, ]$latitude, weight = 1,  color = "red",
             radius = 6, fillOpacity = .7, group = "Incorrect") %>%
  addLegend("bottomright", pal = factpal, values =plot_data$speed_lim2, title = "Predicted Speed Limit", opacity = 1) %>%

  
  # Layers control
  addLayersControl(
    overlayGroups = c("Correct", "Incorrect"),
    options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Incorrect")



# Find the trip who drove over speed at residential area
# [1] "0440E235DB634CBBA9CFE4D2FCC70A2400" "0BDFB94CCF4C4C4390972435809504DF00" "11311AE898644124AF2B9630DDCC308900"
# [4] "138D8F0E12714C79B07AE7232263294300" "14B62AAF76BC4C39B63B28864144264A00" "1E121644216644D493F589511F8ED10000"
# [7] "1E52173A9D6945C588C328D5E014DBB900" "206143C190DA4969A68604BD4DB46D1000" "23AD3C7ED1BC4C4AA7167E049388CF9400"
# [10] "262395DF4E55420EAB9AE2CCDBCD576800" "27BFF261BD0446A0ACBF06A820F2E98000" "2D47A7088EC64425B6B7D1E028FA5F2400"
# [13] "32722FDEC8474250833F282B3C7D49F800" "36304E47D369472C98E99753179841FF00" "36BFB59D9B664A25BE660E715A8C3B9A00"
# [16] "4A2198F8E99B4C0486FE9DD1738A948700" "6987641383894AC0AFE4CD12EB2B6A7F00" "6A919AC3A9154078B882FBD2D2F6C61700"
# [19] "6FCA5C5EB33C4F99AC971B482D919AC100" "729852DD1A8A44B8B9B9F95A6F4A3F5E00" "797F6DCF6C4546C9A357D0D898379F5300"
# [22] "7FB2232D54B04CFE9811DD27AE000E2200" "C74523A1C5BC445883D15226F624FEBC00" "D31489CE1E7B4399804AC47A7EABA5DA00"
# [25] "E41C8ACF31ED4D95AF2E3EFF4408A92400" "E8D797892E2A4085981FB3133ED434F600"
plot_data_true = plot_data[plot_data$comp == TRUE, ]
plot_data_false = plot_data[plot_data$comp == FALSE, ]
factpal = colorFactor(colors()[c(257, 91, 507 , 452, 129)], plot_data_true$speed_lim2)
leaflet(plot_data_true) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,  color = ~factpal(speed_lim2),
             radius = 7, fillOpacity = .8, group = "Correct") %>%
  addCircles(lng = ~plot_data_false$longitude, lat = ~plot_data_false$latitude, weight = 1,  color = "red",
             radius = 6, fillOpacity = .55, group = "Incorrect") %>%
  addLegend("bottomright", pal = factpal, values = plot_data$speed_lim2, title = "Predicted Speed Limit", opacity = 1) %>%
  
  # Layers control
  addLayersControl(
    overlayGroups = c("Correct", "Incorrect"),
    options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Incorrect")
