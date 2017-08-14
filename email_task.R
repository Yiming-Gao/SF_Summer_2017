###setwd("/san-data/usecase/magnet_g/misc/PCA_DATA_VIS")
all_data = fread("Timed_big_data.csv") %>%
  filter(latitude > -99999 & longitude > -9999) %>%
  dplyr::select(
    trip_number,
    timestmp_local,
    latitude,
    longitude,
    stop_ind,
    latG,
    lonG,
    speed,
    ang_speed_gyro #the change in orientation of the car in that second, measured in degrees/second,angular speed for gyroscope
  ) %>%
  mutate(ratio = latG * ang_speed_gyro,
         longitude = round(.$longitude, 4),
         latitude = round(.$latitude, 4)) %>%
  na.omit() %>%
  filter(abs(ratio) < 100) 

latitude_min = 39.80000
latitude_max = 40.10328
longitude_min = -83.25714
longitude_max = -82.70507

dataset = separate(all_data,
                  timestmp_local,
                  c('Hour_editing_needed', 'Minute', 'Second'),
                  sep = ':',
                  fill = 'right',
                  remove = FALSE) %>%
  filter(latitude >= latitude_min & latitude <= latitude_max & longitude <= longitude_max & longitude >= longitude_min) %>%
  mutate(Hour = as.numeric(substr(Hour_editing_needed,
                                  nchar(Hour_editing_needed)-1 ,
                                  nchar(Hour_editing_needed))),
         Minute = as.numeric(Minute),
         Second = as.numeric(Second),
         Date = as.Date(substr(as.character(timestmp_local), 1, 9)))

all_location = dataset %>% dplyr::select(trip_number, longitude, latitude) %>% group_by(trip_number) %>% distinct(longitude, latitude)
all_points = all_location[, 1:2] %>% distinct(longitude, latitude)

# add traffic density
traffic_den = all_location %>%
  group_by(longitude, latitude) %>%
  summarise(traffic_den = n())
all_points = left_join(all_points, traffic_den, by = c("longitude" = "longitude", "latitude" = "latitude")) 
rm(traffic_den)

# add average speed
avg_speed = dataset %>%
  group_by(longitude, latitude) %>%
  summarise(avg_speed = mean(speed[speed != 0]))
all_points = left_join(all_points, avg_speed, by = c("longitude" = "longitude", "latitude" = "latitude")) 
rm(avg_speed)

# add road type (k means)
# road type classification based on k-means clustering, using (longitude, latitude and ) average speed, traffic density
set.seed(627)
all_points = na.omit(all_points) # k-means doesn't handle missing values
kmeans0 = kmeans(all_points[, c(3, 4)], centers = 3, iter.max = 20, nstart = 20)
all_points$road_type = kmeans0$cluster
all_points$road_type = ifelse(all_points$road_type == 2, "Freeway", 
                              ifelse(all_points$road_type == 1, "Residential Road", "Business District"))

# add median speed
quan_speed = dataset %>%
  group_by(longitude, latitude) %>%
  summarise(quan_speed = quantile(speed, 0.5))
all_points = left_join(all_points, quan_speed, by = c("longitude" = "longitude", "latitude" = "latitude")) 
rm(quan_speed)

# add predicted speed limit
all_points$speed_lim1 = ifelse(all_points$road_type == "Freeway", 70, ifelse(all_points$road_type == "Business District", 45, 30))
all_points$speed_lim = ifelse(all_points$speed_lim1 * 0.5 + all_points$quan_speed * 0.5 <= (20 + 35) / 2, 20,
                           ifelse(all_points$speed_lim1 * 0.5 + all_points$quan_speed * 0.5 <= (35 + 55) / 2, 35, 
                                  ifelse(all_points$speed_lim1 * 0.5 + all_points$quan_speed * 0.5 <= (55 + 65) / 2, 55,
                                         ifelse(all_points$speed_lim1 * 0.5 + all_points$quan_speed * 0.5 <= (65 + 70) / 2, 65, 70))))
all_points$speed_lim1 = NULL


# Merge additional variables
all_columbus = left_join(dataset, all_points, by = c("longitude" = "longitude", "latitude" = "latitude")) 
all_columbus$timestmp_local <- all_columbus$Hour_editing_needed <- all_columbus$stop_ind <- all_columbus$ratio <- NULL



######################### 1a. Trips per day #########################
temp_0313 = all_columbus[all_columbus$Date == "2016-03-13", ]
temp_0313 = left_join(temp_0313, temp_0313 %>% dplyr::select(trip_number, longitude, latitude, road_type, speed_lim_smooth) %>% 
                        group_by(trip_number) %>% 
                        distinct(longitude, latitude) %>%
                        group_by(longitude, latitude) %>%
                        summarise(traffic_den_day = n()), 
                      by = c("longitude" = "longitude", "latitude" = "latitude"))
# temp_0313[temp_0313$traffic_den != temp_0313$traffic_den_day, ]

# visualize traffic density on 03/13/2016
plot_data_0313 = temp_0313 %>% dplyr::select(longitude, latitude, traffic_den_day, road_type, avg_speed, speed_lim_smooth) %>%
  distinct(longitude, latitude, traffic_den_day, road_type, avg_speed, speed_lim_smooth)

pal <- colorNumeric(
  palette = c("#E8E4F0", "#C4CBE3", "#91B4D6", "#549BC6", "#1B79B5", "#045D92", "#023858"),
  domain = plot_data_0313$traffic_den_day)

# 1b. map of traffic density
leaflet(plot_data_0313) %>% addTiles() %>%
  addCircles(lng = ~plot_data_0313[plot_data_0313$traffic_den_day > 1, ]$longitude, lat = ~plot_data_0313[plot_data_0313$traffic_den_day > 1, ]$latitude,
             weight = 1,  color = ~pal(plot_data_0313[plot_data_0313$traffic_den_day > 1, ]$traffic_den_day),
             radius = 8, fillOpacity = 1, group = "Traffic density > 1") %>%
  addCircles(lng = ~plot_data_0313[plot_data_0313$traffic_den_day == 1, ]$longitude, lat = ~plot_data_0313[plot_data_0313$traffic_den_day == 1, ]$latitude,
             weight = 1,  color = ~pal(plot_data_0313[plot_data_0313$traffic_den_day == 1, ]$traffic_den_day),
             radius = 8, fillOpacity = 0.3, group = "Traffic density = 1") %>%
  addLegend("bottomright", pal = pal, values = sort(unique(plot_data_0313$traffic_den_day)), title = "Traffic Density on 03/13/2016", opacity = 1) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Traffic density > 1", "Traffic density = 1"),
    options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Traffic density = 1")

# 2b. map of average speed
pal <- colorNumeric(
  palette = "Reds",
  domain = plot_data_0313$avg_speed)

leaflet(plot_data_0313) %>% addTiles() %>%
  addCircles(lng = ~plot_data_0313$longitude, lat = ~plot_data_0313$latitude,
             weight = 1,  color = ~pal(avg_speed),
             radius = 7, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values = plot_data_0313$avg_speed, title = "Average Speed on 03/13/2016", opacity = 1)


# 2b. map of average speed (without background)
pal <- colorNumeric(
  palette = "Reds",
  domain = plot_data_0313$avg_speed)

leaflet(plot_data_0313) %>% 
  addCircles(lng = ~plot_data_0313$longitude, lat = ~plot_data_0313$latitude,
             weight = 1,  color = ~pal(avg_speed),
             radius = 7, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values = plot_data_0313$avg_speed, title = "Average Speed on 03/13/2016", opacity = 1)



# map of speed limit
factpal = colorFactor(colors()[c(257, 91, 507 , 452, 129)], plot_data_0313$speed_lim_smooth)
# factpal = colorFactor(topo.colors(3), points[points$traffic_den >= 5, ]$road_type)

leaflet(plot_data_0313) %>% addTiles() %>%
  addCircles(lng = ~plot_data_0313$longitude, lat = ~plot_data_0313$latitude, weight = 1,  color = ~factpal(speed_lim_smooth),
             radius = 7.5, fillOpacity = 1) %>%
  addLegend("bottomright", pal = factpal, values = plot_data_0313$speed_lim_smooth, title = "Speed Limit Prediction", opacity = 1)


# map of road type
# visualize road type
factpal = colorFactor(topo.colors(3), plot_data_0313$road_type)

leaflet(plot_data_0313) %>% addTiles() %>%
  addCircles(lng = ~plot_data_0313$longitude, lat = ~plot_data_0313$latitude, weight = 1,  color = ~factpal(road_type),
             radius = 7.5, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = factpal, values = plot_data_0313$road_type, title = "Road Type Prediction", opacity = 1)




################################ Plot each trip information ###############################
temp2 = all_columbus %>% dplyr::select(longitude, latitude, trip_number, lonG, speed, Hour, Minute, Second, traffic_den, avg_speed, speed_lim) 
temp2$Time = temp2$Hour + temp2$Minute / 60 + temp2$Second / 3600
temp2$label = ifelse(temp2$Hour >=0 & temp2$Hour <12, "am", "pm")
temp2$Hour = ifelse(temp2$label == "pm" & temp2$Hour != 12, temp2$Hour - 12, temp2$Hour)
temp2$Mode = as.factor(ifelse(temp2$lonG > 0, "Acceleration", "Deceleration"))

# 1di
speed_plot_smooth1 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == "00EB796E3FD14E5FA5C3EC1C3089B2B800", ][1: 764, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  plot_data$traffic_den_smooth = c(rollapply(plot_data$traffic_den, width = 10, mean, fill = 0))
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    scale_y_continuous(name = "Speed", breaks = seq(0, 100, 10), sec.axis = sec_axis(~./(60 / 14), breaks = seq(0, 15, 1), name = "Traffic Density")) + 
    geom_line(aes(y = speed, colour = "Vehicle speed"), size = 0.8) + 
    geom_line(aes(y = speed_lim_smooth, colour = "Predicted speed limit"), size = 0.8) +
    geom_smooth(aes(y = avg_speed, colour = "Average road speed"), se = FALSE, span = 0.2) + 
    geom_point(aes(y = speed, colour = Mode1)) +
    ggtitle(paste("Trip number: ", "00EB796E3FD14E5FA5C3EC1C3089B2B800")) + ylab("Speed") +
    geom_ribbon(aes(ymin = rep(0, 764), ymax = traffic_den_smooth * (60 / 14)), alpha = 0.3, fill = 'gray65') + 
    scale_color_manual(name = "Variables", 
                       values = c("Vehicle speed" = "dodgerblue2", 
                                  "Predicted speed limit" = "indianred1", 
                                  "Average road speed" = "goldenrod2", 
                                  "Acceleration" = "green", 
                                  "Deceleration" = "red1")) + 
    theme(panel.background = element_rect(fill = 'gray98'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.9, 0.85))}

# speed_plot_smooth1("17074D96C94C4CF19C82309E39539A1D00")

speed_plot_line1 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ", 
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    scale_y_continuous(name = "Speed", breaks = seq(0, 100, 10), sec.axis = sec_axis(~./4, breaks = seq(0, 25, 5), name = "Traffic Density")) + 
    geom_line(aes(y = speed, colour = "Vehicle speed"), size = 0.8) + 
    geom_line(aes(y = speed_lim, colour = "Predicted speed limit"), size = 0.8) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    geom_line(aes(y = traffic_den * 4, colour = "Traffic density"), size = 0.8) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.85, 0.8))}

# speed_plot_line1("17074D96C94C4CF19C82309E39539A1D00")

# 1dii
acceleration_plot1 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    scale_y_continuous(name = "Speed", breaks = seq(0, 100, 10), sec.axis = sec_axis(~./10, breaks = seq(0, 10, 1), name = "Traffic Density")) + 
    geom_point(aes(y = speed, colour = Mode)) + 
    geom_smooth(aes(y = speed_lim, colour = "Predicted speed limit"), se = FALSE, span = 0.4) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    geom_smooth(aes(y = traffic_den * 10, colour = "Traffic density"), se = FALSE, span = 0.4) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.85, 0.8))}

# acceleration_plot1("17074D96C94C4CF19C82309E39539A1D00")



# 2di
speed_plot_smooth2 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    geom_line(aes(y = speed, colour = "dodgerblue2"), size = 0.8) + 
    geom_line(aes(y = speed_lim_smooth, colour = "indianred1"), size = 0.8) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    scale_color_manual(labels = c("Vehicle speed", "Predicted speed limit", "Average road speed"), values = c("dodgerblue2", "indianred1", "seagreen3")) + 
    geom_smooth(aes(y = avg_speed, colour = "seagreen3"), se = FALSE, span = 0.4) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.15, 0.8))}

# speed_plot_smooth2("17074D96C94C4CF19C82309E39539A1D00")
# speed_plot_smooth2("00EB796E3FD14E5FA5C3EC1C3089B2B800")

speed_plot_line2 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ][1: 764, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    geom_line(aes(y = speed, colour = "Vehicle speed"), size = 0.8) + 
    geom_line(aes(y = speed_lim_smooth, colour = "Predicted speed limit"), size = 0.8) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    geom_line(aes(y = avg_speed, colour = "Average speed"), size = 0.8) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.85, 0.8))}

# speed_plot_line2("00EB796E3FD14E5FA5C3EC1C3089B2B800")

# 2dii
acceleration_plot2 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ][1: 764, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    geom_point(aes(y = speed, colour = Mode)) + 
    geom_smooth(aes(y = speed_lim_smooth, colour = "indianred1"), se = FALSE, span = 0.4) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    scale_color_manual(labels = c("Acceleration", "Deceleration", "Predicted speed limit", "Average road speed"), values = c("green3", "red1", "indianred1", "seagreen3")) + 
    geom_smooth(aes(y = avg_speed, colour = "seagreen3"), se = FALSE, span = 0.4) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.85, 0.8))}

# acceleration_plot2("17074D96C94C4CF19C82309E39539A1D00")
# acceleration_plot2("02C6D1CFCCF0404B89C0ED4A0EF507DC00")
# acceleration_plot2("0E3CE647C8714AF7A2243AEEBF03A5C300")




# "vehicle speed": blue line, consider 0.2g as "acceleration"
temp2$Mode1 = as.factor(ifelse(temp2$lonG >= 0.15, "Acceleration", ifelse(temp2$lonG <= -0.15, "Deceleration", NA)))
temp2$Mode2 = as.factor(ifelse(temp2$lonG >= 0.2, "Acceleration", ifelse(temp2$lonG <= -0.2, "Deceleration", NA)))

acceleration_plot3 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    geom_line(aes(y = speed, colour = "dodgerblue"), size = 0.8) + 
    geom_point(aes(y = speed, colour = Mode2)) + 
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    scale_color_manual(labels = c("Acceleration", "Deceleration", "Constant speed", "Vehicle Speed"), values = c("green1", "firebrick1", "gray50", "dodgerblue")) + 
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.85, 0.8))}

# acceleration_plot3("00EB796E3FD14E5FA5C3EC1C3089B2B800")


# most frequent value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

temp2$speed_lim_smooth = (temp2 %>% mutate(speed_lim_temp = c(rollapply(speed_lim, width = 15, mean, fill = NA))) %>%
                            mutate(speed_lim_smooth = ifelse(speed_lim_temp <= (20 + 35) / 2, 20,
                                                             ifelse(speed_lim_temp <= (35 + 55) / 2, 35, 
                                                                    ifelse(speed_lim_temp <= (55 + 65) / 2, 55,
                                                                           ifelse(speed_lim_temp <= (65 + 70) / 2, 65, 70))))))$speed_lim_smooth









# 1c
all_columbus_weekends = all_columbus[all_columbus$Date == "2016-03-06" | all_columbus$Date == "2016-03-12" | all_columbus$Date == "2016-03-13" | all_columbus$Date == "2016-03-19",] %>% 
  dplyr::select(trip_number, Hour) %>% distinct(trip_number, Hour) %>% group_by(Hour) %>% summarise(n = n()) %>% mutate(avg = round(n/4))
all_columbus_weekends$Type = as.factor("Weekends")
all_columbus_weekdays = all_columbus[!(all_columbus$Date == "2016-03-06" | all_columbus$Date == "2016-03-12" | all_columbus$Date == "2016-03-13" | all_columbus$Date == "2016-03-19"),] %>% 
  dplyr::select(trip_number, Hour) %>% distinct(trip_number, Hour) %>% group_by(Hour) %>% summarise(n = n()) %>% mutate(avg = round(n/10))
all_columbus_weekdays$Type = as.factor("Weekdays")


ggplot(data = rbind(all_columbus_weekdays, all_columbus_weekends), aes(x = Hour, y = avg, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.65) +
  theme(panel.background = element_rect(fill = 'gray93')) + 
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Average trips per hour on weekdays & weekends") +
  labs(x = "Hour", y = "Count") + 
  scale_y_continuous(breaks = seq(0, 40, 5)) + scale_x_continuous(breaks = seq(0, 23, 1))

# number of trips by weekday/ weekends
ggplot(data = all_columbus %>% dplyr::select(trip_number, Date) %>% group_by(Date) %>% mutate(n = length(unique(trip_number))) %>% distinct(n) %>% mutate(Type = as.factor(ifelse(Date == "2016-03-06" | Date == "2016-03-12" | Date == "2016-03-13"| Date == "2016-03-19", "Weekends", "Weekdays" ))), aes(x = Date, y = n, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.65) + 
  theme(panel.background = element_rect(fill = 'gray93')) + 
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Number of trips on weekdays & weekends") +
  labs(x = "Date", y = "Count") + 
  scale_y_continuous(breaks = seq(0, 550, 50)) 








# visualize speed_lim2 in Columbus
plot_trip_data = all_data[all_data$trip_number == "051A1A2BCE484F3FBC217401836A9AA200", ]

plot_trip_data = plot_trip_data %>% mutate(avg_latG = c(rollapply(latG, width = 10, mean, fill = NA)))

plot_trip_data$algorithm <- NA
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) > 10 & abs(plot_trip_data$latG) > .07] <- "Normal Turn"
plot_trip_data$algorithm[plot_trip_data$latG >= -.07 & plot_trip_data$latG < .07] <- "Normal Driving"
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) < 10 & abs(plot_trip_data$latG) > .07 & abs(plot_trip_data$avg_latG) < .055] <- "Lane Change"
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) < 10 & abs(plot_trip_data$latG) > .07 & abs(plot_trip_data$avg_latG) > .055] <- "Curve in the road"

plot_trip_data_small = rbind(plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 19:53:5.0299999714"): (which(plot_trip_data$timestmp_local == "2016-3-12 19:53:5.0299999714") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 19:59:30.032000065"): (which(plot_trip_data$timestmp_local == "2016-3-12 19:59:30.032000065") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 19:59:35.036999941"): (which(plot_trip_data$timestmp_local == "2016-3-12 19:59:35.036999941") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 19:59:40.002999783"): (which(plot_trip_data$timestmp_local == "2016-3-12 19:59:40.002999783") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 19:59:45.009999752"): (which(plot_trip_data$timestmp_local == "2016-3-12 19:59:45.009999752") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 19:59:50.016000032"): (which(plot_trip_data$timestmp_local == "2016-3-12 19:59:50.016000032") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 19:59:55.022000074"): (which(plot_trip_data$timestmp_local == "2016-3-12 19:59:55.022000074") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 20:0:45.013999939"): (which(plot_trip_data$timestmp_local == "2016-3-12 20:0:45.013999939") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 20:0:50.021000385"): (which(plot_trip_data$timestmp_local == "2016-3-12 20:0:50.021000385") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-12 20:0:55.02699995"): (which(plot_trip_data$timestmp_local == "2016-3-12 20:0:55.02699995") + 4), ])
plot_trip_data_small = rbind(plot_trip_data[914: 1078, ],
                             plot_trip_data[1234: 1248, ])
#      
# 

pal <- colorNumeric(
  palette = "Reds",
  domain = plot_trip_data$speed)

leaflet(plot_trip_data) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,  color = ~pal(speed),
             radius = 7, fillOpacity = 0.8,
             popup = paste("Speed: ", plot_trip_data$speed, "<br>",
                           "Algorithm: ", plot_trip_data$algorithm)) %>%
  addCircles(lng = ~plot_trip_data_small$longitude, lat = ~plot_trip_data_small$latitude, 
             weight = 1,  color = "dodgerblue", radius = 9, fillOpacity = 1,
             popup = paste("Speed: ", plot_trip_data_small$speed, "<br>",
                           "Algorithm: ", plot_trip_data_small$algorithm)) %>%
  addLegend("bottomright", pal = pal, values = plot_trip_data$speed, title = "Vehicle speed", opacity = 1)






# visualize speed_lim2 in Columbus
plot_trip_data = all_data[all_data$trip_number == "00A5B2C4E9254172BE08387115D195FA00", ]

plot_trip_data = plot_trip_data %>% mutate(avg_latG = c(rollapply(latG, width = 10, mean, fill = NA)))

plot_trip_data$algorithm <- NA
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) > 10 & abs(plot_trip_data$latG) > .07] <- "Normal Turn"
plot_trip_data$algorithm[plot_trip_data$latG >= -.07 & plot_trip_data$latG < .07] <- "Normal Driving"
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) < 10 & abs(plot_trip_data$latG) > .07 & abs(plot_trip_data$avg_latG) < .055] <- "Lane Change"
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) < 10 & abs(plot_trip_data$latG) > .07 & abs(plot_trip_data$avg_latG) > .055] <- "Curve in the road"

plot_trip_data_small = rbind(plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:6:4.0160000324"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:6:4.0160000324") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:29.032000065"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:29.032000065") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:34.136000156"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:34.136000156") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:39.032000065"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:39.032000065") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:44.016000032"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:44.016000032") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:49.025000095"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:49.025000095") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:54.03399992"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:54.03399992") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:59.078000069"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:59.078000069") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:8:4.003000021"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:8:4.003000021") + 4), ])
plot_trip_data_small = rbind(plot_trip_data[914: 1078, ],
                             plot_trip_data[1234: 1248, ])
#      
# 

pal <- colorNumeric(
  palette = "Reds",
  domain = plot_trip_data$speed)

leaflet(plot_trip_data) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,  color = ~pal(speed),
             radius = 7, fillOpacity = 0.8,
             popup = paste("Speed: ", plot_trip_data$speed, "<br>",
                           "Algorithm: ", plot_trip_data$algorithm)) %>%
  addCircles(lng = ~plot_trip_data_small$longitude, lat = ~plot_trip_data_small$latitude, 
             weight = 1,  color = "dodgerblue", radius = 9, fillOpacity = 1,
             popup = paste("Speed: ", plot_trip_data_small$speed, "<br>",
                           "Algorithm: ", plot_trip_data_small$algorithm)) %>%
  addLegend("bottomright", pal = pal, values = plot_trip_data$speed, title = "Vehicle speed", opacity = 1)


















# visualize speed_lim2 in Columbus
plot_trip_data = all_data[all_data$trip_number == "0060DE81509044128A13F2035E055F2D00", ]

plot_trip_data = plot_trip_data %>% mutate(avg_latG = c(rollapply(latG, width = 10, mean, fill = NA)))

plot_trip_data$algorithm <- NA
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) > 10 & abs(plot_trip_data$latG) > .07] <- "Normal Turn"
plot_trip_data$algorithm[plot_trip_data$latG >= -.07 & plot_trip_data$latG < .07] <- "Normal Driving"
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) < 10 & abs(plot_trip_data$latG) > .07 & abs(plot_trip_data$avg_latG) < .055] <- "Lane Change"
plot_trip_data$algorithm[abs(plot_trip_data$ang_speed_gyro) < 10 & abs(plot_trip_data$latG) > .07 & abs(plot_trip_data$avg_latG) > .055] <- "Curve in the road"

plot_trip_data_small = rbind(plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:6:4.0160000324"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:6:4.0160000324") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:29.032000065"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:29.032000065") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:34.136000156"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:34.136000156") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:39.032000065"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:39.032000065") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:44.016000032"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:44.016000032") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:49.025000095"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:49.025000095") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:54.03399992"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:54.03399992") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:7:59.078000069"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:7:59.078000069") + 4), ],
                             plot_trip_data[which(plot_trip_data$timestmp_local == "2016-3-13 9:8:4.003000021"): (which(plot_trip_data$timestmp_local == "2016-3-13 9:8:4.003000021") + 4), ])
plot_trip_data_small = rbind(plot_trip_data[749: 1233, ],
                             plot_trip_data[1369: 1648, ])
#      
# 

pal <- colorNumeric(
  palette = "Reds",
  domain = plot_trip_data$speed)

leaflet(plot_trip_data) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,  color = ~pal(speed),
             radius = 7, fillOpacity = 0.8,
             popup = paste("Speed: ", plot_trip_data$speed, "<br>",
                           "Algorithm: ", plot_trip_data$algorithm)) %>%
  addCircles(lng = ~plot_trip_data_small$longitude, lat = ~plot_trip_data_small$latitude, 
             weight = 1,  color = "dodgerblue", radius = 9, fillOpacity = 1,
             popup = paste("Speed: ", plot_trip_data_small$speed, "<br>",
                           "Algorithm: ", plot_trip_data_small$algorithm)) %>%
  addLegend("bottomright", pal = pal, values = plot_trip_data$speed, title = "Vehicle speed", opacity = 1)
