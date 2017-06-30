setwd("/san-data/usecase/magnet_g/misc/PCA_DATA_VIS")
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
temp_0313 = left_join(temp_0313, temp_0313 %>% dplyr::select(trip_number, longitude, latitude) %>% 
                        group_by(trip_number) %>% 
                        distinct(longitude, latitude) %>%
                        group_by(longitude, latitude) %>%
                        summarise(traffic_den_day = n()), 
                      by = c("longitude" = "longitude", "latitude" = "latitude"))
# temp_0313[temp_0313$traffic_den != temp_0313$traffic_den_day, ]

# visualize traffic density on 03/13/2016
plot_data_0313 = temp_0313 %>% dplyr::select(longitude, latitude, traffic_den_day) %>% distinct(longitude, latitude, traffic_den_day)

pal <- colorNumeric(
  palette = "Reds",
  domain = plot_data_0313$traffic_den_day)

# 1b. map of traffic density
leaflet(plot_data_0313) %>% addTiles() %>%
  addCircles(lng = ~plot_data_0313[plot_data_0313$traffic_den_day > 1, ]$longitude, lat = ~plot_data_0313[plot_data_0313$traffic_den_day > 1, ]$latitude,
             weight = 1,  color = ~pal(plot_data_0313[plot_data_0313$traffic_den_day > 1, ]$traffic_den_day),
             radius = 7, fillOpacity = 1, group = "Traffic density > 1") %>%
  addCircles(lng = ~plot_data_0313[plot_data_0313$traffic_den_day == 1, ]$longitude, lat = ~plot_data_0313[plot_data_0313$traffic_den_day == 1, ]$latitude,
             weight = 1,  color = ~pal(plot_data_0313[plot_data_0313$traffic_den_day == 1, ]$traffic_den_day),
             radius = 7, fillOpacity = 0.3, group = "Traffic density = 1") %>%
  addLegend("bottomright", pal = pal, values = sort(unique(plot_data_0313$traffic_den_day)), title = "Traffic Density on 03/13/2016", opacity = 1) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Traffic density > 1", "Traffic density = 1"),
    options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Traffic density = 1")

# 2b. map of average speed
plot_data_0313 = temp_0313 %>% dplyr::select(longitude, latitude, avg_speed) %>% distinct(longitude, latitude, avg_speed)

pal <- colorNumeric(
  palette = "Reds",
  domain = plot_data_0313$avg_speed)

leaflet(plot_data_0313) %>% addTiles() %>%
  addCircles(lng = ~plot_data_0313$longitude, lat = ~plot_data_0313$latitude,
             weight = 1,  color = ~pal(avg_speed),
             radius = 7, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values = plot_data_0313$avg_speed, title = "Average Speed on 03/13/2016", opacity = 1)

################################ Plot each trip information ###############################
temp2 = all_columbus %>% dplyr::select(longitude, latitude, trip_number, lonG, speed, Hour, Minute, Second, traffic_den, avg_speed, speed_lim) 
temp2$Time = temp2$Hour + temp2$Minute / 60 + temp2$Second / 3600
temp2$label = ifelse(temp2$Hour >=0 & temp2$Hour <12, "am", "pm")
temp2$Hour = ifelse(temp2$label == "pm" & temp2$Hour != 12, temp2$Hour - 12, temp2$Hour)
temp2$Mode = as.factor(ifelse(temp2$lonG > 0, "Acceleration", "Deceleration"))

# 1di
speed_plot_smooth1 <- function(trip_number) {
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
    geom_smooth(aes(y = speed, colour = "Vehicle speed"), se = FALSE, span = 0.4) + 
    geom_smooth(aes(y = speed_lim, colour = "Predicted speed limit"), se = FALSE, span = 0.4) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    geom_smooth(aes(y = traffic_den * 10, colour = "Traffic density"), se = FALSE, span = 0.4) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.85, 0.8))}

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
    scale_y_continuous(name = "Traffic Density", breaks = seq(0, 25, 1)) + 
    geom_point(aes(y = traffic_den, color = Mode)) + 
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
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
    geom_smooth(aes(y = speed, colour = "Vehicle speed"), se = FALSE, span = 0.4) + 
    geom_smooth(aes(y = speed_lim, colour = "Predicted speed limit"), se = FALSE, span = 0.4) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    geom_smooth(aes(y = avg_speed, colour = "Average speed"), se = FALSE, span = 0.4) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.15, 0.8))}

# speed_plot_smooth2("17074D96C94C4CF19C82309E39539A1D00")

speed_plot_line2 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    geom_line(aes(y = speed, colour = "Vehicle speed"), size = 0.8) + 
    geom_line(aes(y = speed_lim, colour = "Predicted speed limit"), size = 0.8) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    geom_line(aes(y = avg_speed, colour = "Average speed"), size = 0.8) +
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.15, 0.8))}

# speed_plot_line2("17074D96C94C4CF19C82309E39539A1D00")

# 2dii
acceleration_plot2 <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$Hour[plot_data_label_index], ":",
                             plot_data$Minute[plot_data_label_index], ":",
                             round(plot_data$Second[plot_data_label_index], 0), " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    scale_y_continuous(name = "Average Speed") + 
    geom_smooth(aes(y = avg_speed), se = FALSE, span = 0.4) +
    geom_point(aes(y = avg_speed, color = Mode)) + 
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Variable") + 
    theme(panel.background = element_rect(fill = 'gray93'), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.15, 0.8))}

# acceleration_plot2("17074D96C94C4CF19C82309E39539A1D00")
