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

dataset = separate(all_data,
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
all_points$road_type = ifelse(all_points$road_type == 2, "Business District", 
                              ifelse(all_points$road_type == 1, "Residential Road", "Freeway"))

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
all_temp = left_join(dataset, all_points, by = c("longitude" = "longitude", "latitude" = "latitude")) 
all_temp$timestmp_local <- all_temp$Hour_editing_needed <- all_temp$stop_ind <- all_temp$ratio <- NULL



######################### Trips per day #########################
temp_0313 = all_temp[all_temp$Date == "2016-03-13", ]
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

leaflet((temp_0313 %>% dplyr::select(longitude, latitude, traffic_den_day) %>% distinct(longitude, latitude, traffic_den_day))) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,  color = ~pal(traffic_den_day),
             radius = 7, fillOpacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values = sort(unique(points$traffic_den)), title = "Traffic Density on 03/13/2016", opacity = 1)





temp_0313 %>% dplyr::select(longitude, latitude, traffic_den_day) %>% distinct(longitude, latitude, traffic_den_day) 
