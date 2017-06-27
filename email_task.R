setwd("/san-data/usecase/magnet_g/misc/PCA_DATA_VIS")
all_data = fread("Timed_big_data.csv", nrows = 10000) %>%
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
