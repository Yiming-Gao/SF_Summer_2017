# With_mutate <- function() {
library(opencage)
library("RDSTK") # coordinates2politics() function

# Classify road type ONLY based on average speed
points$turning = points$road_type = character(nrow(points))

points[which(points$avg_speed >= 0 & points$avg_speed <= 30), ]$road_type = "Residential Road"
points[which(points$avg_speed > 30 & points$avg_speed <= 45), ]$road_type = "Business District"
points[which(points$avg_speed > 45), ]$road_type = "Freeway"

# visualize road type
factpal = colorFactor(topo.colors(3), points$road_type)

leaflet(points) %>% addTiles() %>%
  addCircles(lng = ~points$longitude, lat = ~points$latitude, weight = 1,  color = ~factpal(road_type),
             radius = 25, fillOpacity = .8) %>%
  addLegend("bottomright", pal = factpal, values = points$road_type, title = "Road Type", opacity = 1)





for (i in 1: nrow(points)) {
  turning_table = table(na.omit(temp[temp$longitude == points[i, 1] & temp$latitude == points[i, 2], ]$turning))
  points[i, ]$turning = names(turning_table[which.max(turning_table)])
}

# binary coding for k-means clustering
points$normal_turn = ifelse(points$turning == "Normal Turn", 1, 0)
points$curve = ifelse(points$turning == "Curve in the road", 1, 0)
points$lane_change = ifelse(points$turning == "Lane Change", 1, 0)



# road type classification based on k-means clustering, using (longitude, latitude and ) average speed, traffic density
# May try different variables
set.seed(522)
kmeans = kmeans(points[, c(3, 4)], centers = 3, iter.max = 20, nstart = 20)
points = na.omit(points) # k-means doesn't handle missing values
points$road_type = kmeans$cluster
points$road_type = ifelse(points$road_type == 1, "Business District", ifelse(points$road_type == 2, "Residential Road", "Freeway"))


# visualize the road type data
factpal = colorFactor(sort(topo.colors(3)), points$road_type)

leaflet(points) %>% addTiles() %>%
  addCircles(lng = ~points$longitude, lat = ~points$latitude, weight = 1,  color = ~factpal(road_type),
             radius = 20, fillOpacity = .8) %>%
  addLegend("bottomright", pal = factpal, values = points$road_type, title = "Road Type", opacity = 1)

#######################################################################################################################





########################################## Add turning type ##########################################

temp3 = temp2
temp3$Normal_turn = ifelse(temp3$turning == "Normal Turn", 1, 0)
temp3$Curve = ifelse(temp3$turning == "Curve in the road", 1, 0)
temp3$Lane_change = ifelse(temp3$turning == "Lane Change", 1, 0)
temp3$turning = NULL

# Use PCA to handle categorical data
set.seed(522)
temp3 = na.omit(temp3) # k-means doesn't handle missing values
temp3$road_type = kmeans(temp3[, c(6, 7)], centers = 3, iter.max = 20, nstart = 20)$cluster
temp3$road_type = ifelse(temp3$road_type == 3, "Freeway", ifelse(temp3$road_type == 2, "Business District", "Residential Road"))

# visualize the road type data
factpal = colorFactor(sort(topo.colors(3)), temp3$road_type)

leaflet(temp3[1: 50000, ]) %>% addTiles() %>%
  addCircles(lng = ~temp3$longitude, lat = ~temp3$latitude, weight = 1,  color = ~factpal(road_type),
             radius = 10, fillOpacity = .8) %>%
  addLegend("bottomright", pal = factpal, values = temp3$road_type, title = "Road Type", opacity = 1)












