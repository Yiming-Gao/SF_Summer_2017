
##################################### Some plot functions ################################

# a function that plots speed vs avg_speed in each trip
# x-axis labels
labels = data.frame(hour = floor(temp2$Time))
labels$minute = floor((temp2$Time - labels$hour) * 60)
labels$second = floor((temp2$Time - labels$hour - labels$minute / 60) * 3600)
labels$label = ifelse(labels$hour >=0 & labels$hour <12, "am", "pm")
labels$hour = ifelse(labels$label == "pm" & labels$hour != 12, labels$hour - 12, labels$hour)
temp2 = na.omit(cbind(temp2, labels))

# function 1
speed_plot_smooth <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$hour[plot_data_label_index], ":",
                             plot_data$minute[plot_data_label_index], ":",
                             plot_data$second[plot_data_label_index], " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    geom_smooth(aes(y = speed, colour = "speed"), se = FALSE) + 
    geom_smooth(aes(y = avg_speed, colour = "average speed"), se = FALSE) +
    geom_smooth(aes(y = speed_lim1, colour = "speed limit1"), se = FALSE) +
    geom_smooth(aes(y = speed_lim2, colour = "speed limit2"), se = FALSE) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Speed Type") + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))}

# Using this code for demonstration: speed_plot_smooth("17074D96C94C4CF19C82309E39539A1D00"),  speed_plot_smooth("008AB6EB7764422D94AA7747C713CB9E00")


# function 2
speed_plot_line <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  plot_data_label_index = ceiling(seq(1, nrow(plot_data), length.out = 20))
  plot_data_breaks = plot_data$Time[plot_data_label_index]
  plot_data_labels = (paste0(plot_data$hour[plot_data_label_index], ":",
                             plot_data$minute[plot_data_label_index], ":",
                             plot_data$second[plot_data_label_index], " ",
                             plot_data$label))[1: 20]
  
  ggplot(plot_data, aes(Time)) + 
    geom_line(aes(y = speed, colour = "speed"), size = 0.8) + 
    geom_line(aes(y = avg_speed, colour = "average speed"), size = 0.8) +
    geom_line(aes(y = speed_lim1, colour = "speed limit1"), size = 0.8) +
    geom_line(aes(y = speed_lim2, colour = "speed limit2"), size = 0.8) +
    ggtitle(paste("Trip number: ", trip_number)) + ylab("Speed") +
    labs(colour = "Speed Type") + 
    scale_x_continuous(breaks = plot_data_breaks, labels = plot_data_labels) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))}

# Using this code for demonstration: speed_plot_line("011F4F3D55BA444F83BC46954240923200"),  speed_plot_line("008AB6EB7764422D94AA7747C713CB9E00")



# function 3: a function to plot one trip on map
trip_plot <- function(trip_number) {
  plot_data = temp2[temp2$trip_number == trip_number, ]
  
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = plot_data$speed)
  
  factpal = colorFactor(sort(topo.colors(3)), plot_data$road_type)
  
  content = paste(sep = "<br/>", "Trip number: ", plot_data$trip_number)
  
  leaflet(plot_data) %>% addTiles() %>%
    addCircles(lng = ~plot_data$longitude, lat = ~plot_data$latitude, weight = 1,  color = ~factpal(road_type),
               radius = 7, fillOpacity = .8, group = "Predicted Road Type") %>%
    addCircles(lng = ~plot_data$longitude + 0.0001, lat = ~plot_data$latitude + 0.0001, weight = 1,  color = ~pal(speed),
               radius = 7, fillOpacity = 0.8, group = "Actual Speed") %>%
    addLegend("bottomright", pal = factpal, values =plot_data$road_type, title = "Road Type", opacity = 1) %>%
    addLegend("bottomright", pal = pal, values = plot_data$speed, title = "Speed", opacity = 1) %>%
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Actual Speed", "Predicted Road Type"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    
    # popup windows
    addPopups(plot_data$longitude[1], plot_data$latitude[1] + 0.005, 
              content, options = popupOptions(closeButton = TRUE))
}

# trip_plot("05ABA1169DCD43FD8B9FA43C3F55FE7700"), trip_plot("0060DE81509044128A13F2035E055F2D00")


# function 4 log = -83.0173, lat = 40.0043, 13 different trips passing by
# merge two datasets

traffic_den_rela <- function(log, lat) {
  plot_data = temp2[round(temp2$longitude, 3) == log & round(temp2$latitude, 3) == lat, ]
  sum = rep(0, length(unique(plot_data$trip_number)))
  
  for (i in 1: length(unique(plot_data$trip_number))){
    sum[i] = mean(plot_data[plot_data$trip_number == unique(plot_data$trip_number)[i], ]$Time)
  }
  
  sum = ifelse(sum >= 6 & sum <= 10, "6 am - 10 am (rush hour)",
               ifelse(sum > 10 & sum <16, "10 am - 4 pm",
                      ifelse(sum >= 16 & sum <= 20, "4 pm - 8 pm (rush hour)", "8 pm - 6am")))
  
  ggplot(data.frame(table(na.omit(sum)))[c(3, 1, 2, 4),], aes(x = Var1, y = Freq, color = Var1)) + 
    geom_bar(stat="identity", fill="white") + 
    geom_text(aes(label = Freq), vjust=1.6, color = "black", size = 3.5) +
    ggtitle(paste("Location: (", log, ", ", lat, ")")) + ylab("Frequency") + xlab("Time period") + 
    labs(colour = "Time period")
}

# traffic_den_rela(-83.036, 40.076)

# plot this location on map
leaflet() %>% addTiles() %>%
  addPopups(-83.036, 40.076, paste("Location: (", -83.036, ", ", 40.076, ")"),
            options = popupOptions(closeButton = FALSE))



## plot heatmap for SOM object
plotHeatMap <- function(som_model, data, variable=0){    
  # Plot a heatmap for any variable from the data set "data".
  # If variable is 0, an interactive window will be provided to choose the variable.
  # If not, the variable in "variable" will be plotted.
  
  require(dummies)
  require(kohonen)
  
  interactive <- TRUE
  
  while (interactive == TRUE){
    
    if (variable == 0){
      #show interactive window.
      color_by_var <- select.list(names(data), multiple=FALSE,
                                  graphics=TRUE, 
                                  title="Choose variable to color map by.")
      # check for user finished.
      if (color_by_var == ""){ # if user presses Cancel - we quit function        
        return(TRUE)
      }
      interactive <- TRUE
      color_variable <- data.frame(data[, color_by_var])
      
    } else {
      color_variable <- data.frame(data[, variable])
      color_by_var <- names(data)[variable]
      interactive <- FALSE
    }
    
    #if the variable chosen is a string or factor - 
    #Get the levels and ask the user to choose which one they'd like.
    
    if (class(color_variable[,1]) %in% c("character", "factor", "logical")){
      #want to spread this out into dummy factors - but colour by one of those.
      temp_data <- dummy.data.frame(color_variable, sep="_")
      chosen_factor <- select.list(names(temp_data), 
                                   multiple=FALSE,
                                   graphics=TRUE, 
                                   title="Choose level of variable for colouring")
      color_variable <- temp_data[, chosen_factor]
      rm(temp_data, chosen_factor)
      color_by <- color_variable
    } else {      
      #impute the missing values with the mean.
      color_variable[is.na(color_variable[,1]),1] <- mean(color_variable[,1], na.rm=TRUE)
      #color_by <- capVector(color_variable[,1])
      #color_by <- scale(color_by)  
      color_by <- color_variable[,1]
    }
    unit_colors <- aggregate(color_by, by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)
    plot(som_model, type = "property", property=unit_colors[,2], main=color_by_var, palette.name=coolBlueHotRed)    
  }
}
