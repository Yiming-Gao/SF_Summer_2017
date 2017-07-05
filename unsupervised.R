library(dummies)
library(factoextra)

# datas2 contains raw data without rounding, each row is a trip at a second
datas2 = na.omit(left_join(temp2 %>% dplyr::select(trip_number, Time, traffic_den, avg_speed, road_type, speed_lim1, speed_lim2), 
                           datas1 %>% 
                             mutate(Time = as.numeric(substr(.$Hour_editing_needed, 11, 12)) + Minute / 60 + Second / 3600) %>%  
                             filter(.$latitude > latitude_min & .$latitude < latitude_max & .$longitude < longitude_max & .$longitude > longitude_min),
                           by = c("trip_number" = "trip_number", "Time" = "Time")))


# create 5-second lag speed
datas2 = as.data.frame(datas2 %>%
                         group_by(trip_number) %>%
                         mutate(lag_speed = lag(speed, 5)))

# Since road_type, algorithm are categorical, we convert them into numeric using one hot encoding
#create a dummy data frame
new_datas2 = cbind(dummy.data.frame(datas2, names = c("road_type", "algorithm")), datas2[c("road_type", "algorithm")])

prin_comp = prcomp(na.omit(new_datas2[, -which(names(new_datas2) %in% 
                                                 c("trip_number", "timestmp_local", "Hour_editing_needed", "Date",
                                                   "speed_lim1", "Hour", "Minute", "Second", "latitude",
                                                   "longitude", "ratio", "road_type", "algorithm"))]), scale. = T) # normalize the variables to have standard deviation equals to 1
prin_comp$rotation[1:5, 1:4] # returns PC loadings: first 3 principal components and first 5 rows
dim(prin_comp$x) # matrix x has the pc score vectors in a 315403*18 dimension

# plot the resultant principal components
# biplot(prin_comp, scale = 0) # scale = 0 ensures that arrows are scaled to represent the loadings


# Eigenvalues
eig = (prin_comp$sdev)^2

# Variances 
variance = eig / sum(eig)

# Cumulative variances
cumvar = cumsum(variance)
prin_comp_active = data.frame(eig = eig, variance = variance, cumvariance = cumvar)

# scree plot using factoextra package
fviz_screeplot(prin_comp, addlabels = TRUE, hjust = -0.3, linecolor = "firebrick1", ncp = 22)

# comulative scree plot
plot(cumsum(variance), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", main = "Cumulative Scree Plot",
     type = "b", col = "dodgerblue2", xaxt = "n", yaxt  = "n")
axis(1, at = seq(1, 22), las = 1)
axis(2, at = seq(0, 1, 0.1), las = 1)
grid()



########### Coordinates of variables on the principal components
# Helper function:
# Correlation between variables and principal components
var_cor_func <- function(var.loadings, comp.sdev) {
  var.loadings * comp.sdev
}

# Variable correlation / coordinates
loadings = prin_comp$rotation
sdev = prin_comp$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
head(var.coord[, 1:5])

# Contributions of the variables to the principal components
var.cos2 = var.coord^2
comp.cos2 = apply(var.cos2, 2, sum)
contrib = function(var.cos2, comp.cos2) {var.cos2 * 100 / comp.cos2}
var.contrib = t(apply(var.cos2, 1, contrib, comp.cos2))

# Highlight the most important (i.e. contributing variables)
fviz_pca_var(prin_comp, col.var = "contrib") +
  scale_color_gradient2(low = "white", mid = "blue", high = "red", midpoint = 5) + 
  theme_minimal() # PC1 and PC2


### Qualitative variables: road_type, algorithm
# Return the coordinates of a group levels
# x: coordinate of individuals on x axis
# y: coordinate of individuals on y axis
get_coord_quali <- function(x, y, groups) {
  data.frame(x = tapply(x, groups, mean),
             y = tapply(y, groups, mean))}

# calculate the coordinates on components 1 and 2
get_coord_quali(prin_comp$x[, 1], prin_comp$x[, 2], groups = new_datas2[1: dim(prin_comp$x)[1], ]$road_type)


######################## a function to calculate the percentage of a driver drove over speed #################
over_speed <- function(trip_number) {
  data = temp2[temp2$trip_number == trip_number, ]
  # cat(paste("Trip Number:", trip_number),
  #     paste("Percentage of driving over speed limit:", round(mean(data$speed > data$speed_lim2), 4)),
  #     sep = "\n")
  return(mean(data$speed > data$speed_lim2))
}

# over_speed("0440E235DB634CBBA9CFE4D2FCC70A2400")
# look at the overall over speed distribution 
all_distr = setDT(data.frame(sapply(unique(temp2$trip_number), over_speed)), keep.rownames = TRUE)[]
colnames(all_distr) = c("trip_number", "percentage")
ggplot(data = all_distr, aes(percentage)) + 
  geom_histogram(breaks = seq(0, 1, by = 0.04),
                 col = "orangered1",
                 aes(fill = ..count..),
                 alpha = .5) + 
  theme(panel.background = element_rect(fill = 'gray93')) + 
  geom_vline(xintercept = quantile(all_distr[all_distr$percentage != 0, ]$percentage, 0.5), colour = "orangered1", linetype = "twodash", size = 0.6) +
  labs(title = "Histogram for Over Speed Distribution") +
  labs(x = "Percentage", y = "Count") +
  annotate("text", x = 0.55, y = 45, label = paste("Median (excluding zero values):", quantile(all_distr[all_distr$percentage != 0, ]$percentage, 0.5)), colour = "orangered1", size = 4)

temp2 = left_join(temp2, all_distr, by = c("trip_number" = "trip_number"))
temp2$over_speed = ifelse(temp2$percentage <= quantile(all_distr[all_distr$percentage != 0, ]$percentage, 0.5), 0, 1)



# Classification by first 11 PCs
# datas3 contains different trip numbers as well as their average PCA scores and corresponding response 0/1 based on our previous classification
datas3 = (data.frame(trip_number = na.omit(new_datas2)$trip_number, prin_comp$x[, 1:11])) %>% 
  group_by(trip_number) %>% mutate(PC1 = mean(PC1), PC2 = mean(PC2), PC3 = mean(PC3), PC4 = mean(PC4),
                                   PC5 = mean(PC5), PC6 = mean(PC6), PC7 = mean(PC7), PC8 = mean(PC8),
                                   PC9 = mean(PC9), PC10 = mean(PC10), PC11 = mean(PC11)) %>%
  distinct(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11)

datas3 = left_join(datas3, all_distr, by = c("trip_number" = "trip_number"))
datas3$over_speed = as.factor(ifelse(datas3$percentage <= quantile(all_distr[all_distr$percentage != 0, ]$percentage, 0.5), 0, 1))


set.seed(613)
datas3$over_speed = kmeans(datas3[, 1:11], centers = 2, iter.max = 20, nstart = 20)$cluster

# type = 1, speed_plot_line("0441649843F94FE6ADC5E76F2FAD6CB900"), potential dangerous driver 

################################## Classification models ################################
# train-test split
library(kernlab)
library(gridExtra)
set.seed(612)
datas3_idx = createDataPartition(datas3$over_speed, p = 0.75, list = FALSE)
datas3_trn = datas3[datas3_idx, ]
datas3_tst = datas3[-datas3_idx, ]

p1 = ggplot(datas3_trn, aes(PC1, PC2, color = over_speed))+ geom_point(size = 0.8) + 
  scale_colour_manual(values = c("dodgerblue2", 'orangered1')) + theme(legend.position = "left")
p2 = ggplot(datas3_trn, aes(PC2, PC3, color = over_speed))+ geom_point(size = 0.8) + 
  scale_colour_manual(values = c("dodgerblue2", 'orangered1')) + theme(legend.position = "left")
p3 = ggplot(datas3_trn, aes(PC1, PC3, color = over_speed))+ geom_point(size = 0.8) + 
  scale_colour_manual(values = c("dodgerblue2", 'orangered1')) + theme(legend.position = "left")
p4 = ggplot(datas3_trn, aes(PC1, PC4, color = over_speed))+ geom_point(size = 0.8) + 
  scale_colour_manual(values = c("dodgerblue2", 'orangered1')) + theme(legend.position = "left")
grid.arrange(p1, p2, p3, p4, ncol = 2)

# very ugly 3d plot
with(datas3_trn, {
  s3d <- scatterplot3d(PC1, PC2, PC3,        # x y and z axis
                       color = ifelse(datas3_trn$over_speed == 0, "dodgerblue2", "orangered1"), pch = c(16, 17)[as.numeric(datas3_trn$over_speed)],
                       scale.y = 1.2, 
                       main = "Driving Type",
                       xlab = "PC1",
                       ylab = "PC2",
                       zlab = "PC3")
  angle = 30
  s3d.coords = s3d$xyz.convert(PC1, PC2, PC3)
  # add the legend
  legend("bottom", bty = "n", cex = 1, title = "Type of Drivers", c("Safe", "Dangerous"),
         fill = c("dodgerblue2", "orangered1"), inset = -0.2, xpd = TRUE, horiz = TRUE)
})


# accuracy function
svm_acc <- function(actual, predicted) {
  mean(actual == predicted)
}

# Tune with caret package
# Linear Kernel
svm_grid =  expand.grid(C = c(2 ^ (-5:5)))
svm_control = trainControl(method = "cv", number = 5, 
                           returnResamp = "all", verbose = FALSE)

lin_svm_fit = train(over_speed ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn, 
                    method = "svmLinear",
                    trControl = svm_control, tuneGrid = svm_grid)

# train and test accuracy
svm_acc(actual = datas3_trn$over_speed, predicted = predict(lin_svm_fit, datas3_trn))
svm_acc(actual = datas3_tst$over_speed, predicted = predict(lin_svm_fit, datas3_tst))

lin_svm_fit = svm(over_speed ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn,
                  kernel = 'linear', cost = lin_svm_fit$bestTune$C)
plot(lin_svm_fit, data = datas3_trn, PC2 ~ PC1, slice = list(PC3 = 3),
     svSymbol = 1, dataSymbol = 2, symbolPalette = topo.colors(2),
     color.palette = terrain.colors)

# Polynomial Kernel
poly_svm_fit = train(over_speed ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn, 
                     method = "svmPoly",
                     trControl = svm_control)

# train and test accuracy
svm_acc(actual = datas3_trn$over_speed, predicted = predict(poly_svm_fit, datas3_trn))
svm_acc(actual = datas3_tst$over_speed, predicted = predict(poly_svm_fit, datas3_tst))

poly_svm_fit = svm(over_speed ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn,
                   kernel = 'polynomial', scale = poly_svm_fit$bestTune$scale, degree = poly_svm_fit$bestTune$degree, cost = poly_svm_fit$bestTune$C)
plot(poly_svm_fit, data = datas3_trn, PC2 ~ PC1, slice = list(PC3 = 3),
              svSymbol = 1, dataSymbol = 2, symbolPalette = topo.colors(2),
              color.palette = terrain.colors)


# predict a trip
trip_pred <- function(trip_number) {
  test = new_datas2[new_datas2$trip_number == trip_number, -which(names(new_datas2) %in% c("trip_number", "timestmp_local", "Hour_editing_needed", "Date",
                                                                                           "speed_lim1", "Hour", "Minute", "Second", "latitude",
                                                                                           "longitude", "ratio", "road_type", "algorithm"))]
  
  test = na.omit(data.frame(predict(prin_comp, newdata = test))[, 1:11]) %>% mutate(PC1 = mean(PC1), PC2 = mean(PC2), PC3 = mean(PC3), PC4 = mean(PC4),
                                                                                    PC5 = mean(PC5), PC6 = mean(PC6), PC7 = mean(PC7), PC8 = mean(PC8),
                                                                                    PC9 = mean(PC9), PC10 = mean(PC10), PC11 = mean(PC11)) %>%
    distinct(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11)
  
  cat(paste("Trip Number:", trip_number),
      paste("Over speed proportion:", round(over_speed(trip_number), 4)),
      paste("Prediction:", ifelse(predict(lin_svm_fit, as.data.frame("trip_number" = "17A4B1EA911641428193238381D4F36400", test)) == 0, "Normal driving", "Overspeed")),
      sep = "\n")
}
# trip_pred("18241303B71A4A24863DB05AE8EB3CBB00")


################################### SOM ################################
library(kohonen)
# https://www.slideshare.net/shanelynn/2014-0117-dublin-r-selforganising-maps-for-customer-segmentation-shane-lynn
data_som = na.omit(left_join(new_datas2, all_distr, by = c("trip_number" = "trip_number"))[, which(names(left_join(new_datas2, all_distr, by = c("trip_number" = "trip_number"))) %in% 
                                        c("latG", "lonG", "speed", "ang_speed_gyro", "lon_delta",
                                          "stop_ind", "percentage"))])[seq(1, 100001, length.out = 2000), ]
data_som_matrix = as.matrix(scale(data_som))
som_grid = somgrid(xdim = 20, ydim = 20, topo = "hexagonal")
som_model = som(data_som_matrix, grid = som_grid, rlen = 100, alpha = c(0.05, 0.01), keep.data = TRUE)
coolBlueHotRed <- function(n, alpha = 1) { rainbow(n, end = 4/6, alpha = alpha)[n:1] }
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
plot(som_model, type = "changes")
plot(som_model, type = "count", palette.name = coolBlueHotRed)
plot(som_model, type = "dist.neighbours", palette.name = coolBlueHotRed)
plot(som_model, type = "codes")
plot(som_model, type = "property", property = som_model$codes[[1]][, 3], main = "speed", palette.name = coolBlueHotRed)
# unscaled plot
plotHeatMap(som_model, data = data_som)

## use hierarchical clustering to cluster
som_cluster = kmeans(dist(som_model$codes[[1]][, 1:6]), centers = 2)$cluster # exclude percentage
# plot these results:
plot(som_model, type = "mapping", bgcol = c("#BFFFFFFF", "#FF9FFFFF")[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster, lwd = 3)




################################## More clustering ################################
# Merge additional variables
all_columbus = left_join(dataset, all_points, by = c("longitude" = "longitude", "latitude" = "latitude")) 
all_columbus$timestmp_local <- all_columbus$Hour_editing_needed <- NULL

prin_comp = prcomp(na.omit(all_columbus[all_columbus$Date == "2016-03-13", -which(names(all_columbus) %in% 
                                                                                    c("trip_number", "Date", "Hour", "Minute", "Second", "latitude",
                                                                                      "longitude", "ratio", "road_type", "algorithm", "speed_lim"))]), scale. = T) # normalize the variables to have standard deviation equals to 1
prin_comp$rotation[1:5, 1:4] # returns PC loadings: first 3 principal components and first 5 rows
dim(prin_comp$x) # matrix x has the pc score vectors in a 165266 * 8 dimension

# plot the resultant principal components
# biplot(prin_comp, scale = 0) # scale = 0 ensures that arrows are scaled to represent the loadings


# Eigenvalues
eig = (prin_comp$sdev)^2

# Variances 
variance = eig / sum(eig)

# Cumulative variances
cumvar = cumsum(variance)
prin_comp_active = data.frame(eig = eig, variance = variance, cumvariance = cumvar)

# scree plot using factoextra package
fviz_screeplot(prin_comp, addlabels = TRUE, hjust = -0.3, linecolor = "firebrick1", ncp = 22)

# comulative scree plot
plot(cumsum(variance), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", main = "Cumulative Scree Plot",
     type = "b", col = "dodgerblue2", xaxt = "n", yaxt  = "n")
axis(1, at = seq(1, 22), las = 1)
axis(2, at = seq(0, 1, 0.1), las = 1)
grid()



########### Coordinates of variables on the principal components
# Helper function:
# Correlation between variables and principal components
var_cor_func <- function(var.loadings, comp.sdev) {
  var.loadings * comp.sdev
}

# Variable correlation / coordinates
loadings = prin_comp$rotation
sdev = prin_comp$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
head(var.coord[, 1:5])

# Contributions of the variables to the principal components
var.cos2 = var.coord^2
comp.cos2 = apply(var.cos2, 2, sum)
contrib = function(var.cos2, comp.cos2) {var.cos2 * 100 / comp.cos2}
var.contrib = t(apply(var.cos2, 1, contrib, comp.cos2))

# Highlight the most important (i.e. contributing variables)
fviz_pca_var(prin_comp, col.var = "contrib") +
  scale_color_gradient2(low = "white", mid = "blue", high = "red", midpoint = 5) + 
  theme_minimal() # PC1 and PC2



# hierarchical clustering
hc = hclust(dist(prin_comp$x), method = "complete")
cut = cutree(hc , 3)
ColorDendrogram(iris_hc, y = iris_cut,
                labels = names(iris_cut),
                main = "Iris, Complete Linkage",
                branchlength = 1.5)
