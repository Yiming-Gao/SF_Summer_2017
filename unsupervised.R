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
  theme_minimal()


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

# over_speed("0441649843F94FE6ADC5E76F2FAD6CB900")
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
temp2$driving_type = ifelse(temp2$percentage <= quantile(all_distr[all_distr$percentage != 0, ]$percentage, 0.5), 0, 1)



# Classification by first 11 PCs
# datas3 contains different trip numbers as well as their average PCA scores and corresponding response 0/1 based on our previous classification
datas3 = (data.frame(trip_number = na.omit(new_datas2)$trip_number, prin_comp$x[, 1:11])) %>% 
  group_by(trip_number) %>% mutate(PC1 = mean(PC1), PC2 = mean(PC2), PC3 = mean(PC3), PC4 = mean(PC4),
                                   PC5 = mean(PC5), PC6 = mean(PC6), PC7 = mean(PC7), PC8 = mean(PC8),
                                   PC9 = mean(PC9), PC10 = mean(PC10), PC11 = mean(PC11)) %>%
  distinct(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11)

datas3 = left_join(datas3, all_distr, by = c("trip_number" = "trip_number"))
datas3$driving_type = as.factor(ifelse(datas3$percentage <= quantile(all_distr[all_distr$percentage != 0, ]$percentage, 0.5), 0, 1))


set.seed(613)
datas3$driving_type = kmeans(datas3[, 1:11], centers = 2, iter.max = 20, nstart = 20)$cluster

# type = 1, speed_plot_line("0441649843F94FE6ADC5E76F2FAD6CB900"), potential dangerous driver 

################################## Classification with SVM ################################
# train-test split
library(kernlab)
set.seed(612)
datas3_idx = createDataPartition(datas3$driving_type, p = 0.75, list = FALSE)
datas3_trn = datas3[datas3_idx, ]
datas3_tst = datas3[-datas3_idx, ]
xyplot(PC2 ~ PC1, datas3_trn, groups = datas3$driving_type, pch = 20, auto.key = TRUE) # Visualize first 2 PCs, not linearly separable

# accuracy function
svm_acc <- function(actual, predicted) {
  mean(actual == predicted)
}

# Tune with caret package
# Linear Kernel
svm_grid =  expand.grid(C = c(2 ^ (-5:5)))
svm_control = trainControl(method = "cv", number = 5, 
                           returnResamp = "all", verbose = FALSE)

lin_svm_fit = train(driving_type ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn, 
                    method = "svmLinear",
                    trControl = svm_control, tuneGrid = svm_grid)

# train and test accuracy
svm_acc(actual = datas3_trn$driving_type, predicted = predict(lin_svm_fit, datas3_trn))
svm_acc(actual = datas3_tst$driving_type, predicted = predict(lin_svm_fit, datas3_tst))

lin_svm_fit = svm(driving_type ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn,
                  kernel = 'linear', cost = lin_svm_fit$bestTune$C)
plot(lin_svm_fit, data = datas3_trn, PC2 ~ PC1, slice = list(PC3 = 3),
     svSymbol = 1, dataSymbol = 2, symbolPalette = topo.colors(4)[c(1, 4)],
     color.palette = cm.colors)

# Polynomial Kernel
poly_svm_fit = train(driving_type ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn, 
                     method = "svmPoly",
                     trControl = svm_control)

# train and test accuracy
svm_acc(actual = datas3_trn$driving_type, predicted = predict(poly_svm_fit, datas3_trn))
svm_acc(actual = datas3_tst$driving_type, predicted = predict(poly_svm_fit, datas3_tst))

poly_svm_fit = svm(driving_type ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = datas3_trn,
                   kernel = 'polynomial', scale = poly_svm_fit$bestTune$scale, degree = poly_svm_fit$bestTune$degree, cost = poly_svm_fit$bestTune$C)
plot(poly_svm_fit, data = datas3_trn, PC2 ~ PC1, slice = list(PC3 = 3),
     svSymbol = 1, dataSymbol = 2, symbolPalette = topo.colors(4)[c(1, 4)],
     color.palette = cm.colors)






