# Import Libraries
library(ggplot2)
library(forecast)
library(caTools)
library(randomForest) # Change according to needs

# Reading the dataset
dataset = read.csv('dinesh_data.csv')

# Taking care of missing data
dataset$PM2.5 = ifelse(is.na(dataset$PM2.5),
                       ave(dataset$PM2.5, FUN = function(x) mean(x, na.rm = TRUE)),
                       dataset$PM2.5)
dataset$Temp = ifelse(is.na(dataset$Temp),
                      ave(dataset$Temp, FUN = function(x) mean(x, na.rm = TRUE)),
                      dataset$Temp)
dataset$WindSpeed = ifelse(is.na(dataset$WindSpeed),
                           ave(dataset$WindSpeed, FUN = function(x) mean(x, na.rm = TRUE)),
                           dataset$WindSpeed)
dataset$WindDirection = ifelse(is.na(dataset$WindDirection),
                               ave(dataset$WindDirection, FUN = function(x) mean(x, na.rm = TRUE)),
                               dataset$WindDirection)
dataset$SolarRadiation = ifelse(is.na(dataset$SolarRadiation),
                                ave(dataset$SolarRadiation, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$SolarRadiation)
dataset$RelativeHumidity = ifelse(is.na(dataset$RelativeHumidity),
                                  ave(dataset$RelativeHumidity, FUN = function(x) mean(x, na.rm = TRUE)),
                                  dataset$RelativeHumidity)

# Timestamp conversion
dataset$Time = as.POSIXct(strptime(dataset$Time, format ="%d-%m-%Y %H:%M"))

# Cleaning pollution index values
clean_y = tsclean(ts(dataset$PM2.5))

# Plot Comparison with cleaned values
ggplot() +
  geom_path(aes(x = dataset$Time, y = dataset$PM2.5),
            colour = 'red',
            group = 1) +
  geom_path(aes(x = dataset$Time, y = clean_y),
            colour = 'blue',
            group = 1) +
  ggtitle('Comparison with cleaned values') +
  xlab('Date and Time') +
  ylab('AQI') 

# Replace y with cleaned matrix
dataset$PM2.5 = clean_y
remove(clean_y)

# Renewed plot of y
ggplot() +
  geom_path(aes(x = dataset$Time, y = dataset$PM2.5),
            colour = 'red',
            group = 1) +
  ggtitle('Cleaned Observations') +
  xlab('Date and Time') +
  ylab('AQI') 

# Adding previous hour pm2.5 concentration column
temp = dataset$PM2.5[1:(nrow(dataset)-1)]
dataset = dataset[-1,]
dataset$prevPM2.5 = temp
remove(temp)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor = randomForest(formula = PM2.5 ~ Temp + Hour + WindSpeed + WindDirection 
                         + SolarRadiation + RelativeHumidity 
                          + prevPM2.5,
                         data = training_set,
                         ntree = 200)
summary(regressor)

# Predicting Test Set Results
y_pred = predict(regressor, newdata = test_set)

# Comparing predicted and actual values of test set
ggplot() +
  geom_line(aes(x = test_set$Time, y = test_set$PM2.5),
            linetype = 2) +
  geom_line(aes(x = test_set$Time, y = y_pred)) +
  xlab('Date and Time') +
  ylab('PM2.5') 


# Mean error
error = abs((y_pred - test_set$PM2.5)/test_set$PM2.5)
error = mean(error)
error = error*100

# Root mean square error
rmse = (y_pred-test_set$PM2.5)^2
rmse = sum(rmse)
rmse = sqrt(rmse/nrow(test_set))