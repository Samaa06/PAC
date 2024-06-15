data = read.csv('data/carpriceprediction/analysisData.csv')
scoringData = read.csv('data/carpriceprediction/scoringData.csv')
#pred = predict(model,newdata=scoringData)
#submissionFile = data.frame(id = scoringData$id, price = pred)
#write.csv(submissionFile, 'submission1_test.csv',row.names = F)

library(caret)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(forcats)
library(corrplot)
library(ggcorrplot)
library(randomForest)
library(ipred)
library(gbm)
library(xgboost)


#glimpse which columns have NULLs
sapply(data, function(x) sum(is.na(x)))


## GRAPHICS AND PLOTS ##

ggplot(data, aes(x = price)) +
  geom_density(fill = "skyblue", color = "blue") +
  labs(title = "Car Price Distribution Plot") +
  theme_minimal()

ggplot(data, aes(y = price)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Car Price Spread") +
  theme_minimal()

which.max(data$price)

options(repr.plot.width = 20, repr.plot.height = 10)

# Create the count plot
plot <- ggplot(data, aes(x = make_name)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25) +
  labs(title = 'Count of Car based on Manufacturer', x = 'Manufacturer', y = 'Count of Car')

print(plot)

numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data)

# Create the correlation heatmap
ggcorrplot(correlation_matrix, 
           type = "lower",
           show.diag = FALSE,
           colors = c("red", "white", "darkgreen"),
           title = "Correlation Heat Map")

ggplot(data, aes(x = mileage, y = price)) +
  geom_point(aes(color = fuel_type), alpha = 0.5) +  # Color points by fuel type
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "Price vs. Mileage by Fuel Type", x = "Mileage", y = "Price") +
  theme_minimal()

ggplot(data, aes(x = price)) +
  geom_histogram(bins = 80, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Car Prices", x = "Price", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = seller_rating)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Seller Ratings", x = "Seller Rating", y = "Frequency")

avg_horsepower_by_engine <- data %>% 
  group_by(engine_type) %>% 
  summarize(Average_Horsepower = mean(horsepower, na.rm = TRUE))

ggplot(avg_horsepower_by_engine, aes(x = Average_Horsepower, y = engine_type, fill = engine_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Horsepower by Engine Type", x = "Engine Type", y = "Average Horsepower")




## DATA CLEANING ##

#unique(data$fuel_type)
#replace blanks in trim_name, fuel_type, power, torque, transmission, transmission_display
#wheel_system, wheel_system_display, engine_type, description, major_options
#fleet, frame_damaged, franchise_make, has_accidents, isCab, is_cpo, salvage

#replacing NULLs in numeric columns with the mean and NULLs in char columns with mode
for (i in 1:ncol(data)) {
  if (is.numeric(data[, i])) {
    data[is.na(data[, i]), i] <- mean(data[, i], na.rm = TRUE)
  } else {
    data[is.na(data[, i]), i] <- names(sort(table(data[, i]), decreasing = TRUE))[1]
  }
}
sum(is.na(data)) 

#make_name
summary(data$make_name)
sum(is.na(data$make_name))
data$make_name = as.factor(data$make_name)
# Create dummy variables
#dummies <- dummyVars("~ make_name", data = data, fullRank = FALSE)
# Transform data to include dummies
#make_transformed <- predict(dummies, newdata = data)
# Convert to data frame if it's not one and view the head
#make_transformed<- as.data.frame(make_transformed)
#head(make_transformed)
#data <- cbind(data, make_transformed)

#model_name
sum(is.na(data$model_name))
data$model_name = as.factor(data$model_name)
# Create dummy variables
#dummies <- dummyVars("~ make_name", data = data, fullRank = FALSE)
# Transform data to include dummies
#model_transformed <- predict(dummies, newdata = data)
# Convert to data frame if it's not one and view the head
#model_transformed<- as.data.frame(model_transformed)
#head(model_transformed)
#data <- cbind(data, model_transformed)

#body_type
sum(is.na(data$body_type))
data$body_type = as.factor(data$body_type)
# Create dummy variables
#dummies <- dummyVars("~ body_type", data = data, fullRank = FALSE)
# Transform data to include dummies
#body_transformed <- predict(dummies, newdata = data)
# Convert to data frame if it's not one and view the head
#body_transformed<- as.data.frame(body_transformed)
#head(body_transformed)
#data <- cbind(data, body_transformed)

#trim_name
trim_counts <- table(data$trim_name)
mode_trim <- names(trim_counts)[which.max(trim_counts)]
data$trim_name[data$trim_name == ""] <- mode_trim
sum(data$trim_name == "")
data$trim_name = as.factor(data$trim_name)

#fuel type
#Add'Gasoline' when fuel_tank_volume_gallons > 0 and fuel_type is blank
data$fuel_type <- ifelse(data$fuel_tank_volume_gallons > 0 & data$fuel_type == "",
                         "Gasoline", data$fuel_type)
#Add 'Electric' when fuel_tank_volume_gallons = 0 and fuel_type is blank
data$fuel_type <- ifelse(data$fuel_tank_volume_gallons == 0 & data$fuel_type =="",
                         "Electric", data$fuel_type)
sum(data$fuel_type == "")
data$fuel_type = as.factor(data$fuel_type)


# Calculate average fuel tank volume for Gasoline
avg_volume <- mean(data$fuel_tank_volume_gallons[data$fuel_type == "Gasoline"], na.rm = TRUE)
data$fuel_tank_volume_gallons[data$fuel_tank_volume_gallons %in% c(NA, "") & data$fuel_type == "Gasoline"] <- avg_volume
# Sub NA or blank values with 0 for Electric
data$fuel_tank_volume_gallons[data$fuel_type == "Electric" & (is.na(data$fuel_tank_volume_gallons) | data$fuel_tank_volume_gallons == "")] <- 0
sum(data$fuel_tank_volume_gallons == "")


#power
data$RPM <- (sub(".*@ (.+?) RPM", "\\1", data$power)) # Extract everything until RPM 
# Replace NA values with the mean RPM
data$RPM <- as.numeric(as.character(data$RPM))
data$RPM[is.na(data$RPM)] <- mean(data$RPM, na.rm = TRUE)
sum(data$RPM == "")
sum(is.na(data$RPM))

#torque
data$torque = as.factor(data$torque)


#transmission
transmission_counts <- table(data$transmission)
mode_transmission <- names(transmission_counts)[which.max(transmission_counts)]
data$transmission[data$transmission == ""] <- mode_transmission
sum(data$transmission == "")
data$transmission = as.factor(data$transmission)


#transmission_display
transmission_display_counts <- table(data$transmission_display)
mode_transmission_display <- names(transmission_display_counts)[which.max(transmission_display_counts)]
data$transmission_display[data$transmission_display == ""] <- mode_transmission_display
sum(data$transmission_display == "")
data$transmission_display = as.factor(data$transmission_display)


#wheel system
wheel_system_counts <- table(data$wheel_system)
mode_wheel_system <- names(wheel_system_counts)[which.max(wheel_system_counts)]
data$wheel_system[data$wheel_system == ""] <- mode_wheel_system
sum(data$wheel_system == "")
data$wheel_system = as.factor(data$wheel_system)

#wheel system display
wheel_system_display_counts <- table(data$wheel_system_display)
mode_wheel_system_display <- names(wheel_system_display_counts)[which.max(wheel_system_display_counts)]
data$wheel_system_display[data$wheel_system_display == ""] <- mode_wheel_system_display
sum(data$wheel_system_display == "")
data$wheel_system_display = as.factor(data$wheel_system_display)


#engine_type
engine_type_counts <- table(data$engine_type)
mode_engine_type <- names(engine_type_counts)[which.max(engine_type_counts)]
data$engine_type[data$engine_type == ""] <- mode_engine_type
sum(data$engine_type == "")
data$engine_type = as.factor(data$engine_type)

#exterior_colour
data$exterior_color = as.factor(data$exterior_color)

#interior colour
data$interior_color = as.factor(data$interior_color)

#fleet
data$fleet <- ifelse(data$fleet == "True", 1, 0)
sum(data$fleet == "")
sum(is.na(data$fleet))

#frame_damaged
data$frame_damaged <- ifelse(data$frame_damaged == "True", 1, 0)
sum(data$frame_damaged == "")
sum(is.na(data$frame_damaged))

#franchise_dealer
data$franchise_dealer <- ifelse(data$franchise_dealer == "True", 1, 0)
sum(data$franchise_dealer == "")
sum(is.na(data$franchise_dealer))

#has_accidents
data$has_accidents <- ifelse(data$has_accidents == "True", 1, 0)
sum(data$has_accidents == "")
sum(is.na(data$has_accidents))
unique(data$has_accidents)

#isCab
data$isCab <- ifelse(data$isCab == "True", 1, 0)
sum(data$isCab == "")
sum(is.na(data$isCab))
unique(data$isCab)

#is_cpo
data$is_cpo <- ifelse(data$is_cpo == "True", 1, 0)
sum(data$is_cpo == "")
sum(is.na(data$is_cpo))
unique(data$is_cpo)

#is_new
data$is_new <- ifelse(data$is_new == "True", 1, 0)
sum(data$is_new == "")
sum(is.na(data$is_new))
unique(data$is_new)

#listed_date
data$listed_date <- as.Date(data$listed_date, format = "%Y-%m-%d")
data$listed_date <- as.numeric(data$listed_date)

#mielage
# Find rows where fuel_type is "Gasoline" and mileage is NA
blank_mileage_indices <- which(data$fuel_type == "Gasoline" & is.na(data$mileage))
avg_mileage <- mean(data$mileage, na.rm = TRUE)
# Replace NA values in mileage with the average mileage for rows where fuel_type is "Gasoline"
data$mileage[blank_mileage_indices] <- avg_mileage
data$mileage[is.na(data$mileage)] <- avg_mileage

#owner_count
sum(data$owner_count == "")
sum(is.na(data$owner_count))


#salvage
data$salvage <- ifelse(data$salvage == "True", 1, 0)
sum(data$salvage == "")
sum(is.na(data$salvage))
unique(data$salvage)


#seller_rating
sum(data$seller_rating == "")
sum(is.na(data$seller_rating))

##listing_color
data$listing_color <- trimws(data$listing_color)  # Trim whitespace
color_counts <- table(data$listing_color)
top_color_types <- names(color_counts)[order(-color_counts)][1:7]
data$listing_color <- ifelse(data$listing_color %in% top_color_types, data$listing_color, "other")
data$listing_color = as.factor(data$listing_color)
summary(data$listing_color)

#figure out grep
unique(data$make_name)

data <- data %>%
  mutate(is_high_end = if_else(
    grepl("Mercedes-Benz|BMW|Audi|Lexus|Land Rover|Jaguar|Porsche|Maserati|Alfa Romeo|Bentley
          |Rolls-Royce|Aston Martin",
          make_name, ignore.case = TRUE), 1, 0))

data <- data %>%
  mutate(is_affordable = if_else(
    grepl("Chevrolet|Ford|Toyota|Nissan|Dodge|Hyundai|Subaru|Chrysler|Honda|Jeep|Kia
          |Volkswagen|Mazda|Buick|Mitsubishi|Genesis|FIAT",
          make_name, ignore.case = TRUE), 1, 0))

data <- data %>%
  mutate(
    Backup_Camera = ifelse(grepl("backup camera", major_options, ignore.case = TRUE), 1, 0),
    Alloy_Wheels = ifelse(grepl("alloy wheels", major_options, ignore.case = TRUE), 1, 0),
    Bluetooth = ifelse(grepl("bluetooth", major_options, ignore.case = TRUE), 1, 0),
    Heated_Seats = ifelse(grepl("heated seats", major_options, ignore.case = TRUE), 1, 0),
    Navigation_System = ifelse(grepl("navigation", major_options, ignore.case = TRUE), 1, 0),
    Remote_Start = ifelse(grepl("remote start", major_options, ignore.case = TRUE), 1, 0),
    Sunroof = ifelse(grepl("sunroof", major_options, ignore.case = TRUE), 1, 0),
    CarPlay = ifelse(grepl("car play|carplay|apple carplay|android auto", major_options, ignore.case = TRUE), 1, 0),
    Test_Drive = ifelse(grepl("test", description, ignore.case = TRUE), 1, 0),
    Leather = ifelse(grepl("leather", description, ignore.case = TRUE), 1, 0),
    Maintained = ifelse(grepl("maintained", description, ignore.case = TRUE), 1, 0)
  )
head(data)

### RANDOM FOREST ###

#SPLIT DATA
set.seed(1031)
train_indices <- createDataPartition(data$price, p = 0.70, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


#randomForest 
forest <- randomForest(price~ model_name+body_type+fuel_tank_volume_gallons+fuel_type+ highway_fuel_economy+
                         city_fuel_economy+torque+transmission+ wheel_system+ back_legroom_inches+ length_inches+
                         width_inches+height_inches+ engine_type + engine_displacement+ horsepower+daysonmarket+ exterior_color+
                         maximum_seating+ year+ fleet+ frame_damaged+ franchise_dealer+has_accidents+ isCab+
                         is_cpo+ is_new+ mileage+ salvage+ seller_rating+ RPM+ is_high_end+ is_affordable+ Backup_Camera+ Alloy_Wheels+ Bluetooth+
                         Heated_Seats+ Navigation_System+ Remote_Start+ Sunroof+ CarPlay+ Test_Drive + Leather + Maintained,
                       data=train_data,
                       ntree = 1000)

summary(forest)
pred_train <- predict(forest, train_data)
rmse_train <- sqrt(mean((train_data$price - pred_train)^2))
pred_test <- predict(forest, newdata = test_data)
rmse_test <- sqrt(mean((test_data$price - pred_test)^2))

pred <- predict(forest, newdata = scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'submission_30.csv',row.names = F)

### XG BOOST ###

#Features Selection
num_train_data <- data[, sapply(data, is.numeric)]
corr_matrix <- cor(num_train_data, use = "complete.obs") 
corr_target <- corr_matrix[,"price"]
print(corr_target)

# Remove features with low correlation with the target
threshold <- 0.2 
selected_features <- names(which(abs(corr_target) > threshold))
num_train_data_selected <- num_train_data[, selected_features]

# Checking for multicollinearity and removing highly correlated predictors
high_corr_threshold <- 0.75  
high_corr_matrix <- findCorrelation(corr_matrix, cutoff = high_corr_threshold, verbose = TRUE)
num_train_data_final <- num_train_data_selected[, -high_corr_matrix]


#SPLIT DATA
set.seed(1031) 
split <- createDataPartition(data$price, p = 0.70, list = FALSE)
train_data <- data[split, ]
test_data <- data[-split, ]

# Define predictors (X) and target variable (y)
X_train <- train_data[, -which(names(train_data) == "price")]
y_train <- train_data$price
X_test <- test_data[, -which(names(test_data) == "price")]
y_test <- test_data$price

# Convert data into DMatrix format for XGBoost
#X_train <- sapply(X_train, as.numeric)
#X_test <- sapply(X_test, as.numeric)

#Lasso
lasso_model <- cv.glmnet(
  x = as.matrix(train_data),
  y = train_data$price,
  alpha = 1,
  nfolds = 5
)
optimal_lambda <- lasso_model$lambda.min #optimal lambda
selected_features <- predict(lasso_model, newx = as.matrix(X_train), s = optimal_lambda, type = "nonzero")
selected_features <- as.numeric(unlist(selected_features))

# Filter train data with selected features
train_selected <- train_data[, selected_features]
test_elected <- test_data[, selected_features]
scoring_selected <- scoringData[, selected_features]


# Define XGBoost params
params <- list(
  objective = "reg:squarederror",  # Regression task
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Max depth of trees
  subsample = 0.5,                 # Subsampling rate
  colsample_bytree = 0.5           # Subsample ratio of columns when constructing each tree
)

# Create the DMatrix
dtrain <- xgb.DMatrix(data = as.matrix(train_selected), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(test_elected), label = y_test)

xgb_model <- xgboost(data = dtrain,
                     label = train$price,
                     nrounds = 5000,
                     params = params,
                     early_stopping_rounds = 5,
                     nthread = 8)
y_pred <- predict(xgb_model, newdata = dtest)
rmse_xg <- sqrt(mean((y_test - y_pred)^2))

importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)

train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  allowParallel = TRUE
)

# Grid of parameters to tune
tune_grid <- expand.grid(
  nrounds = 5000,
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.05, 0.1),
  gamma = c(0, 0.1, 0.2),  
  colsample_bytree = c(0.5, 0.7, 1),
  min_child_weight = c(1, 3, 5),  
  subsample = c(0.5, 0.7, 1)
)

model <- train(
  x = as.matrix(train_selected), 
  y = y_train,
  trControl = train_control,
  tuneGrid = tune_grid,
  method = "xgbTree"
)
print(model$bestTune)

scoringBoost <- sapply(scoring_selected, as.numeric)
prediction = predict(xgb_model, newdata = as.matrix(scoringBoost)) #pred for xgboost

#submit file
submissionFile = data.frame(id = scoringData$id, price = prediction)
write.csv(submissionFile, 'submission_27.csv',row.names = F)

## SCORING DATA ##

#CLEAN/PROCESS SCORING DATA
for (i in 1:ncol(scoringData)) {
  if (is.numeric(scoringData[, i])) {
    scoringData[is.na(scoringData[, i]), i] <- mean(scoringData[, i], na.rm = TRUE)
  } else {
    scoringData[is.na(scoringData[, i]), i] <- names(sort(table(scoringData[, i]), decreasing = TRUE))[1]
  }
}
sum(is.na(scoringData)) 

#make_name
summary(scoringData$make_name)
sum(is.na(scoringData$make_name))
scoringData$make_name = as.factor(scoringData$make_name)

#model_name
sum(is.na(scoringData$model_name))
scoringData$model_name = as.factor(scoringData$model_name)

#body_type
sum(is.na(scoringData$body_type))
scoringData$body_type = as.factor(scoringData$body_type)

#trim_name
trim_counts <- table(scoringData$trim_name)
mode_trim <- names(trim_counts)[which.max(trim_counts)]
scoringData$trim_name[scoringData$trim_name == ""] <- mode_trim
sum(scoringData$trim_name == "")
scoringData$trim_name = as.factor(scoringData$trim_name)

#fuel type
# Make fuel_type 'Gasoline' when fuel_tank_volume_gallons > 0 and fuel_type is blank
scoringData$fuel_type <- ifelse(scoringData$fuel_tank_volume_gallons > 0 & scoringData$fuel_type == "", "Gasoline", scoringData$fuel_type)
# Make fuel_type 'Electric' when fuel_tank_volume_gallons = 0
scoringData$fuel_type <- ifelse(scoringData$fuel_tank_volume_gallons == 0 & scoringData$fuel_type =="", "Electric", scoringData$fuel_type)
sum(scoringData$fuel_type == "")
scoringData$fuel_type = as.factor(scoringData$fuel_type)

# Calculate the average fuel tank volume for Gasoline
avg_volume <- mean(scoringData$fuel_tank_volume_gallons[scoringData$fuel_type == "Gasoline"], na.rm = TRUE)
scoringData$fuel_tank_volume_gallons[scoringData$fuel_tank_volume_gallons %in% c(NA, "") & scoringData$fuel_type == "Gasoline"] <- avg_volume
# Replace NA or blank values with 0 for Electric fuel type
scoringData$fuel_tank_volume_gallons[scoringData$fuel_type == "Electric" & (is.na(scoringData$fuel_tank_volume_gallons) | scoringData$fuel_tank_volume_gallons == "")] <- 0
sum(scoringData$fuel_tank_volume_gallons == "")
scoringData$fuel_tank_volume_gallons = as.factor(scoringData$fuel_tank_volume_gallons)

#power
scoringData$RPM <- (sub(".*@ (.+?) RPM", "\\1", scoringData$power)) # Extract everything until RPM # Extract and convert to numeric
# Replace NA values with the mean RPM
scoringData$RPM <- as.numeric(as.character(scoringData$RPM))
scoringData$RPM[is.na(scoringData$RPM)] <- mean(scoringData$RPM, na.rm = TRUE)
sum(scoringData$RPM == "")
sum(is.na(scoringData$RPM))

#torque
scoringData$torque = as.factor(scoringData$torque)

#transmission
transmission_counts <- table(scoringData$transmission)
mode_transmission <- names(transmission_counts)[which.max(transmission_counts)]
scoringData$transmission[scoringData$transmission == ""] <- mode_transmission
sum(scoringData$transmission == "")
scoringData$transmission = as.factor(scoringData$transmission)

#transmission_display
transmission_display_counts <- table(scoringData$transmission_display)
mode_transmission_display <- names(transmission_display_counts)[which.max(transmission_display_counts)]
scoringData$transmission_display[scoringData$transmission_display == ""] <- mode_transmission_display
sum(scoringData$transmission_display == "")
scoringData$transmission_display = as.factor(scoringData$transmission_display)

#wheel system
wheel_system_counts <- table(scoringData$wheel_system)
mode_wheel_system <- names(wheel_system_counts)[which.max(wheel_system_counts)]
scoringData$wheel_system[scoringData$wheel_system == ""] <- mode_wheel_system
sum(scoringData$wheel_system == "")
scoringData$wheel_system = as.factor(scoringData$wheel_system)

#wheel system display
wheel_system_display_counts <- table(scoringData$wheel_system_display)
mode_wheel_system_display <- names(wheel_system_display_counts)[which.max(wheel_system_display_counts)]
scoringData$wheel_system_display[scoringData$wheel_system_display == ""] <- mode_wheel_system_display
sum(scoringData$wheel_system_display == "")
scoringData$wheel_system_display = as.factor(scoringData$wheel_system_display)

#engine_type
engine_type_counts <- table(scoringData$engine_type)
mode_engine_type <- names(engine_type_counts)[which.max(engine_type_counts)]
scoringData$engine_type[scoringData$engine_type == ""] <- mode_engine_type
sum(scoringData$engine_type == "")
scoringData$engine_type = as.factor(scoringData$engine_type)

#exterior_colour
scoringData$exterior_color = as.factor(scoringData$exterior_color)

#interior colour
scoringData$interior_color = as.factor(scoringData$interior_color)

#fleet
scoringData$fleet <- ifelse(scoringData$fleet == "True", 1, 0)
sum(scoringData$fleet == "")
sum(is.na(scoringData$fleet))

#frame_damaged
scoringData$frame_damaged <- ifelse(scoringData$frame_damaged == "True", 1, 0)
sum(scoringData$frame_damaged == "")
sum(is.na(scoringData$frame_damaged))

#franchise_dealer
scoringData$franchise_dealer <- ifelse(scoringData$franchise_dealer == "True", 1, 0)
sum(scoringData$franchise_dealer == "")
sum(is.na(scoringData$franchise_dealer))

#has_accidents
scoringData$has_accidents <- ifelse(scoringData$has_accidents == "True", 1, 0)
sum(scoringData$has_accidents == "")
sum(is.na(scoringData$has_accidents))
unique(scoringData$has_accidents)

#isCab
scoringData$isCab <- ifelse(scoringData$isCab == "True", 1, 0)
sum(scoringData$isCab == "")
sum(is.na(scoringData$isCab))
unique(scoringData$isCab)

#is_cpo
scoringData$is_cpo <- ifelse(scoringData$is_cpo == "True", 1, 0)
sum(scoringData$is_cpo == "")
sum(is.na(scoringData$is_cpo))
unique(scoringData$is_cpo)

#is_new
scoringData$is_new <- ifelse(scoringData$is_new == "True", 1, 0)
sum(scoringData$is_new == "")
sum(is.na(scoringData$is_new))
unique(scoringData$is_new)

#listed_date
scoringData$listed_date <- as.Date(scoringData$listed_date, format = "%Y-%m-%d")
scoringData$listed_date <- as.numeric(scoringData$listed_date)

#owner_count
sum(scoringData$owner_count == "")
sum(is.na(scoringData$owner_count))

#salvage
scoringData$salvage <- ifelse(scoringData$salvage == "True", 1, 0)
sum(scoringData$salvage == "")
sum(is.na(scoringData$salvage))
unique(scoringData$salvage)

#seller_rating
sum(scoringData$seller_rating == "")
sum(is.na(scoringData$seller_rating))

##listing_color
scoringData$listing_color <- trimws(scoringData$listing_color)  # Trim whitespace
color_counts <- table(scoringData$listing_color)
top_color_types <- names(color_counts)[order(-color_counts)][1:7]
scoringData$listing_color <- ifelse(scoringData$listing_color %in% top_color_types, scoringData$listing_color, "other")
scoringData$listing_color = as.factor(scoringData$listing_color)
summary(scoringData$listing_color)

#figure out grep
scoringData <- scoringData %>%
  mutate(is_high_end = if_else(
    grepl("Mercedes-Benz|BMW|Audi|Lexus|Land Rover|Jaguar|Porsche|Maserati|Alfa Romeo|Bentley
          |Rolls-Royce|Aston Martin",
          make_name, ignore.case = TRUE), 1, 0))

scoringData <- scoringData %>%
  mutate(is_affordable = if_else(
    grepl("Chevrolet|Ford|Toyota|Nissan|Dodge|Hyundai|Subaru|Chrysler|Honda|Jeep|Kia
          |Volkswagen|Mazda|Buick|Mitsubishi|Genesis|FIAT",
          make_name, ignore.case = TRUE), 1, 0))

scoringData <- scoringData %>%
  mutate(
    Backup_Camera = ifelse(grepl("backup camera", major_options, ignore.case = TRUE), 1, 0),
    Alloy_Wheels = ifelse(grepl("alloy wheels", major_options, ignore.case = TRUE), 1, 0),
    Bluetooth = ifelse(grepl("bluetooth", major_options, ignore.case = TRUE), 1, 0),
    Heated_Seats = ifelse(grepl("heated seats", major_options, ignore.case = TRUE), 1, 0),
    Navigation_System = ifelse(grepl("navigation", major_options, ignore.case = TRUE), 1, 0),
    Remote_Start = ifelse(grepl("remote start", major_options, ignore.case = TRUE), 1, 0),
    Sunroof = ifelse(grepl("sunroof", major_options, ignore.case = TRUE), 1, 0),
    CarPlay = ifelse(grepl("car play|carplay|apple carplay|android auto", major_options, ignore.case = TRUE), 1, 0),
    Test_Drive = ifelse(grepl("test", description, ignore.case = TRUE), 1, 0),
    Leather = ifelse(grepl("leather", description, ignore.case = TRUE), 1, 0),
    Maintained = ifelse(grepl("maintained", description, ignore.case = TRUE), 1, 0)
  )



