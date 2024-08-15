library(tidyverse) #include "dplyr" "ggplot2"
library(Hmisc)
library(psych)
library(scales)
library(caTools) # seed
library(patchwork) # multiplot 
library(car) # vif
library(reshape2) # melt() to convert wide format to long format
library(caret)
library(randomForest)
library(glmnet)


# Read the CSV file
file_path <- "C:/799CapStone_Z/facial_oral_temperature_data/FLIR_groups1and2.csv"
data <- read.csv(file_path, header = FALSE)
data_selected <- data %>% select(3:27, 29, 117:122)

colnames(data_selected) <- data_selected[3,]
data_selected <-data_selected[-(1:3),]
Hmisc::describe(data_selected)
# Convert all empty strings to NA
data_selected[data_selected == ""] <- NA

# Remove rows with any NA values
data_cleaned <- data_selected[complete.cases(data_selected), ]

# combine age 21-30 to one group 
data_cleaned$Age[data_cleaned$Age == "21-25" | data_cleaned$Age == "26-30"] <- "21-30"
data_cleaned$Age[data_cleaned$Age == ">60" ] <- "60+"
Hmisc::describe(data_cleaned)


mydataframe <- data_cleaned %>% 
  mutate(
    # Encoding categorical variables
    # Gender_Female as base line
    Gender_M = ifelse(Gender == "Male", 1, 0),
   
  )

# Removing columns: 'Gender', 'Age', and 'Ethnicity'
mydataframe <- mydataframe[, !(names(mydataframe) %in% c("Gender", "Age", "Ethnicity"))]  

# # Save the dataframe to a CSV file
# write.csv(mydataframe, file = "C:/799CapStone_Z/facial_oral_temperature_data/mydataframe.csv")


# Ensure all variables are numerical
mydataframe <- mydataframe %>% 
  mutate_if(~ all(!is.na(as.numeric(as.character(.)))), as.numeric)

# Basic descriptive statistics 
des_stats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s,
           skew=skew, kurtosis=kurt))
}
sapply(mydataframe[1:29], des_stats)
# Define a function to detect outliers
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x < lower_bound | x > upper_bound
}


# Function to remove outliers for specified variables
remove_outliers <- function(df, variables) {
  for (var in variables) {
    if (var %in% names(df)) {
      outliers <- detect_outliers(df[[var]])
      df <- df[!outliers, ]
    }
  }
  return(df)
}
columns_to_clean <- names(mydataframe)[1:29]
mydataframe <- remove_outliers(mydataframe, columns_to_clean)

sapply(mydataframe[1:29], des_stats)


# standardize variables
mydataframe[, 1:29] <- scale(mydataframe[, 1:29])

mydataframe_copy <- mydataframe

set.seed(42)
split <- sample.split(mydataframe$T_OR_Max1, SplitRatio = 0.8)
train_data <- subset(mydataframe, split == TRUE)
test_data <- subset(mydataframe, split == FALSE)

fit_full <- lm(T_OR_Max1~. ,data=train_data)

fit_reduce_inter <- lm(T_OR_Max1 ~ T_offset1 + Max1R13_1 + aveAllR13_1 + 
                         T_RC_Dry1 + T_RC_Wet1 + canthi4Max1 + T_FHBC1 + T_FH_Max1 + 
                         T_Max1 + T_atm + Humidity + T_offset1:aveAllR13_1 + T_offset1:T_FHBC1 + 
                         T_offset1:T_atm + aveAllR13_1:T_FHBC1 + aveAllR13_1:Humidity + 
                         T_RC_Dry1:canthi4Max1 + T_RC_Dry1:T_FHBC1 + T_RC_Dry1:T_Max1 + 
                         T_RC_Dry1:Humidity + T_RC_Wet1:canthi4Max1 + T_RC_Wet1:T_Max1 + 
                         T_RC_Wet1:T_atm + T_RC_Wet1:Humidity + canthi4Max1:T_FH_Max1 + 
                         T_FHBC1:Humidity + T_FH_Max1:T_Max1 + T_FH_Max1:T_atm + T_Max1:T_atm, data = train_data)

# Load required libraries
# Prepare the data
x_train <- model.matrix(T_OR_Max1 ~ ., train_data)[,-1] # Remove intercept
y_train <- train_data$T_OR_Max1

# Ridge Regression
set.seed(42)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0) # alpha = 0 for ridge
best_lambda_ridge <- ridge_model$lambda.min

# Fit the final ridge model
ridge_final <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda_ridge)
print(ridge_final)

# Predictions on training data
ridge_train_preds <- predict(ridge_final, s = best_lambda_ridge, newx = x_train)
ridge_train_rmse <- sqrt(mean((ridge_train_preds - y_train)^2))

# Predictions on test data
x_test <- model.matrix(T_OR_Max1 ~ ., test_data)[,-1]
y_test <- test_data$T_OR_Max1
ridge_test_preds <- predict(ridge_final, s = best_lambda_ridge, newx = x_test)
ridge_test_rmse <- sqrt(mean((ridge_test_preds - y_test)^2))

# Output RMSE
cat("Ridge RMSE on Training Data:", ridge_train_rmse, "\n")
cat("Ridge RMSE on Testing Data:", ridge_test_rmse, "\n")

