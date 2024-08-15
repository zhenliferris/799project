# Load necessary libraries
library(caret)

# Load the data
data <- read.csv('C:/799CapStone_Z/facial_oral_temperature_data/mydataframe.csv')

# Remove the first column
data <- data[, -1]

# Define the target variable and predictor variables
target <- 'T_RC_Max1'
predictors <- setdiff(names(data), target)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data[[target]], p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

# Build a linear regression model using lm
lm_model <- lm(as.formula(paste(target, "~", paste(predictors, collapse = "+"))), data = dataTrain)

# Print the summary of the lm model
summary(lm_model)

# Make predictions on the test set with lm model
lm_predictions <- predict(lm_model, dataTest)

# Evaluate the lm model
lm_results <- postResample(lm_predictions, dataTest[[target]])
print(lm_results)

# Build a generalized linear model using glm with Gaussian family (equivalent to lm)
glm_model <- glm(as.formula(paste(target, "~", paste(predictors, collapse = "+"))), data = dataTrain, family = gaussian())

# Print the summary of the glm model
summary(glm_model)

# Make predictions on the test set with glm model
glm_predictions <- predict(glm_model, dataTest)

# Evaluate the glm model
glm_results <- postResample(glm_predictions, dataTest[[target]])
print(glm_results)

# Compare the results
comparison <- data.frame(
  Model = c('Linear Regression (lm)', 'Generalized Linear Model (glm)'),
  RMSE = c(lm_results[1], glm_results[1]),
  R2 = c(lm_results[2], glm_results[2]),
  MAE = c(lm_results[3], glm_results[3])
)
print(comparison)
