library(tidyverse) #include "dplyr" "ggplot2"
library(Hmisc)
library(psych)
library(scales)
library(caTools) # seed
library(patchwork) # multiplot 
library(car) # vif
library(reshape2) # melt() to convert wide format to long format
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
# Categorical data
catvars_plot <- names(data_cleaned)[27:29]
for (catvar in catvars_plot) {
  p <- ggplot(data_cleaned, aes_string(x = catvar, fill=catvar)) +
    geom_bar() +
    labs(title = paste("Bar chart grouped by", catvar), x = catvar, y = "Frequency")+
    coord_flip() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          axis.text.y = element_blank())
  
  print(p)
}

mydataframe <- data_cleaned %>% 
  mutate(
    # Encoding categorical variables
    # Gender_Female as base line
    Gender_M = ifelse(Gender == "Male", 1, 0),
    # Age_less20 as base line
    Age_less30 = ifelse(Age == "21-30", 1, 0),
    Age_less40 = ifelse(Age == "31-40", 1, 0),
    Age_less50 = ifelse(Age == "41-50", 1, 0),
    Age_less60 = ifelse(Age == "51-60", 1, 0),
    Age_greater60 = ifelse(Age == "60+", 1, 0),
    # Ethnicity American Indian or Alaskan Native as base line
    Ethnicity_A = ifelse(Ethnicity == "Asian", 1, 0),
    Ethnicity_B = ifelse(Ethnicity == "Black or African-American", 1, 0),
    Ethnicity_H = ifelse(Ethnicity == "Hispanic/Latino", 1, 0),
    Ethnicity_M = ifelse(Ethnicity == "Multiracial", 1, 0),
    Ethnicity_W = ifelse(Ethnicity == "White", 1, 0),
    #variable_SD = scale(variable),
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
i<-1
e<-3
while(i<28){
  plots <- list()
  vars_plot <- names(mydataframe)[i:(i+2)]
  for (var in vars_plot) {
    p <- ggplot(mydataframe, aes_string(x = var)) +
      geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", alpha = 0.5) +
      geom_density(alpha = .3, fill = "#FF6666") +
      labs(title = paste("Distribution of", var), x = var, y = "Density") +
      theme(
        plot.title = element_text(size = 8),  # Smaller plot title
        axis.title = element_text(size = 6),  # Smaller axis titles
        axis.text = element_text(size = 4)    # Smaller axis text
      )
    
    plots[[var]] <- p
  }
  
  # Combine the plots using patchwork
  combined_plot <- wrap_plots(plots) + plot_layout(ncol = 3)
  
  # Print the combined plot
  print(combined_plot)
  i<-i+3
}
plots <- list()
vars_plot <- names(mydataframe)[28:29]
for (var in vars_plot) {
  p <- ggplot(mydataframe, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bin=30, fill = "blue", alpha = 0.5) +
    geom_density(alpha = .3, fill = "#FF6666") +
    labs(title = paste("Distribution of", var), x = var, y = "Density")+
    theme(
      plot.title = element_text(size = 8),  # Smaller plot title
      axis.title = element_text(size = 6),  # Smaller axis titles
      axis.text = element_text(size = 4)    # Smaller axis text
    )
  
  plots[[var]] <- p
}

# Combine the plots using patchwork
combined_plot <- wrap_plots(plots) + plot_layout(ncol = 3)

# Print the combined plot
print(combined_plot)

# Correlation matrix and tests of significance via corr.test() in psych package
corr.test(mydataframe[1:29], use="complete")
corr_result <- corr.test(mydataframe[1:29], use = "complete")

corr_matrix <- corr_result$r
# wider to long form
corr_matrix_melt <- melt(corr_matrix)

ggplot(data = corr_matrix_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + #heat map tile
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + #Ensures that each tile is square by fixing the aspect ratio.
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

i<-1
e<-3
while(i<25){
  plots <- list()
  
  # plot target vs dependent variables
  devars <- names(mydataframe)[i:(i+2)]
  for (var in devars) {
    p <- ggplot(mydataframe, aes_string(x = var, y="T_OR_Max1")) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      labs(title = paste("T_OR_Max1 vs.", var), x = var, y = "T_OR_Max1")+
      theme(
        plot.title = element_text(size = 8),  # Smaller plot title
        axis.title = element_text(size = 6),  # Smaller axis titles
        axis.text = element_text(size = 4)    # Smaller axis text
      )
    
    plots[[var]] <- p
  }
  # Combine the plots using patchwork
  combined_plot <- wrap_plots(plots) + plot_layout(ncol = 3)
  
  # Print the combined plot
  print(combined_plot)
  i<-i+3
}

plots <- list()
devars <- names(mydataframe)[27:29]
for (var in devars) {
  p <- ggplot(mydataframe, aes_string(x = var, y="T_OR_Max1")) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = paste("T_OR_Max1 vs.", var), x = var, y = "T_OR_Max1")+
    theme(
      plot.title = element_text(size = 8),  # Smaller plot title
      axis.title = element_text(size = 6),  # Smaller axis titles
      axis.text = element_text(size = 4)    # Smaller axis text
    )
  
  plots[[var]] <- p
}
# Combine the plots using patchwork
combined_plot <- wrap_plots(plots) + plot_layout(ncol = 3)

# Print the combined plot
print(combined_plot)
# standardize variables
mydataframe[, 1:29] <- scale(mydataframe[, 1:29])
set.seed(42)
split <- sample.split(mydataframe$T_OR_Max1, SplitRatio = 0.6)
train_data <- subset(mydataframe, split == TRUE)
test_data <- subset(mydataframe, split == FALSE)

# Full regression model
fit_full <- glm(T_OR_Max1~. ,data=train_data)
summary(fit_full)
vif_values <- vif(fit_full)
print(vif_values)
# stepwised regression model
fit_reduce <- step(fit_full)
summary(fit_reduce)
vif_values <- vif(fit_reduce)
print(vif_values)
reduced_columns_pca <- mydataframe %>% 
  select(T_offset1, aveAllR13_1, aveAllL13_1, T_RC_Wet1, 
         T_LC_Max1, RCC1, canthi4Max1, T_FHCC1, 
         T_FHLC1, T_FHBC1, T_FH_Max1, T_Max1, 
         T_atm, Humidity, Distance)


corr_result <- corr.test(reduced_columns_pca, use = "complete")

corr_matrix <- corr_result$r
print(round(corr_matrix, 2))
# wider to long form
corr_matrix_melt <- melt(corr_matrix)

ggplot(data = corr_matrix_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + #heat map tile
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() + #Ensures that each tile is square by fixing the aspect ratio.
  labs(title = "Reduced Regression Correlation Heatmap", x = "", y = "")


fa.parallel(reduced_columns_pca, fa = "pc", n.iter = 100,
            show.legend = FALSE, main = "Scree plot with parallel analysis")
abline(h=1)

# Perform PCA using prcomp
pca_result <- prcomp(reduced_columns_pca,
                     center = FALSE, # Data already centered
                     scale. = FALSE) # Data already scaled

# Extract the eigenvalues from the PCA result
eigenvalues <- pca_result$sdev^2

# Print the eigenvalues
print(paste("Component eigenvalues: ", round(eigenvalues, 2)))

# Calculate the percentage contributions for each principal component
percentage_contributions <- eigenvalues / sum(eigenvalues) * 100

# Print the percentage contributions
print(paste("Component contributions: ", round(percentage_contributions, 2), "%"))

# Print accumulated contributions
acc_con <- 0
for (i in seq_along(percentage_contributions)) {
  acc_con <- acc_con + percentage_contributions[i]
  print(paste("Accumulated contribution up to component", i, ": ", round(acc_con, 2), "%"))
}

# Create a data frame for plotting
principal_components <- paste0("PC", 1:length(percentage_contributions))
contributions_df <- data.frame(
  Principal_Component = factor(principal_components, levels = principal_components),
  Percentage_Contribution = percentage_contributions
)

# Plot the percentage contributions
ggplot(contributions_df, aes(x = Principal_Component, y = Percentage_Contribution, fill = Principal_Component)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        legend.position = "none")+
  labs(title = "Percentage Contributions of Principal Components",
       x = "Principal Component",
       y = "Percentage Contribution")

# Extracting the first 8 principal components
pc_data <- pca_result$x[, 1:8]
# Extracting the first 4 principal components
pc_data_1 <- pca_result$x[, 1:4]
# Extracting the first principal components
pc_data_2 <- pca_result$x[, 1]

reduced_columns_nonepca <- mydataframe %>% 
  select(Ethnicity_W, Ethnicity_M,,T_OR_Max1)

# Combine the PCs with the response variable
glm_data <- cbind(pc_data, reduced_columns_nonepca)
set.seed(42)
split <- sample.split(glm_data$T_OR_Max1, SplitRatio = 0.6)
train_data_pca <- subset(glm_data, split == TRUE)
test_data_pca <- subset(glm_data, split == FALSE)

fit_pca <- glm(T_OR_Max1~. ,data=train_data_pca)
summary(fit_pca)
vif_values_pca <- vif(fit_pca)
print(vif_values_pca)


# Combine the PCs with the response variable
glm_data_1 <- cbind(pc_data_1, reduced_columns_nonepca)
set.seed(42)
split <- sample.split(glm_data_1$T_OR_Max1, SplitRatio = 0.6)
train_data_pca_1 <- subset(glm_data_1, split == TRUE)
test_data_pca_1 <- subset(glm_data_1, split == FALSE)

fit_pca_1 <- glm(T_OR_Max1~. ,data=train_data_pca_1)
summary(fit_pca_1)
vif_values_pca_1 <- vif(fit_pca_1)
print(vif_values_pca_1)


reduced_columns_nonepca <- mydataframe %>% 
  select(Ethnicity_W, Ethnicity_M,,T_OR_Max1)

# Combine the PCs with the response variable
glm_data_2 <- cbind(pc_data_2, reduced_columns_nonepca)
set.seed(42)
split <- sample.split(glm_data_2$T_OR_Max1, SplitRatio = 0.6)
train_data_pca_2 <- subset(glm_data_2, split == TRUE)
test_data_pca_2 <- subset(glm_data_2, split == FALSE)

fit_pca_2 <- glm(T_OR_Max1~. ,data=train_data_pca_2)
summary(fit_pca_2)
vif_values_pca_2 <- vif(fit_pca_2)
print(vif_values_pca_2)

# Calculate evaluation metrics
evaluate_model <- function(model, test_data) {
  # Predict the test data
  predictions <- predict(model, newdata = test_data)
  
  # Actual values
  actuals <- test_data$T_OR_Max1
  
  # Calculate MAE
  mae <- mean(abs(predictions - actuals))
  
  # Calculate MSE
  mse <- mean((predictions - actuals)^2)
  
  # Calculate RMSE
  rmse <- sqrt(mse)
  
  # Calculate R-squared
  r_squared <- 1 - (sum((actuals - predictions)^2) / sum((actuals - mean(actuals))^2))
  
  return(c(MAE = mae, MSE = mse, RMSE = rmse, R2 = r_squared))
}

# Evaluate the full model
full_model_metrics <- evaluate_model(fit_full, test_data)
print(full_model_metrics)

# Evaluate the reduced model
reduced_model_metrics <- evaluate_model(fit_reduce, test_data)
print(reduced_model_metrics)

# Evaluate the PCA models
pca_model_metrics <- evaluate_model(fit_pca, test_data_pca)
print(pca_model_metrics)

pca_model_1_metrics <- evaluate_model(fit_pca_1, test_data_pca_1)
print(pca_model_1_metrics)

pca_model_2_metrics <- evaluate_model(fit_pca_2, test_data_pca_2)
print(pca_model_2_metrics)

# Visualize the predictions vs actuals
plot_predictions <- function(model, test_data, title = "Predictions vs Actuals") {
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data$T_OR_Max1
  
  ggplot(data = data.frame(actuals, predictions), aes(x = actuals, y = predictions)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = title, x = "Actual Values", y = "Predicted Values") +
    theme_minimal()
}

# Plot for full model
plot_full <- plot_predictions(fit_full, test_data, "Full Model: Predictions vs Actuals")
print(plot_full)

# Plot for reduced model
plot_reduced <- plot_predictions(fit_reduce, test_data, "Reduced Model: Predictions vs Actuals")
print(plot_reduced)

# Plot for PCA models
plot_pca <- plot_predictions(fit_pca, test_data_pca, "PCA Model: Predictions vs Actuals")
print(plot_pca)

plot_pca_1 <- plot_predictions(fit_pca_1, test_data_pca_1, "PCA Model 1: Predictions vs Actuals")
print(plot_pca_1)

plot_pca_2 <- plot_predictions(fit_pca_2, test_data_pca_2, "PCA Model 2: Predictions vs Actuals")
print(plot_pca_2)

