#| echo: true
#| message: false
#| fig-align: center
#| code-line-numbers: "|1|2-12"
library(tidyverse) 
library(Hmisc)
library(psych)
library(scales)
library(caTools)
library(patchwork)
library(car)
library(reshape2) 
library(caret)
library(glmnet)
library(gridExtra)
# Read the CSV file
file_path <- "C:/799CapStone_Z/facial_oral_temperature_data/FLIR_groups1and2.csv"
data <- read.csv(file_path, header = FALSE)
data_selected <- data %>% select(3:27, 29, 117:122)

colnames(data_selected) <- data_selected[3,]
data_selected <-data_selected[-(1:3),]
# Hmisc::describe(data_selected)
#| echo: true
#| message: false
#| fig-align: center
#| code-line-numbers: "|1-2|4-5|7-9"
# Convert all empty strings to NA
data_selected[data_selected == ""] <- NA
# typeof(NA)
# sapply(data_selected, function(col) any(is.na(col))) 
# Remove rows with any NA values
data_cleaned <- data_selected[complete.cases(data_selected), ]

# combine age 21-30 to one group 
data_cleaned$Age[data_cleaned$Age == "21-25" | data_cleaned$Age == "26-30"] <- "21-30"
data_cleaned$Age[data_cleaned$Age == ">60" ] <- "60+"
# Hmisc::describe(data_cleaned)
# Categorical data
catvars_plot <- names(data_cleaned)[27:29]
plot_list <- list()

for (catvar in catvars_plot) {
  p <- ggplot(data_cleaned, aes_string(x = catvar, fill = catvar)) +
    geom_bar() +
    labs(title = paste("Bar chart grouped by", catvar), x = catvar, y = "Frequency") +
    coord_flip() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          axis.text.y = element_blank())
  
  plot_list[[catvar]] <- p
}

grid.arrange(grobs = plot_list, ncol = 1)

#| echo: true
#| message: false
#| fig-align: center
#| code-line-numbers: "|1,2,5|"
mydataframe <- data_cleaned %>% 
  mutate(
    # Encoding categorical variables
    # Gender_Female as base line
    Gender_M = ifelse(Gender == "Male", 1, 0),)
    
# Removing columns: 'Gender', 'Age', and 'Ethnicity'
mydataframe <- mydataframe[, !(names(mydataframe) %in% c("Gender", "Age", "Ethnicity"))]  

# Ensure all variables are numerical
mydataframe <- mydataframe %>% 
  mutate_if(~ all(!is.na(as.numeric(as.character(.)))), as.numeric)

#| echo: true
#| message: false
#| fig-align: center
#| code-line-numbers: "|1-4|5-9"
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
# i<-1
# e<-3
# while(i<28){
#     plots <- list()
#     vars_plot <- names(mydataframe)[i:(i+2)]
#     for (var in vars_plot) {
#       p <- ggplot(mydataframe, aes_string(x = var)) +
#         geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", alpha = 0.5) +
#         geom_density(alpha = .3, fill = "#FF6666") +
#         labs(title = paste("Distribution of", var), x = var, y = "Density") +
#          theme(
#           plot.title = element_text(size = 8),  # Smaller plot title
#           axis.title = element_text(size = 6),  # Smaller axis titles
#           axis.text = element_text(size = 4)    # Smaller axis text
#         )
#       
#       plots[[var]] <- p
#     }
#     
#     # Combine the plots using patchwork
#     combined_plot <- wrap_plots(plots) + plot_layout(ncol = 3)
#     
#     # Print the combined plot
#     print(combined_plot)
#     i<-i+3
# }
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

# sapply(mydataframe[1:29], des_stats)
# i<-1
# e<-3
# while(i<28){
#     plots <- list()
#     vars_plot <- names(mydataframe)[i:(i+2)]
#     for (var in vars_plot) {
#       p <- ggplot(mydataframe, aes_string(x = var)) +
#         geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", alpha = 0.5) +
#         geom_density(alpha = .3, fill = "#FF6666") +
#         labs(title = paste("Distribution of", var), x = var, y = "Density") +
#          theme(
#           plot.title = element_text(size = 8),  # Smaller plot title
#           axis.title = element_text(size = 6),  # Smaller axis titles
#           axis.text = element_text(size = 4)    # Smaller axis text
#         )
#       
#       plots[[var]] <- p
#     }
#     
#     # Combine the plots using patchwork
#     combined_plot <- wrap_plots(plots) + plot_layout(ncol = 3)
#     
#     # Print the combined plot
#     print(combined_plot)
#     i<-i+3
# }
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

# i<-1
# e<-3
# while(i<25){
#     plots <- list()
# 
#     # plot target vs dependent variables
#     devars <- names(mydataframe)[i:(i+2)]
#     for (var in devars) {
#       p <- ggplot(mydataframe, aes_string(x = var, y="T_OR_Max1")) +
#         geom_point() +
#         geom_smooth(method = "lm", se = TRUE) +
#         labs(title = paste("T_OR_Max1 vs.", var), x = var, y = "T_OR_Max1")+
#          theme(
#           plot.title = element_text(size = 8),  # Smaller plot title
#           axis.title = element_text(size = 6),  # Smaller axis titles
#           axis.text = element_text(size = 4)    # Smaller axis text
#         )
#       
#       plots[[var]] <- p
#     }
#      # Combine the plots using patchwork
#     combined_plot <- wrap_plots(plots) + plot_layout(ncol = 3)
#     
#     # Print the combined plot
#     print(combined_plot)
#     i<-i+3
# }

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
mydataframe_copy <- mydataframe
#| echo: true
#| message: false
#| fig-align: center
#| code-line-numbers: "|1|2|3,4"
set.seed(42)
split <- sample.split(mydataframe$T_OR_Max1, SplitRatio = 0.8)
train_data <- subset(mydataframe, split == TRUE)
test_data <- subset(mydataframe, split == FALSE)

# Full regression model
fit_full <- lm(T_OR_Max1~. ,data=train_data)
summary(fit_full)
# Reduced regression model with interaction terms
fit_reduce_inter <- lm(T_OR_Max1 ~ T_offset1 + Max1R13_1 + aveAllR13_1 + 
    T_RC_Dry1 + T_RC_Wet1 + canthi4Max1 + T_FHBC1 + T_FH_Max1 + 
    T_Max1 + T_atm + Humidity + T_offset1:aveAllR13_1 +
    T_offset1:T_FHBC1 + T_offset1:T_atm + aveAllR13_1:T_FHBC1 +
    aveAllR13_1:Humidity + T_RC_Dry1:canthi4Max1 + 
    T_RC_Dry1:T_FHBC1 + T_RC_Dry1:T_Max1 + T_RC_Dry1:Humidity +
    T_RC_Wet1:canthi4Max1 + T_RC_Wet1:T_Max1 + T_RC_Wet1:T_atm +
    T_RC_Wet1:Humidity + canthi4Max1:T_FH_Max1 + T_FHBC1:Humidity +
    T_FH_Max1:T_Max1 + T_FH_Max1:T_atm + T_Max1:T_atm, 
    data = train_data)
summary(fit_reduce_inter)
vif_values <- vif(fit_reduce_inter)
cat("\n")
print(vif_values)
# PCA
mydataframe_pca <- mydataframe_copy
# create interactions
mydataframe_pca <- mydataframe_pca %>%
  mutate(
    T_offset1_aveAllR13_1=T_offset1*aveAllR13_1,
    T_offset1_T_FHBC1=T_offset1*T_FHBC1, 
    T_offset1_T_atm=T_offset1*T_atm,
    aveAllR13_1_T_FHBC1=aveAllR13_1*T_FHBC1,
    aveAllR13_1_Humidity=aveAllR13_1*Humidity,
    T_RC_Dry1_canthi4Max1=T_RC_Dry1*canthi4Max1,
    T_RC_Dry1_T_FHBC1=T_RC_Dry1*T_FHBC1,
    T_RC_Dry1_T_Max1=T_RC_Dry1*T_Max1,
    T_RC_Dry1_Humidity=T_RC_Dry1*Humidity,
    T_RC_Wet1_canthi4Max1=T_RC_Wet1*canthi4Max1,
    T_RC_Wet1_T_Max1=T_RC_Wet1*T_Max1,
    T_RC_Wet1_T_atm=T_RC_Wet1*T_atm,
    T_RC_Wet1_Humidity=T_RC_Wet1*Humidity,
    canthi4Max1_T_FH_Max1=canthi4Max1*T_FH_Max1,
    T_FHBC1_Humidity=T_FHBC1*Humidity,
    T_FH_Max1_T_Max1=T_FH_Max1*T_Max1,
    T_FH_Max1_T_atm=T_FH_Max1*T_atm,
    T_Max1_T_atm=T_Max1*T_atm
  )

reduced_columns_pca <- mydataframe_pca %>% 
  select(T_offset1,Max1R13_1, aveAllR13_1, T_RC_Dry1, T_RC_Wet1, 
         canthi4Max1, T_FHBC1, T_FH_Max1, T_Max1, T_atm, T_offset1_aveAllR13_1,
         T_offset1_T_FHBC1, 
         T_offset1_T_atm,
         aveAllR13_1_T_FHBC1,
         aveAllR13_1_Humidity,
         T_RC_Dry1_canthi4Max1,
         T_RC_Dry1_T_FHBC1,
         T_RC_Dry1_T_Max1,
         T_RC_Dry1_Humidity,
         T_RC_Wet1_canthi4Max1,
         T_RC_Wet1_T_Max1,
         T_RC_Wet1_T_atm,
         T_RC_Wet1_Humidity,
         canthi4Max1_T_FH_Max1,
         T_FHBC1_Humidity,
         T_FH_Max1_T_Max1,
         T_FH_Max1_T_atm,
         T_Max1_T_atm)

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
# print(paste("Component eigenvalues: ", round(eigenvalues, 2)))

# Calculate the percentage contributions for each principal component
percentage_contributions <- eigenvalues / sum(eigenvalues) * 100

# Print the percentage contributions
# print(paste("Component contributions: ", round(percentage_contributions, 2), "%"))

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
# Extracting the first 6 principal components
pc_data <- pca_result$x[, 1:6]

reduced_columns_target<- mydataframe_pca %>% 
  select(T_OR_Max1)


# modeling
lm_data <- cbind(pc_data, reduced_columns_target)
set.seed(42)
split <- sample.split(lm_data$T_OR_Max1, SplitRatio = 0.8)
train_data_pca <- subset(lm_data, split == TRUE)
test_data_pca <- subset(lm_data, split == FALSE)

fit_pca <- lm(T_OR_Max1~. ,data=train_data_pca)
summary(fit_pca)
vif_values_pca <- vif(fit_pca)
print(vif_values_pca)
#| echo: true
#| message: false
#| fig-align: center
#| code-line-numbers: "|1-16|19-22"
# Prepare the data
x_train <- model.matrix(T_OR_Max1 ~ T_offset1 + Max1R13_1 + aveAllR13_1 +                                     T_RC_Dry1 + T_RC_Wet1 + canthi4Max1 + T_FHBC1 + T_FH_Max1+
                          T_Max1 + T_atm + Humidity + T_offset1:aveAllR13_1 +
                         T_offset1:T_FHBC1 + 
                         T_offset1:T_atm + aveAllR13_1:T_FHBC1 + 
                         aveAllR13_1:Humidity +
                         T_RC_Dry1:canthi4Max1 + T_RC_Dry1:T_FHBC1 +
                         T_RC_Dry1:T_Max1 + 
                         T_RC_Dry1:Humidity + T_RC_Wet1:canthi4Max1 +
                         T_RC_Wet1:T_Max1 + 
                         T_RC_Wet1:T_atm + T_RC_Wet1:Humidity + 
                         canthi4Max1:T_FH_Max1 + 
                         T_FHBC1:Humidity + T_FH_Max1:T_Max1 + T_FH_Max1:T_atm +
                         T_Max1:T_atm, train_data)[,-1] 
y_train <- train_data$T_OR_Max1

# Ridge Regression
set.seed(42)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0) # alpha = 0 for ridge
best_lambda_ridge <- ridge_model$lambda.min
# Fit the final ridge model
ridge_final <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda_ridge)

# Extract coefficients
ridge_coefficients <- coef(ridge_final)

# Print coefficients
# print(ridge_coefficients)

# Construct the equation
intercept <- ridge_coefficients[1] # Intercept
coefficients <- ridge_coefficients[-1] # Coefficients for predictors
predictor_names <- colnames(x_train)

# Display the equation
cat("Ridge Regression Equation:\n")
cat("T_OR_Max1 =", intercept, "+\n")
for (i in 1:length(coefficients)) {
  cat(sprintf("(%.4f * %s)", coefficients[i], predictor_names[i]))
  if (i < length(coefficients)) {
    cat(" +\n")
  } else {
    cat("\n")
  }
}
# Predictions on training data
ridge_train_preds <- predict(ridge_final, s = best_lambda_ridge, newx = x_train)
ridge_train_rmse <- sqrt(mean((ridge_train_preds - y_train)^2))

# Calculate RSS and TSS for training data
rss_train <- sum((ridge_train_preds - y_train)^2) # Residual Sum of Squares
tss_train <- sum((y_train - mean(y_train))^2)    # Total Sum of Squares

# Calculate R-squared for training data
ridge_r2_train <- 1 - rss_train / tss_train


# Predictions on test data
x_test <- model.matrix(T_OR_Max1 ~ T_offset1 + Max1R13_1 + aveAllR13_1 +                                     T_RC_Dry1 + T_RC_Wet1 + canthi4Max1 + T_FHBC1 + T_FH_Max1 + 
                         T_Max1 + T_atm + Humidity + T_offset1:aveAllR13_1 +
                         T_offset1:T_FHBC1 + 
                         T_offset1:T_atm + aveAllR13_1:T_FHBC1 + 
                         aveAllR13_1:Humidity +
                         T_RC_Dry1:canthi4Max1 + T_RC_Dry1:T_FHBC1 +
                         T_RC_Dry1:T_Max1 + 
                         T_RC_Dry1:Humidity + T_RC_Wet1:canthi4Max1 +
                         T_RC_Wet1:T_Max1 + 
                         T_RC_Wet1:T_atm + T_RC_Wet1:Humidity + 
                         canthi4Max1:T_FH_Max1 + 
                         T_FHBC1:Humidity + T_FH_Max1:T_Max1 + T_FH_Max1:T_atm +
                         T_Max1:T_atm, test_data)[,-1]

y_test <- test_data$T_OR_Max1

ridge_test_preds <- predict(ridge_final, s = best_lambda_ridge, newx = x_test)
ridge_test_rmse <- sqrt(mean((ridge_test_preds - y_test)^2))


cat("Caculated value based on Ridge model:", "\n")
# Output R-squared
cat("R-squared :", ridge_r2_train, "\n")
# Output RMSE
cat("RMSE on Training Data:", ridge_train_rmse, "\n")
cat("RMSE on Testing Data:", ridge_test_rmse, "\n")

