# Load necessary libraries
library(caret)
library(car)
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(grid)
library(glmnet)
library(Metrics)
library(rpart)
library(rpart.plot)
library(pROC)
library(tidyr)
library(reshape2)

# Load and prepare the data
load_and_prepare_data <- function(file_path) {
  wage_data <- read_csv(file_path)
  wage_data <- wage_data %>%
    mutate_if(is.character, as.factor) %>%
    select_if(~!is.factor(.) || length(levels(.)) > 1)
  return(wage_data)
}

# Generate and plot visualizations
plot_data_visualizations <- function(data) {
  wage_histogram <- ggplot(data, aes(x = wage)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    ggtitle("Distribution of Wage") +
    xlab("Wage") +
    ylab("Frequency")
  
  wage_education_boxplot <- ggplot(data, aes(x = education, y = wage, fill = education)) +
    geom_boxplot() +
    ggtitle("Wage Distribution by Education Level") +
    xlab("Education Level") +
    ylab("Wage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  wage_age_scatterplot <- ggplot(data, aes(x = age, y = wage)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    ggtitle("Wage vs Age") +
    xlab("Age") +
    ylab("Wage")
  
  correlation_plot <- ggpairs(data[, c("year", "age", "logwage", "wage")])
  
  # Arrange plots
  wage_histogram_grob <- ggplotGrob(wage_histogram)
  wage_education_boxplot_grob <- ggplotGrob(wage_education_boxplot)
  wage_age_scatterplot_grob <- ggplotGrob(wage_age_scatterplot)
  correlation_plot_grob <- GGally::ggmatrix_gtable(correlation_plot)
  
  grid.arrange(
    wage_histogram_grob,
    wage_education_boxplot_grob,
    wage_age_scatterplot_grob,
    correlation_plot_grob,
    nrow = 2
  )
}

# Function for Multilinear Regression
fit_multilinear_regression <- function(data) {
  model <- lm(wage ~ ., data = data)
  print(summary(model))
  par(mfrow = c(2, 2))
  plot(model)
  return(model)
}

# Function for Ridge Regression
fit_ridge_regression <- function(data) {
  # Prepare the model matrix (predictors) and response vector
  x <- model.matrix(wage ~ . - 1, data = data)  # Remove the intercept for glmnet
  y <- data$wage
  
  # Fit ridge regression model with a sequence of lambda values
  ridge_model <- glmnet(x, y, alpha = 0)  # alpha = 0 for ridge regression
  
  # Plot the coefficient path for ridge regression
  plot(ridge_model, xvar = "lambda", label = TRUE)
  title("Coefficient Path for Ridge Regression")
  
  # Perform cross-validation to find the optimal lambda
  cv_ridge <- cv.glmnet(x, y, alpha = 0)
  plot(cv_ridge)
  title("Cross-Validation for Selecting Lambda")
  
  # Extract the best lambda value (minimizing cross-validated mean squared error)
  best_lambda <- cv_ridge$lambda.min
  cat("Best lambda: ", best_lambda, "\n")
  
  # Return the coefficients at the best lambda
  return(coef(ridge_model, s = best_lambda))
}

# Function for Lasso Regression (L1 Regularization)
fit_lasso_regression <- function(data) {
  x <- model.matrix(wage ~ . - 1, data = data)
  y <- data$wage
  
  # Fit Lasso model
  lasso_model <- glmnet(x, y, alpha = 1)  # alpha = 1 for Lasso
  cv_lasso <- cv.glmnet(x, y, alpha = 1)
  plot(cv_lasso)
  title("Cross-Validation for Selecting Lambda in Lasso")
  
  best_lambda <- cv_lasso$lambda.min
  cat("Best lambda for Lasso: ", best_lambda, "\n")
  
  return(list(model = lasso_model, coefficients = coef(lasso_model, s = best_lambda)))
}

# Function for Ridge Regression (L2 Regularization)
fit_ridge_regression <- function(data) {
  x <- model.matrix(wage ~ . - 1, data = data)
  y <- data$wage
  
  ridge_model <- glmnet(x, y, alpha = 0)  # alpha = 0 for Ridge
  cv_ridge <- cv.glmnet(x, y, alpha = 0)
  plot(cv_ridge)
  title("Cross-Validation for Selecting Lambda in Ridge")
  
  best_lambda <- cv_ridge$lambda.min
  cat("Best lambda for Ridge: ", best_lambda, "\n")
  
  return(list(model = ridge_model, coefficients = coef(ridge_model, s = best_lambda)))
}

# Function to compare model performances
compare_model_performance <- function(data, lasso_results, ridge_results) {
  x <- model.matrix(wage ~ . - 1, data = data)  # Create model matrix for predictions
  actuals <- data$wage  # Actual wage values for comparison
  
  # Predictions using the best lambda found from CV
  lasso_pred <- predict(lasso_results$model, newx = x, s = lasso_results$model$lambda.min)
  ridge_pred <- predict(ridge_results$model, newx = x, s = ridge_results$model$lambda.min)
  
  # Calculate RMSE for each model
  lasso_rmse <- rmse(actuals, lasso_pred)
  ridge_rmse <- rmse(actuals, ridge_pred)
  
  # Output results
  cat("RMSE for Lasso: ", lasso_rmse, "\n")
  cat("RMSE for Ridge: ", ridge_rmse, "\n")
}

# Function to create and visualize a Regression Tree
fit_and_plot_regression_tree <- function(data, formula) {
  # Fit the regression tree model
  tree_model <- rpart(formula, data = data, method = "anova")
  
  # Plot the regression tree showing the mean and percentage of observations
  rpart.plot(tree_model, main = "Regression Tree for Wage Prediction", 
             extra = 101,  # Display the mean of the dependent variable and percentage of observations
             under = TRUE, # Place node numbers underneath the node labels
             faclen = 0)   # Don't abbreviate factor levels
  
  return(tree_model)
}

# Main script execution
file_path <- "/Users/nikhilprao/Documents/Data/Wage.csv"
wage_data <- load_and_prepare_data(file_path)
plot_data_visualizations(wage_data)
fit_multilinear_regression(wage_data)
linear_model <- fit_multilinear_regression(wage_data)
ridge_coefficients <- fit_ridge_regression(wage_data)
print(ridge_coefficients)

# Fit models
lasso_results <- fit_lasso_regression(wage_data)
ridge_results <- fit_ridge_regression(wage_data)

# Compare models
#compare_model_performance(wage_data, lasso_results, ridge_results)
set.seed(123) # For reproducibility
# Assuming wage_data is already loaded and prepared
tree_model <- fit_and_plot_regression_tree(wage_data, wage ~ age + education)

# and 'education' is already a factor with levels appropriately set as in your output:
formula <- wage ~ education + age
tree_model <- rpart(formula, data = wage_data, method = "anova")

# Visualize the tree with detailed node information
rpart.plot(tree_model, main = "Regression Tree for Wage Prediction", 
           extra = 101,  # Show the mean and percentage of observations in nodes
           under = TRUE, faclen = 0)

# Print the model summary to see the text description of each node
print(tree_model)

calculate_roc_auc <- function(actual, predicted) {
  library(pROC)
  roc_response <- roc(actual, predicted)
  plot(roc_response, main="ROC Curve")
  auc(roc_response)
}

compare_model_performance <- function(data, lasso_results, ridge_results, linear_model, tree_model) {
  x <- model.matrix(wage ~ . - 1, data = data)
  actuals <- data$wage
  
  # Get predictions for all models
  lasso_pred <- predict(lasso_results$model, newx = x, s = lasso_results$model$lambda.min)
  ridge_pred <- predict(ridge_results$model, newx = x, s = ridge_results$model$lambda.min)
  linear_pred <- predict(linear_model, newx = x)
  tree_pred <- predict(tree_model, newx = data)
  
  # Calculate RMSE and MAE for each model
  metrics <- rbind(
    Lasso = c(RMSE = rmse(actuals, lasso_pred), MAE = mae(actuals, lasso_pred)),
    Ridge = c(RMSE = rmse(actuals, ridge_pred), MAE = mae(actuals, ridge_pred)),
    Linear = c(RMSE = rmse(actuals, linear_pred), MAE = mae(actuals, linear_pred)),
    Tree = c(RMSE = rmse(actuals, tree_pred), MAE = mae(actuals, tree_pred))
  )
  
  print(metrics)
}


metrics <- compare_model_performance(wage_data, lasso_results, ridge_results, linear_model, tree_model)

# Convert the matrix 'metrics' to a long format data frame suitable for ggplot2
metrics_long <- melt(as.data.frame(metrics), variable.name = "Metric", value.name = "Value")
metrics_long$Model <- rep(c("Lasso", "Ridge", "Linear", "Tree"), each = 2)  # Repeating model names

# Define the plotting function
plot_model_comparison <- function(metrics_df) {
  ggplot(metrics_df, aes(x = Model, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    theme_minimal() +
    labs(title = "Model Performance Comparison", y = "Metric Value") +
    scale_fill_brewer(palette = "Pastel1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.8), vjust = -0.5)
}

# Plot the metrics
plot_model_comparison(metrics_long)


# New observation data
new_data <- data.frame(
  year = 2003,
  age = 28,
  maritl = "1. Never Married",
  race = "3. Asian",
  education = "4. College Grad",
  region = "2. Middle Atlantic",
  jobclass = "2. Information",
  health = "2. >=Very Good",
  health_ins = "1. Yes",
  logwage = 5  # logwage is provided, if this should be predicted remove it from here
)

# Convert categorical variables to factors with levels matching the original training data
new_data$maritl <- factor(new_data$maritl, levels = levels(wage_data$maritl))
new_data$race <- factor(new_data$race, levels = levels(wage_data$race))
new_data$education <- factor(new_data$education, levels = levels(wage_data$education))
new_data$region <- factor(new_data$region, levels = levels(wage_data$region))
new_data$jobclass <- factor(new_data$jobclass, levels = levels(wage_data$jobclass))
new_data$health <- factor(new_data$health, levels = levels(wage_data$health))
new_data$health_ins <- factor(new_data$health_ins, levels = levels(wage_data$health_ins))

# Predict the wage using the chosen model (assuming 'linear_model' is your fitted linear regression model)
predicted_wage <- predict(linear_model, newdata = new_data)

# Compare the predicted wage to the original wage
original_wage <- 148.413159102577  # The original wage value to compare against
prediction_error <- abs(predicted_wage - original_wage)

# Output comparison
cat("Predicted Wage: ", predicted_wage, "\n")
cat("Original Wage: ", original_wage, "\n")
cat("Prediction Error: ", prediction_error, "\n")



# Predict the wage using the tree model
predicted_wage_tree <- predict(tree_model, newdata = new_data)

# Compare the predicted wage to the original wage
original_wage <- 148.413159102577  # The original wage value to compare against
prediction_error_tree <- abs(predicted_wage_tree - original_wage)

# Output comparison
cat("Predicted Wage from Tree Model: ", predicted_wage_tree, "\n")
cat("Original Wage: ", original_wage, "\n")
cat("Prediction Error: ", prediction_error_tree, "\n")