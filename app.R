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