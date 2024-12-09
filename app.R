load_libraries <- function() {
  libraries <- c(
    "caret", "car", "readr", "dplyr", "ggplot2", "GGally", "gridExtra",
    "grid", "glmnet", "Metrics", "rpart", "rpart.plot", "pROC", "tidyr", "reshape2",
    "randomForest","DiagrammeR", "xgboost", "adabag"
  )
  lapply(libraries, function(lib) {
    suppressMessages(suppressWarnings(require(lib, character.only = TRUE)))
  })
}

load_and_prepare_data <- function(file_path) {
  wage_data <- read_csv(file_path)
  wage_data <- wage_data %>%
    mutate(across(where(is.character), as.factor)) %>%
    select(where(~ !is.factor(.) || length(levels(.)) > 1))
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

# Add more data visualizations
add_data_visualizations <- function(data) {
  scatter_age_education <- ggplot(data, aes(x = age, y = wage, color = education)) +
    geom_point(alpha = 0.5) +
    labs(title = "Wage vs Age by Education", x = "Age", y = "Wage")
  
  wage_density <- ggplot(data, aes(x = wage, fill = education)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Wage by Education", x = "Wage", y = "Density")
  
  education_barplot <- ggplot(data, aes(x = education, fill = education)) +
    geom_bar() +
    labs(title = "Education Level Distribution", x = "Education Level", y = "Count")
  
  grid.arrange(scatter_age_education, wage_density, education_barplot, nrow = 2)
}

# Function for Multilinear Regression
fit_multilinear_regression <- function(data) {
  model <- lm(wage ~ ., data = data)
  print(summary(model))
  par(mfrow = c(2, 2))
  plot(model)
  return(model)
}

# Function to implement Random Forests
fit_random_forest <- function(data, formula) {
  rf_model <- randomForest(formula, data = data, importance = TRUE)
  print(rf_model)
  varImpPlot(rf_model, main = "Variable Importance (Random Forest)")
  plot(rf_model)
  return(rf_model)
}

# Function to implement XGBoost
fit_xgboost <- function(data, formula) {
  x_data <- model.matrix(formula, data)[, -1]  # Remove intercept
  y_data <- data[[as.character(formula[[2]])]]  # Extract response variable
  
  xgb_model <- xgboost(
    data = as.matrix(x_data), label = y_data, max.depth = 3,
    eta = 0.1, nrounds = 100, objective = "reg:squarederror", verbose = 0
  )
  #plot(xgb_model)
  print(xgb_model)
  return(xgb_model)
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

# Function to compute cross-validation for Lasso and Ridge
perform_cross_validation <- function(data, alpha) {
  x <- model.matrix(wage ~ . - 1, data = data)
  y <- data$wage
  cv_model <- cv.glmnet(x, y, alpha = alpha)
  plot(cv_model)
  title(ifelse(alpha == 1, "Lasso Cross-Validation", "Ridge Cross-Validation"))
  best_lambda <- cv_model$lambda.min
  cat("Best Lambda: ", best_lambda, "\n")
  return(best_lambda)
}

# Function to compare model metrics
compare_model_metrics <- function(actuals, predictions_list, model_names) {
  metrics <- data.frame(
    Model = model_names,
    RMSE = sapply(predictions_list, function(pred) rmse(actuals, pred)),
    MAE = sapply(predictions_list, function(pred) mae(actuals, pred))
  )
  
  print(metrics)
  ggplot(metrics, aes(x = Model)) +
    geom_bar(aes(y = RMSE, fill = "RMSE"), stat = "identity", position = "dodge") +
    geom_bar(aes(y = MAE, fill = "MAE"), stat = "identity", position = "dodge") +
    labs(title = "Model Performance Comparison", y = "Metric Value") +
    scale_fill_manual(values = c("RMSE" = "blue", "MAE" = "red"))
}

# Function to generate summary report
generate_summary_report <- function(metrics, filename = "model_summary_report.txt") {
  sink(filename)
  cat("Summary Report of Model Performance\n")
  cat("-----------------------------------\n")
  print(metrics)
  sink()
  cat("Summary report saved to ", filename, "\n")
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

# Main Script
load_libraries()

file_path <- "./Wage.csv"
wage_data <- load_and_prepare_data(file_path)

str(wage_data)
summary(wage_data)
# Check for missing values
sum(is.na(wage_data))
wage_data <- na.omit(wage_data)  # Remove rows with missing values

# Visualizations
add_data_visualizations(wage_data)
plot_data_visualizations(wage_data)

# Fit Models
fit_multilinear_regression(wage_data)
rf_model <- fit_random_forest(wage_data, wage ~ age + education + year)
xgb_model <- fit_xgboost(wage_data, wage ~ age + education + year)

xgb.plot.tree(model = xgb_model, trees = 0, show_node_id = TRUE)
dev.off()

par(mar = c(5, 5, 4, 2))
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix, main = "Feature Importance")
print(importance_matrix)

# Regularization Cross-Validation
best_lambda_lasso <- perform_cross_validation(wage_data, alpha = 1)
best_lambda_ridge <- perform_cross_validation(wage_data, alpha = 0)

# Model Comparison
predictions_list <- list(
  predict(rf_model, wage_data),
  predict(xgb_model, model.matrix(wage ~ ., data = wage_data)[, -1]),
  predict(ada_model, newdata = wage_data)$class  # Ensure compatibility
)

model_names <- c("Random Forest", "XGBoost", "AdaBoost")
metrics <- compare_model_metrics(actuals, predictions_list, model_names)

# Generate Summary Report
generate_summary_report(metrics)