load_libraries <- function() {
  libraries <- c(
    "caret", "car", "readr", "dplyr", "ggplot2", "GGally", "gridExtra",
    "grid", "glmnet", "Metrics", "rpart", "rpart.plot", "pROC", "tidyr", "reshape2",
    "randomForest", "DiagrammeR", "xgboost"
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
  # Wage Histogram
  wage_histogram <- ggplot(data, aes(x = wage)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    ggtitle("Distribution of Wage") +
    xlab("Wage") +
    ylab("Frequency") +
    theme_minimal(base_size = 12)
  
  # Wage by Education Boxplot
  wage_education_boxplot <- ggplot(data, aes(x = education, y = wage, fill = education)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 1, notch = TRUE) +
    ggtitle("Wage Distribution by Education Level") +
    xlab("Education Level") +
    ylab("Wage") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  # Wage vs Age Scatterplot
  wage_age_scatterplot <- ggplot(data, aes(x = age, y = wage)) +
    geom_point(alpha = 0.6, color = "darkblue") +
    geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
    ggtitle("Wage vs Age") +
    xlab("Age") +
    ylab("Wage") +
    theme_minimal(base_size = 12)
  
  # Correlation Plot
  correlation_plot <- ggpairs(data[, c("year", "age", "logwage", "wage")],
                               lower = list(continuous = wrap("smooth", color = "blue", alpha = 0.3)),
                               diag = list(continuous = wrap("densityDiag", fill = "blue", alpha = 0.3)),
                               upper = list(continuous = wrap("cor", size = 4))) +
    ggtitle("Correlation Plot")
  
  # Convert ggmatrix to a grob
  correlation_grob <- GGally::ggmatrix_gtable(correlation_plot)
  
  # Improved Layout
  grid.arrange(
    ggplotGrob(wage_histogram),               # Top-left: Wage Histogram
    ggplotGrob(wage_education_boxplot),       # Top-right: Wage by Education
    ggplotGrob(wage_age_scatterplot),         # Bottom-left: Wage vs Age
    correlation_grob,                         # Bottom-right: Correlation Plot
    layout_matrix = rbind(
      c(1, 2),  # Wage Histogram and Education Boxplot side-by-side
      c(3, 4)   # Wage vs Age and Correlation Plot side-by-side
    ),
    top = textGrob("Data Visualizations", gp = gpar(fontsize = 18, fontface = "bold"))
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
# Remove rows with missing values
wage_data <- na.omit(wage_data)

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

## Tuning of RF
# Corrected Random Forest Hyperparameter Tuning using caret
tune_random_forest <- function(data, formula) {
  # Define the training control for cross-validation
  train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
  
  # Define the hyperparameters to tune (mtry) - number of variables to try at each split
  tune_grid <- expand.grid(mtry = c(2, 3, 4))  # Tuning 'mtry' parameter
  
  # Train the random forest model with tuning
  rf_tune_model <- train(formula, data = data, method = "rf",
                         trControl = train_control, tuneGrid = tune_grid,
                         importance = TRUE)
  
  print(rf_tune_model)  # Print model performance results
  varImpPlot(rf_tune_model$finalModel, main = "Variable Importance (Tuned Random Forest)")
  
  return(rf_tune_model$finalModel)
}

# Example usage:
rf_tuned_model <- tune_random_forest(wage_data, wage ~ age + education + year)

## XGB tuning
# Function to tune hyperparameters for XGBoost
tune_xgboost <- function(data, formula) {
  # Define the training control for cross-validation
  train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
  
  # Define the hyperparameter grid to tune for XGBoost
  tune_grid <- expand.grid(
    nrounds = c(50, 100, 200),               # Number of boosting rounds
    max_depth = c(3, 6, 9),                   # Maximum tree depth
    eta = c(0.01, 0.1, 0.2),                 # Learning rate
    gamma = c(0, 1),                          # Minimum loss reduction
    colsample_bytree = c(0.5, 0.7, 1),       # Fraction of features to sample per tree
    min_child_weight = c(1, 5, 10),           # Minimum sum of instance weight in a child
    subsample = c(0.5, 0.7, 1)                # Subsampling ratio for training
  )
  
  # Train the XGBoost model with tuning
  xgb_tune_model <- train(formula, data = data, method = "xgbTree",
                          trControl = train_control, tuneGrid = tune_grid,
                          verbose = 0)  # Silent mode during training
  
  print(xgb_tune_model)  # Print model performance results
  return(xgb_tune_model$finalModel)  # Return the best-tuned model
}

xgb_tuned_model <- tune_xgboost(wage_data, wage ~ age + education + year)

# Model Comparison
actuals <- wage_data$wage

rf_predictions <- predict(rf_model, wage_data)
x_data <- model.matrix(wage ~ age + education + year, data = wage_data)[, -1]
xgb_predictions <- predict(xgb_model, newdata = as.matrix(x_data))
lm_predictions <- predict(fit_multilinear_regression(wage_data), newdata = wage_data)

# Create the model matrix for wage_data (excluding the intercept column)
x_data <- model.matrix(wage ~ age + education + year, data = wage_data)[, -1]
# Ensure that the new data for prediction matches the training data structure
rf_tuned_predictions <- predict(rf_tuned_model, newdata = x_data)
xgb_tuned_predictions <- predict(xgb_tuned_model, newdata = as.matrix(x_data))


predictions_list <- list(rf_predictions, xgb_predictions, lm_predictions, rf_tuned_predictions, xgb_tuned_predictions)
model_names <- c("Random Forest", "XGBoost", "Linear Regression", "Tuned Random Forest", "Tuned XGBoost")
metrics <- compare_model_metrics(actuals, predictions_list, model_names)

# Generate Summary Report
generate_summary_report(metrics)

# Data provided
metrics <- data.frame(
  Model = c("Random Forest", "XGBoost", "Linear Regression", "Tuned Random Forest", "Tuned XGBoost"),
  RMSE = c(33.53792, 34.08860, 12.57193, 34.57570, 34.47375),
  MAE = c(22.789318, 23.312429, 7.030531, 23.623297, 23.511137)
)

# Reshape the data to long format for plotting
metrics_long <- metrics %>%
  pivot_longer(cols = c(RMSE, MAE), names_to = "Metric", values_to = "Value")

# Create the line graph
ggplot(metrics_long, aes(x = Model, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +  # Line for each metric
  geom_point(size = 3) + # Points for each model
  labs(title = "Model Performance Comparison", y = "Metric Value", x = "Model") +
  scale_color_manual(values = c("RMSE" = "blue", "MAE" = "red")) +
  theme_minimal()