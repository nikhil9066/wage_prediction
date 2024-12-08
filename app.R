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

# Main script execution
file_path <- "/Users/nikhilprao/Documents/Data/Wage.csv"
wage_data <- load_and_prepare_data(file_path)
plot_data_visualizations(wage_data)