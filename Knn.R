# Load necessary libraries
library(tseries)
library(forecast)
library(tsfknn)

# Step 1: Load and prepare the data
gdp_data <- read.csv("C:/Users/anura/Downloads/India-forecasting/GDP- India.csv")
gdp_data$log_GDP <- log(gdp_data$Value)
gdp_data$log_GDP_diff <- c(NA, diff(gdp_data$log_GDP, differences = 1))
gdp_data <- na.omit(gdp_data)
gdp_ts <- ts(gdp_data$log_GDP_diff, frequency = 4)

# -----------------------------------------------------
# One-Step-Ahead Forecasting with Optimal k
# -----------------------------------------------------
n <- length(gdp_ts)
train_size <- n - 8
lags <- 1
horizon <- 1
k_values <- 1:5  # Range of k values to search for optimal k
best_k <- NULL
lowest_mse <- Inf
mse_values_knn <- numeric(n-train_size)
# Grid search for optimal k
for (k in k_values) {
  mse_values <- numeric(n - train_size)
  
  for (i in train_size:(n - 1)) {
    train_set <- gdp_ts[1:i]
    
    # Use `knn_forecasting` with the current k
    knn_model <- knn_forecasting(train_set, h = horizon, k = k, lags = lags, transform = "none")
    
    forecast_value <- knn_model$prediction[1]
    actual_value <- gdp_ts[i + 1]
    mse_values[i - train_size + 1] <- (forecast_value - actual_value)^2
  }
  
  average_mse <- mean(mse_values, na.rm = TRUE)
  
  # Update best k if the current k gives a lower MSE
  if (average_mse < lowest_mse) {
    lowest_mse <- average_mse
    best_k <- k
    mse_values_knn = mse_values
  }
}

print(paste("Optimal k for One-Step-Ahead Forecast:", best_k))
print(paste("Lowest One-Step-Ahead MSE for Optimal k:", lowest_mse))


# -----------------------------------------------------
# Multi-Step-Ahead Forecasting with Optimal k
# -----------------------------------------------------
optimal_horizon <- 12
cumulative_mse_iterations <- numeric(optimal_horizon)
best_k_multi_step <- NULL
lowest_mse_multi_step <- Inf

# Grid search for optimal k in multi-step-ahead forecasting
for (k in k_values) {
  horizon <- 1
  lags <- horizon
  mse_values <- numeric(optimal_horizon)
  
  for (i in (n - 1):(n - optimal_horizon)) {
    train_set <- gdp_ts[1:i]
    
    # Apply KNN forecasting model with the current k
    knn_model <- knn_forecasting(train_set, h = horizon, k = k, lags = lags, transform = "none")
    
    mse_value <- 0
    sum <- 0
    for (j in (i + 1):n) {
      actual_value <- gdp_ts[j]
      forecast_value <- knn_model$prediction[j - i]
      sum <- sum + 1
      mse_value <- mse_value + (forecast_value - actual_value)^2
    }
    mse_value <- mse_value / sum
    mse_values[horizon] <- mse_value
    
    horizon <- horizon + 1
    lags <- lags + 1
  }
  
  # Calculate the average MSE for this k
  average_mse_multi_step <- mean(mse_values, na.rm = TRUE)
  
  # Update best k if the current k gives a lower MSE
  if (average_mse_multi_step < lowest_mse_multi_step) {
    lowest_mse_multi_step <- average_mse_multi_step
    best_k_multi_step <- k
    cumulative_mse_iterations <- mse_values
  }
}

print(paste("Optimal k for Multi-Step-Ahead Forecast:", best_k_multi_step))
print(paste("Lowest Multi-Step-Ahead MSE for Optimal k:", lowest_mse_multi_step))
