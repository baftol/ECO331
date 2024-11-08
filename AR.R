# Load necessary libraries
library(tseries)
library(forecast)

# Step 1: Load and prepare the data
gdp_data <- read.csv("C:/Users/anura/Downloads/India-forecasting/GDP- India.csv")
gdp_data$log_GDP <- log(gdp_data$Value)
gdp_data$log_GDP_diff <- c(NA, diff(gdp_data$log_GDP, differences = 1))
gdp_data <- na.omit(gdp_data)
gdp_ts <- ts(gdp_data$log_GDP_diff, frequency = 4)

# -----------------------------------------------------
# One-Step-Ahead Forecasting with Optimal AR(p) Model
# -----------------------------------------------------
n <- length(gdp_ts)           # Total number of data points after differencing
train_size <- n - 8           # Initial training set size
mse_values_ar <- numeric(n - train_size)

# Rolling forecast loop for one-step-ahead AR model
for (i in train_size:(n - 1)) {
  # Define the training set (first `i` points)
  train_set <- gdp_ts[1:i]
  
  # Fit AR model with optimal lag order p selected by AIC
  ar_model <- auto.arima(train_set, max.q = 0, max.d = 0, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
  
  # Forecast the next point
  forecast_value <- forecast(ar_model, h = 1)$mean[1]
  
  # Calculate the MSE for this forecast
  actual_value <- gdp_ts[i + 1]
  mse_values_ar[i - train_size + 1] <- (forecast_value - actual_value)^2
}

# Calculate and print the average one-step-ahead MSE for AR model
average_mse_ar_one_step <- mean(mse_values_ar, na.rm = TRUE)
print(paste("Average One-Step-Ahead MSE for AR model:", average_mse_ar_one_step))


# -----------------------------------------------------
# Multi-Step-Ahead Forecasting with Optimal AR(p) Model
# -----------------------------------------------------
horizon <- 1               # Forecast horizon for multi-step-ahead
k <- 2                      # Number of neighbors for KNN
lags <- horizon             # Set lags equal to horizon

# Initialize vector to store cumulative MSE values for each iteration
cumulative_mse_iterations_ar <- numeric(horizon)

# Backward forecast loop
for (i in (n - 1):(n - 12)) {
  # Define the training set up to point `i`
  train_set <- gdp_ts[1:i]
  
  # Apply AR forecasting model with optimal lag order p
  ar_model <- auto.arima(train_set, max.q = 0, max.d = 0, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
  
  # Calculate the cumulative MSE for the forecasted horizon
  mse_value <- 0
  sum <- 0
  for (j in (i + 1):n) {
    actual_value <- gdp_ts[j]
    forecast_value <- forecast(ar_model, h = horizon)$mean[min(j - i, horizon)]
    sum <- sum + 1
    mse_value <- mse_value + (forecast_value - actual_value)^2
  }
  mse_value <- mse_value / sum
  horizon <- horizon + 1
  lags <- lags + 1
  
  # Store the MSE for this iteration
  cumulative_mse_iterations_ar[n - i] <- mse_value
}

# Calculate and print the average multi-step-ahead MSE for AR model
average_mse_ar_multi_step <- mean(cumulative_mse_iterations_ar, na.rm = TRUE)
print(paste("Average Multi-Step-Ahead MSE for AR model:", average_mse_ar_multi_step))
