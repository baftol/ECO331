# Load necessary libraries
library(tseries)
library(forecast)

# Step 1: Load and prepare the data
final_data <- read.csv("C:/Users/anura/Downloads/India-forecasting/final_data.csv")
gdp_ts <- ts(final_data$log_GDP_diff, frequency = 4)
covariates <- as.matrix(final_data[, c("CPI_diff", "Unemployment_Rate", "Capacity_Utilization_diff", "Yield_5y","Yield_7y","repo_Rate")])

# -----------------------------------------------------
# One-Step-Ahead Forecasting for All Models
# -----------------------------------------------------
n <- length(gdp_ts)           # Total number of data points after differencing
train_size <- n - 8           # Initial training set size

# Initialize variables to store MSE values
mse_values_arx <- numeric(n - train_size)
mse_values_sarimax <- numeric(n - train_size)
mse_values_lr <- numeric(n - train_size)
mse_values_mean_lr_sarimax <- numeric(n - train_size)  # Store MSE for Mean LR-SARIMAX

for (i in train_size:(n - 1)) {
  # Define the training set (first `i` points)
  train_set <- gdp_ts[2:i]
  train_cov <- covariates[1:(i - 1), , drop = FALSE]  # Covariates lagged by 1
  
  # ----- ARX Model -----
  arx_model <- auto.arima(train_set, xreg = train_cov, d = 0, D = 0, max.q = 0, seasonal = FALSE, stepwise = TRUE)
  forecast_value_arx <- forecast(arx_model, xreg = as.matrix(covariates[i, , drop = FALSE]), h = 1)$mean[1]
  mse_values_arx[i - train_size + 1] <- (forecast_value_arx - gdp_ts[i + 1])^2
  
  # ----- SARIMAX Model -----
  sarimax_model <- auto.arima(train_set, xreg = train_cov, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
  forecast_value_sarimax <- forecast(sarimax_model, xreg = as.matrix(covariates[i, , drop = FALSE]), h = 1)$mean[1]
  mse_values_sarimax[i - train_size + 1] <- (forecast_value_sarimax - gdp_ts[i + 1])^2
  
  # ----- Linear Regression Model -----
  lm_model <- lm(train_set ~ ., data = data.frame(train_set, train_cov))
  forecast_value_lr <- predict(lm_model, newdata = data.frame(covariates[i, , drop = FALSE]))
  mse_values_lr[i - train_size + 1] <- (forecast_value_lr - gdp_ts[i + 1])^2
  
  # ----- Mean LR-SARIMAX Prediction -----
  mean_forecast_value <- (forecast_value_sarimax + forecast_value_lr) / 2
  mse_values_mean_lr_sarimax[i - train_size + 1] <- (mean_forecast_value - gdp_ts[i + 1])^2
}

# Calculate average one-step-ahead MSEs
average_mse_arx_one_step <- mean(mse_values_arx, na.rm = TRUE)
average_mse_sarimax_one_step <- mean(mse_values_sarimax, na.rm = TRUE)
average_mse_lr_one_step <- mean(mse_values_lr, na.rm = TRUE)
average_mse_mean_lr_sarimax_one_step <- mean(mse_values_mean_lr_sarimax, na.rm = TRUE)

# -----------------------------------------------------
# Multi-Step-Ahead Forecasting for All Models
# -----------------------------------------------------
horizon <- 1  # Start with a horizon of 1
mse_values_arx_multi <- numeric(12)
mse_values_sarimax_multi <- numeric(12)
mse_values_lr_multi <- numeric(12)
mse_values_mean_lr_sarimax_multi <- numeric(12)

for (i in (n - 1):(n - 12)) {
  current_horizon <- horizon
  train_set <- gdp_ts[(current_horizon + 1):i]  # Lag target variable by current_horizon
  train_cov <- covariates[1:(i - current_horizon), , drop = FALSE]  # Lag covariates by current_horizon
  
  # ----- ARX Model -----
  arx_model <- auto.arima(train_set, xreg = train_cov, d = 0, D = 0, max.q = 0, seasonal = FALSE, stepwise = TRUE)
  mse_value_arx <- 0
  sum <- 0
  for (j in (i + 1):n) {
    actual_value <- gdp_ts[j]
    forecast_value_arx <- forecast(arx_model, xreg = as.matrix(covariates[j - current_horizon, , drop = FALSE]), h = current_horizon)$mean[1]
    mse_value_arx <- mse_value_arx + (forecast_value_arx - actual_value)^2
    sum <- sum + 1
  }
  mse_values_arx_multi[n - i] <- mse_value_arx / sum
  
  # ----- SARIMAX Model -----
  sarimax_model <- auto.arima(train_set, xreg = train_cov, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
  mse_value_sarimax <- 0
  for (j in (i + 1):n) {
    actual_value <- gdp_ts[j]
    forecast_value_sarimax <- forecast(sarimax_model, xreg = as.matrix(covariates[j - current_horizon, , drop = FALSE]), h = current_horizon)$mean[1]
    mse_value_sarimax <- mse_value_sarimax + (forecast_value_sarimax - actual_value)^2
  }
  mse_values_sarimax_multi[n - i] <- mse_value_sarimax / sum
  
  # ----- Linear Regression Model -----
  lm_model <- lm(train_set ~ ., data = data.frame(train_set, train_cov))
  mse_value_lr <- 0
  for (j in (i + 1):n) {
    actual_value <- gdp_ts[j]
    forecast_value_lr <- predict(lm_model, newdata = data.frame(covariates[j - current_horizon, , drop = FALSE]))
    mse_value_lr <- mse_value_lr + (forecast_value_lr - actual_value)^2
  }
  mse_values_lr_multi[n - i] <- mse_value_lr / sum
  
  # ----- Mean LR-SARIMAX Prediction -----
  mse_value_mean_lr_sarimax <- 0
  for (j in (i + 1):n) {
    actual_value <- gdp_ts[j]
    forecast_value_sarimax <- forecast(sarimax_model, xreg = as.matrix(covariates[j - current_horizon, , drop = FALSE]), h = current_horizon)$mean[1]
    forecast_value_lr <- predict(lm_model, newdata = data.frame(covariates[j - current_horizon, , drop = FALSE]))
    mean_forecast_value <- (forecast_value_sarimax + forecast_value_lr) / 2
    mse_value_mean_lr_sarimax <- mse_value_mean_lr_sarimax + (mean_forecast_value - actual_value)^2
  }
  mse_values_mean_lr_sarimax_multi[n - i] <- mse_value_mean_lr_sarimax / sum
  
  horizon <- horizon + 1
}

# Calculate average multi-step-ahead MSEs
average_mse_arx_multi <- mean(mse_values_arx_multi, na.rm = TRUE)
average_mse_sarimax_multi <- mean(mse_values_sarimax_multi, na.rm = TRUE)
average_mse_lr_multi <- mean(mse_values_lr_multi, na.rm = TRUE)
average_mse_mean_lr_sarimax_multi <- mean(mse_values_mean_lr_sarimax_multi, na.rm = TRUE)

# -----------------------------------------------------
# Combine Results into Table for Scenario 1
# -----------------------------------------------------
scenario_5_table <- data.frame(
  Metric = c("One_Step_Ahead_MSE", "Multi_Step_Ahead_MSE"),
  SARIMAX = c(average_mse_sarimax_one_step, average_mse_sarimax_multi),
  LR = c(average_mse_lr_one_step, average_mse_lr_multi),
  Mean_LR_SARIMAX = c(average_mse_mean_lr_sarimax_one_step, average_mse_mean_lr_sarimax_multi),
  ARX = c(average_mse_arx_one_step, average_mse_arx_multi)
)

# Convert all numeric columns to scientific notation
scenario_5_table[, -1] <- lapply(scenario_5_table[, -1], function(x) format(x, scientific = TRUE, digits = 4))

# Display the formatted table
print(scenario_5_table)

# -----------------------------------------------------
# Display Table with kableExtra
# -----------------------------------------------------
scenario_5_table %>%
  kbl(
    caption = "Scenario 1: Comparison of Average MSE in One-Step vs. Multi-Step Forecasting"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
