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
  # One-Step-Ahead Forecasting with Optimal SARIMA Model
  # -----------------------------------------------------
  n <- length(gdp_ts)           # Total number of data points after differencing
  train_size <- n - 8           # Initial training set size
  mse_values_sarima <- numeric(n - train_size)
  
  # Rolling forecast loop for one-step-ahead SARIMA model
  for (i in train_size:(n - 1)) {
    # Define the training set (first `i` points)
    train_set <- gdp_ts[1:i]
    
    # Fit SARIMA model with seasonal period s = 4 using stepwise AIC minimization
    sarima_model <- auto.arima(train_set, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
    
    # Forecast the next point
    forecast_value <- forecast(sarima_model, h = 1)$mean[1]
    
    # Calculate the MSE for this forecast
    actual_value <- gdp_ts[i + 1]
    mse_values_sarima[i - train_size + 1] <- (forecast_value - actual_value)^2
  }
  
  # Calculate and print the average one-step-ahead MSE for SARIMA model
  average_mse_sarima_one_step <- mean(mse_values_sarima, na.rm = TRUE)
  print(paste("Average One-Step-Ahead MSE for SARIMA model:", average_mse_sarima_one_step))
  
  
  # -----------------------------------------------------
  # Multi-Step-Ahead Forecasting with Optimal SARIMA Model
  # -----------------------------------------------------
  horizon <- 1               # Start horizon for multi-step-ahead forecasting
  cumulative_mse_iterations_sarima <- numeric(12)
  
  # Backward forecast loop for multi-step-ahead SARIMA model
  for (i in (n - 1):(n - 12)) {
    # Define the training set up to point `i`
    train_set <- gdp_ts[1:i]
    
    # Apply SARIMA model with seasonal period s = 4 using stepwise AIC minimization
    sarima_model <- auto.arima(train_set, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
    
    # Calculate the cumulative MSE for the forecasted horizon
    mse_value <- 0
    sum <- 0
    for (j in (i + 1):n) {
      actual_value <- gdp_ts[j]
      forecast_value <- forecast(sarima_model, h = horizon)$mean[min(j - i, horizon)]
      sum <- sum + 1
      mse_value <- mse_value + (forecast_value - actual_value)^2
    }
    mse_value <- mse_value / sum
    horizon <- horizon + 1
    
    # Store the MSE for this iteration
    cumulative_mse_iterations_sarima[n - i] <- mse_value
  }
  
  # Calculate and print the average multi-step-ahead MSE for SARIMA model
  average_mse_sarima_multi_step <- mean(cumulative_mse_iterations_sarima, na.rm = TRUE)
  print(paste("Average Multi-Step-Ahead MSE for SARIMA model:", average_mse_sarima_multi_step))
