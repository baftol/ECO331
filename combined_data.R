# Set the working directory
setwd("C:/Users/anura/Downloads/India-forecasting")

# Load yield curve data and rename the 'Value' columns for clarity
yield_3m <- read.csv("3-month.csv")
names(yield_3m)[names(yield_3m) == "Value"] <- "Yield_3m"

yield_3y <- read.csv("3-Year.csv")
names(yield_3y)[names(yield_3y) == "Value"] <- "Yield_3y"

yield_10y <- read.csv("10-Year.csv")
names(yield_10y)[names(yield_10y) == "Value"] <- "Yield_10y"

yield_5y <- read.csv("5-Year.csv")
names(yield_5y)[names(yield_5y) == "Value"] <- "Yield_5y"

yield_7y <- read.csv("7-Year.csv")
names(yield_7y)[names(yield_7y) == "Value"] <- "Yield_7y"
# Combine yield data row-wise (assumes same number of rows)
yield_data <- data.frame(
  Yield_3m = yield_3m$Yield_3m,
  Yield_3y = yield_3y$Yield_3y,
  Yield_10y = yield_10y$Yield_10y,
  Yield_5y = yield_5y$Yield_5y,
  Yield_7y = yield_7y$Yield_7y
)

# Calculate Yield Curve Components
yield_data$Level <- rowMeans(yield_data[, c("Yield_3m", "Yield_3y", "Yield_10y")], na.rm = TRUE)
yield_data$Slope <- yield_data$Yield_10y - yield_data$Yield_3m
yield_data$Curvature <- (2 * yield_data$Yield_3y) - (yield_data$Yield_3m + yield_data$Yield_10y)
#Load GDP
gdp_data <- read.csv("GDP- India.csv")
gdp_data$log_GDP <- log(gdp_data$Value)
gdp_data$log_GDP_diff <- c(NA, diff(gdp_data$log_GDP, differences = 1))
gdp_data <- data.frame(log_GDP_diff = na.omit(gdp_data$log_GDP_diff))

# Load macroeconomic variables
unemployment <- read.csv("unemployment-Rate India.csv")
names(unemployment)[names(unemployment) == "Value"] <- "Unemployment_Rate"

repo <- read.csv("Repo Rate- India.csv")
names(repo)[names(repo) == "Value"] <- "repo_Rate"

cpi <- read.csv("CPI- India.csv")
names(cpi)[names(cpi) == "Value"] <- "CPI"

capacity_utilization <- read.csv("Capacity Utilization- India.csv")
names(capacity_utilization)[names(capacity_utilization) == "Value"] <- "Capacity_Utilization"

# Take the first difference of CPI and Capacity Utilization
cpi$CPI_diff <- c(NA, diff(cpi$CPI))
capacity_utilization$Capacity_Utilization_diff <- c(NA, diff(capacity_utilization$Capacity_Utilization))

# Drop the first row of the differentiated CPI and Capacity Utilization to align with other data
cpi <- data.frame(CPI_diff = cpi$CPI_diff[-1])  # Ensure cpi remains a data frame
capacity_utilization <- data.frame(Capacity_Utilization_diff = capacity_utilization$Capacity_Utilization_diff[-1])  # Ensure capacity_utilization remains a data frame


# Combine all data into a single data frame
ultimate_data <- data.frame(
  Yield_3m = yield_data$Yield_3m,
  Yield_3y = yield_data$Yield_3y,
  Yield_5y = yield_data$Yield_5y,
  Yield_7y = yield_data$Yield_7y,
  Yield_10y = yield_data$Yield_10y,
  Level = yield_data$Level,
  Slope = yield_data$Slope,
  Curvature = yield_data$Curvature,
  Unemployment_Rate = unemployment$Unemployment_Rate,
  repo_Rate=repo$repo_Rate,
  CPI_diff = cpi$CPI_diff,
  Capacity_Utilization_diff = capacity_utilization$Capacity_Utilization_diff,
  log_GDP_diff = gdp_data$log_GDP_diff
)

# Display the first few rows of the ultimate dataset to check
head(ultimate_data)
