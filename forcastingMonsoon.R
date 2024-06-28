# Load necessary libraries
library(forecast)
library(tidyverse)

rainfall_data <- 
  
# Assuming 'rainfall_data' is your dataset with the 'Year' and 'Jul' columns
rainfall_ts <- ts(rainfall_data$Jul, start = c(2000, 1), frequency = 1)

# Plot the data
plot(rainfall_ts)

# Fit an ARIMA model
fit <- auto.arima(rainfall_ts)

# Check diagnostics
checkresiduals(fit)

# Forecast for July 2024
forecast(fit, h = 1)
