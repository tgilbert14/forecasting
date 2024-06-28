
## this is to plot temp and precip separately ----------------------------------

## temp data Jul ----
temp_data <- read_csv(paste0(app_path,"/data/TucsonNOAA_tempMeanData.csv"))
temp_ts <- ts(temp_data$Jul, start = c(min(temp_data$Year), 1), frequency = 1)
autoplot(temp_ts, main = "July")

# Fit an ARIMA model for temperature
fit_temp <- auto.arima(temp_ts)
# Forecast future values for temperature
future_forecast_temp <- forecast(fit_temp, h=1)

## precip data Jul ----
rainfall_data <- read_csv(paste0(app_path,"/data/TucsonNOAA_percipData.csv"))
rainfall_ts <- ts(rainfall_data$Jul, start = c(min(rainfall_data$Year), 1), frequency = 1)
autoplot(rainfall_ts, main = "July")

# Fit an ARIMA model for rainfall
fit_rainfall <- auto.arima(rainfall_ts)
# Forecast future values for rainfall
future_forecast_rainfall <- forecast(fit_rainfall, h=1)

# Plot the forecasts
plot(future_forecast_rainfall, main="Rainfall Forecast")
plot(future_forecast_temp, main="Temperature Forecast")

# Save the forecast values
write.csv(future_forecast_rainfall$mean, file="outputs/forecasted_rainfall_values.csv")
write.csv(future_forecast_temp$mean, file="outputs/forecasted_temperature_values.csv")

## save start of year for time series
start_year <- min(temp_data$Year)
## plot all data
temp_data <- temp_data %>%
  dplyr::select(!Year)
temp_ts <- ts(temp_data, start = c(start_year, 1), frequency = 1)
p.t<- autoplot(temp_ts)

rainfall_data <- rainfall_data %>% 
  dplyr::select(!c(Year,Annual))
rainfall_ts <- ts(rainfall_data, start = c(start_year, 1), frequency = 1)
p.r<- autoplot(rainfall_ts)

## example of fancier plot
library(plotly)
# Convert it to an interactive Plotly object
interactive_plot <- ggplotly(p.r)
# Display the interactive plot
interactive_plot








# Fri Jun 28 13:37:52 2024 ------------------------------

## work in progress below....
## adding temp data to forecast -> Does not work ----
library(vars)
library(tidyverse)

## temp data
temp_data <- read_csv(paste0(app_path,"/data/TucsonNOAA_tempMeanData.csv"))
## save start of year for time series
start_year <- min(temp_data$Year)
temp_data <- temp_data %>%
  dplyr::select(!Year)
#View(temp_data)
temp_ts <- ts(temp_data, start = c(start_year, 1), frequency = 12)
autoplot(temp_ts)

## precip data
rainfall_data <- read_csv(paste0(app_path,"/data/TucsonNOAA_percipData.csv"))
rainfall_data <- rainfall_data %>% 
  dplyr::select(!c(Year,Annual))
#View(rainfall_data)
rainfall_ts <- ts(rainfall_data, start = c(start_year, 1), frequency = 12)
autoplot(rainfall_ts)

data_df <- data.frame(rainfall=rainfall_ts, temperature=temp_ts)
#View(data_df)

# clean up df... no NA's (get rid of 2024 for now)
data_df_minus2024 <- head(data_df, -1)
# make all zeros trace (0.1)
data_df_minus2024[data_df_minus2024 == 0] <- 0.01
#View(data_df_minus2024)

## filter for only 1 month...
data_month <- data_df_minus2024 %>% 
  dplyr::select(contains("Jul"))
View(data_month)
# Convert the data frame to a time series object
data_ts <- ts(data_month, start=c(start_year, 1), frequency=1)
#View(data_ts)
# Selecting P lag order
var_select_results <- VARselect(data_ts, lag.max=20, type="both")
optimal_lag <- var_select_results$selection

# Fit a VAR model
var_model <- VAR(data_ts, p=1) # 'p' is the lag order, adjust based on your data
# Forecast future values
forecast_results <- predict(var_model, n.ahead=1) # 'n.ahead' is the forecast horizon

# Extract the forecast for rainfall
rainfall_forecast <- forecast_results$fcst$rainfall

# Plot the forecast
plot(rainfall_forecast)