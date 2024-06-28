## forecasting for 1 month

# Load necessary libraries
library(forecast)
library(tidyverse)

## set environment path and save
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
app_path <<- getwd()

rainfall_data <- read_csv(paste0(app_path,"/data/TucsonNOAA_percipData.csv"))
#View(rainfall_data)

# Assuming 'rainfall_data' is your dataset with the 'Year' and 'Jul' columns
#rainfall_ts <- ts(rainfall_data$Jul, start = c(2000, 1), frequency = 1)
rainfall_ts <- ts(rainfall_data$Jul, start = c(min(rainfall_data$Year), 1), frequency = 1)

# Fit an ARIMA model
fit <- auto.arima(rainfall_ts)

# lower values are better fit below for AIC, AICc and BIC ->

# Akaike Information Criterion
# AIC is a measure of the relative quality of a statistical model for a given
# set of data. It deals with the trade-off between the goodness of fit of the
# model and the complexity of the model.
fit$aic
# Corrected Akaike Information Criterion
# AICc is a version of AIC corrected for small sample sizes.
fit$aicc
# Bayesian Information Criterion
# BIC is similar to AIC but includes a stronger penalty for models with more
# parameters
fit$bic
## estimate or residual errors - closer to zero is better
fit$sigma2

# Check diagnostics ->
checkresiduals(fit)
# ACF plot should decay to 0 and stay within confidence interval
# Ljung-Box test, we want high P-value, shows residuals are random (p > 0.05)
# residuals plot should be randomly scattered histogram should resemble a normal distribution

# Forecast for July 2025
july_2025_forcast<- forecast(fit, h = 1)
july_2025_forcast$mean[1]
checkresiduals(fit)


# Forecast for Aug 2025
rainfall_ts <- ts(rainfall_data$Aug, start = c(min(rainfall_data$Year), 1), frequency = 1)
fit <- auto.arima(rainfall_ts)
aug_2025_forcast<- forecast(fit, h = 1)
aug_2025_forcast$mean[1]
checkresiduals(fit)
plot(aug_2025_forcast)

# Forecast for Sept 2025
rainfall_ts <- ts(rainfall_data$Sep, start = c(min(rainfall_data$Year), 1), frequency = 1)
fit <- auto.arima(rainfall_ts)
sept_2025_forcast<- forecast(fit, h = 1)
sept_2025_forcast$mean[1]
checkresiduals(fit)


library(Metrics)
# get rid od NA's before evaluating
actual_values <- rainfall_data$Jul[!is.na(rainfall_data$Jul)]
fitted_values <- fitted(fit)[!is.na(fitted(fit))]
# Mean Absolute Error
mae <- mae(actual_values, fitted_values)
# Root Mean Squared Error
rmse <- rmse(actual_values, fitted_values)

# lower values mean better fit
mae
rmse















## Example of fitting and comparing multiple models ----

# Fit the first ARIMA model using auto.arima - selects 'best' parameters based on data
fit1 <- auto.arima(rainfall_ts)

# Fit a second ARIMA model with specified parameters (e.g., ARIMA(1,0,1))
fit2 <- Arima(rainfall_ts, order = c(1, 0, 1))

# Fit a third ARIMA model with different parameters (e.g., ARIMA(2,1,2))
fit3 <- Arima(rainfall_ts, order = c(2, 1, 2))


# calculating AIC(s) manually -> fit1
# Assuming 'fit1' is your model object from auto.arima()
n <- length(rainfall_ts)  # Sample size
k <- length(fit1$coef) + 1  # Number of parameters including the variance

# Calculate AIC
aic_value_fit1 <- AIC(fit1)
# Calculate AICc manually
aicc_value_fit1 <- aic_value_fit1 + (2 * k * (k + 1)) / (n - k - 1)
# Now you can compare AIC and AICc values
print(aicc_value_fit1)

# calculating BIC -> fit1
# Extract the log-likelihood from the model
logLik_value <- logLik(fit1)
# Calculate BIC manually
bic_value_fit1 <- -2 * as.numeric(logLik_value) + k * log(n)
# Print the BIC value
print(bic_value_fit1)


## fit2 -->
n <- length(rainfall_ts)  # Sample size
k <- length(fit2$coef) + 1  # Number of parameters including the variance
aic_value_fit2 <- AIC(fit2)
aicc_value_fit2 <- aic_value_fit2 + (2 * k * (k + 1)) / (n - k - 1)
print(aicc_value_fit2)

logLik_value <- logLik(fit2)
bic_value_fit2 <- -2 * as.numeric(logLik_value) + k * log(n)
print(bic_value_fit2)


## fit3 -->
n <- length(rainfall_ts)  # Sample size
k <- length(fit3$coef) + 1  # Number of parameters including the variance
aic_value_fit3 <- AIC(fit3)
aicc_value_fit3 <- aic_value_fit3 + (2 * k * (k + 1)) / (n - k - 1)
print(aicc_value_fit3)

logLik_value <- logLik(fit3)
bic_value_fit3 <- -2 * as.numeric(logLik_value) + k * log(n)
print(bic_value_fit3)
