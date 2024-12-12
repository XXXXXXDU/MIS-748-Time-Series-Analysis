# Required Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(TSA)


#===============================================================================
# Data Loading and Initial Processing
#===============================================================================

# Import the raw dataset, treating empty strings and "NA" as missing values
data <- read.csv("China Data.csv", na.strings = c("", "NA"))

# Calculate year-over-year GDP percentage change
china_data <- data %>%
  arrange(year) %>%
  mutate(gdp_pct = (gdp / lag(gdp) - 1) * 100)

#===============================================================================
# Feature Selection Process
#===============================================================================

# 1. Extract columns containing percentage changes
change_pct_columns <- grep("_change_pct$", colnames(china_data), value = TRUE)
change_pct_data <- china_data[, change_pct_columns, drop = FALSE]

# 2. Create correlation matrix for feature selection
cor_matrix <- cor(change_pct_data, use = "pairwise.complete.obs")

# 3. Select least correlated features iteratively
# This ensures we choose variables that provide unique information
selected_columns <- c()
remaining_columns <- colnames(cor_matrix)

for (i in 1:7) {
  if (i == 1) {
    # For first selection, choose column with lowest average correlation
    avg_cor <- colMeans(abs(cor_matrix), na.rm = TRUE)
    next_col <- names(which.min(avg_cor))
  } else {
    # For subsequent selections, choose columns least correlated with already selected ones
    avg_cor <- colMeans(abs(cor_matrix[selected_columns, remaining_columns, drop = FALSE]), 
                        na.rm = TRUE)
    next_col <- names(which.min(avg_cor))
  }
  
  selected_columns <- c(selected_columns, next_col)
  remaining_columns <- setdiff(remaining_columns, next_col)
}

# 4. Remove energy-related columns from selection
selected_columns <- selected_columns[!grepl("^energy_", selected_columns)]

#===============================================================================
# Mapping Features to Electricity Data
#===============================================================================

# Define mapping between consumption changes and electricity production
electricity_mapping <- list(
  "biofuel_cons_change_pct" = "biofuel_electricity",
  "hydro_cons_change_pct" = "hydro_electricity",
  "oil_prod_change_pct" = "oil_electricity",
  "other_renewables_cons_change_pct" = "other_renewable_electricity",
  "solar_cons_change_pct" = "solar_electricity",
  "nuclear_cons_change_pct" = "nuclear_electricity"
)

# Get corresponding electricity columns
corresponding_electricity_columns <- unlist(electricity_mapping[selected_columns])

#===============================================================================
# Final Dataset Preparation
#===============================================================================

# 1. Select relevant columns
unique_columns <- unique(c("year", selected_columns, corresponding_electricity_columns))
china_data <- china_data %>%
  select(all_of(unique_columns))

# 2. Ensure GDP data is included and recalculate percentage change
china_data <- china_data %>%
  left_join(
    data %>% select(year, gdp),
    by = "year"
  ) %>%
  arrange(year) %>%
  mutate(gdp_pct = (gdp / lag(gdp) - 1) * 100)

# 3. Handle missing values through interpolation
# Uses na.approx from zoo package to fill gaps in time series
china_data_cleaned <- china_data %>%
  mutate(across(
    where(is.numeric) & !matches("year"),
    ~ zoo::na.approx(., na.rm = FALSE, maxgap = Inf, rule = 2),
    .names = "{.col}"
  ))

#===============================================================================
# Data Preparation for Visualization
#===============================================================================

# Convert electricity generation data to long format
china_data_long <- china_data_cleaned %>%
  pivot_longer(
    cols = c(solar_electricity, nuclear_electricity, oil_electricity),
    names_to = "variable", 
    values_to = "value"
  )

# Convert percentage change data to long format
china_data_long_pct <- china_data_cleaned %>%
  pivot_longer(
    cols = c(oil_prod_change_pct, solar_cons_change_pct, nuclear_cons_change_pct),
    names_to = "variable", 
    values_to = "value"
  )

#===============================================================================
# Plot 1: Absolute Generation Values
#===============================================================================

p1 <- ggplot(china_data_long, aes(x = year, y = value, color = variable)) +
  # Add line geometry
  geom_line() +
  # Create separate panels for each energy type
  facet_wrap(~variable, scales = "free_y") +
  # Add labels and title
  labs(
    title = "Trends in Generation by Energy Type in China (1982-2022)",
    x = "Year", 
    y = "Value"
  ) +
  # Apply minimal theme for clean appearance
  theme_minimal()

# First, let's check what columns are actually available
print("Available columns in china_data_cleaned:")
print(colnames(china_data_cleaned))

#===============================================================================
# Data Preparation for Visualization
#===============================================================================

# Convert electricity generation data to long format
china_data_long <- china_data_cleaned %>%
  pivot_longer(
    cols = c(solar_electricity, nuclear_electricity, oil_electricity),
    names_to = "variable", 
    values_to = "value"
  )

# Print the structure of the first transformation to verify it worked
print("Structure of china_data_long:")
str(china_data_long)

# Convert percentage change data to long format - we'll adapt these column names
# based on what's actually available in your data
china_data_long_pct <- china_data_cleaned %>%
  select(year, ends_with("_change_pct")) %>%  # This will select all percentage change columns
  pivot_longer(
    cols = -year,  # Select all columns except year
    names_to = "variable", 
    values_to = "value"
  )

# Print the structure of the percentage change data
print("Structure of china_data_long_pct:")
str(china_data_long_pct)

#===============================================================================
# Plot 1: Absolute Generation Values
#===============================================================================

p1 <- ggplot(china_data_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Trends in Generation by Energy Type in China (1982-2022)",
    x = "Year", 
    y = "Value"
  ) +
  theme_minimal()

#===============================================================================
# Plot Generation for Energy Data
#===============================================================================

# Create long format data for electricity generation
china_data_long <- china_data_cleaned %>%
  pivot_longer(
    cols = c(solar_electricity, nuclear_electricity, oil_electricity),
    names_to = "variable", 
    values_to = "value"
  )

# Create and display generation trends plot
p1 <- ggplot(china_data_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Trends in Generation by Energy Type in China (1982-2022)",
    x = "Year", 
    y = "Value"
  ) +
  theme_minimal()
print(p1)

# Create long format data for percentage changes
china_data_long_pct <- china_data_cleaned %>%
  pivot_longer(
    cols = c(oil_prod_change_pct, solar_cons_change_pct, nuclear_cons_change_pct),
    names_to = "variable", 
    values_to = "value"
  )

# Create and display percentage changes plot
p2 <- ggplot(china_data_long_pct, aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Trends in % Change by Energy Type in China (1982-2022)",
    x = "Year", 
    y = "Value"
  ) +
  theme_minimal()
print(p2)



#===============================================================================
# ADF Test Implementation
#===============================================================================

# Extract relevant variables for testing
variables_for_test <- china_data_cleaned %>%
  select(year, gdp_pct, oil_prod_change_pct, oil_electricity)

# Create function for ADF test
perform_adf_test <- function(data, variable_name) {
  # Remove NA values
  clean_data <- na.omit(data)
  
  # Perform ADF test
  tryCatch({
    adf_result <- adf.test(clean_data)
    
    # Return results as data frame
    data.frame(
      Variable = variable_name,
      Test_Statistic = as.numeric(adf_result$statistic),
      P_Value = adf_result$p.value,
      Is_Stationary = ifelse(adf_result$p.value < 0.05, 
                             "Stationary", 
                             "Non-stationary"),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    # Return NA if test fails
    data.frame(
      Variable = variable_name,
      Test_Statistic = NA,
      P_Value = NA,
      Is_Stationary = "Test Failed",
      stringsAsFactors = FALSE
    )
  })
}

#===============================================================================
# Perform ADF Tests
#===============================================================================

# Test each variable
adf_results <- rbind(
  perform_adf_test(variables_for_test$gdp_pct, "GDP Change Percentage"),
  perform_adf_test(variables_for_test$oil_prod_change_pct, "Oil Production Change Percentage"),
  perform_adf_test(variables_for_test$oil_electricity, "Oil Electricity Production")
)

#===============================================================================
# Visualization of Time Series
#===============================================================================

# Convert data to long format for ggplot
variables_long <- variables_for_test %>%
  pivot_longer(
    cols = c(gdp_pct, oil_prod_change_pct, oil_electricity),
    names_to = "variable",
    values_to = "value"
  )

# Create visualization
adf_plot <- ggplot(variables_long, aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Time Series Analysis of GDP and Oil Variables",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold")
  )

#===============================================================================
# Display Results
#===============================================================================

# Print ADF test results
print("ADF Test Results:")
print(adf_results)

# Display time series plots
print(adf_plot)

#===============================================================================
# Differencing Non-Stationary Variables
#===============================================================================

# Create first differences for non-stationary variables
variables_for_test_diff <- variables_for_test %>%
  mutate(
    gdp_pct_diff = c(NA, diff(gdp_pct)),
    oil_electricity_diff = c(NA, diff(oil_electricity))
  )

#===============================================================================
# Perform ADF Tests After Differencing
#===============================================================================

# Test differenced GDP
gdp_diff_adf <- perform_adf_test(variables_for_test_diff$gdp_pct_diff, 
                                 "GDP Change Percentage (Differenced)")

# Test original oil production (already stationary)
oil_prod_adf <- perform_adf_test(variables_for_test$oil_prod_change_pct,
                                 "Oil Production Change Percentage")

# Test differenced oil electricity
oil_elec_diff_adf <- perform_adf_test(variables_for_test_diff$oil_electricity_diff,
                                      "Oil Electricity Production (Differenced)")

# Combine results after differencing
adf_results_after_diff <- rbind(
  gdp_diff_adf,
  oil_prod_adf,
  oil_elec_diff_adf
)

#===============================================================================
# Visualization of Differenced Time Series
#===============================================================================

# Convert differenced data to long format for ggplot
variables_diff_long <- variables_for_test_diff %>%
  select(year, gdp_pct_diff, oil_prod_change_pct, oil_electricity_diff) %>%
  pivot_longer(
    cols = c(gdp_pct_diff, oil_prod_change_pct, oil_electricity_diff),
    names_to = "variable",
    values_to = "value"
  )

# Create visualization of differenced series
diff_plot <- ggplot(variables_diff_long, aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Time Series Analysis After Differencing",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold")
  )

#===============================================================================
# Display Results After Differencing
#===============================================================================

# Print ADF test results after differencing
print("ADF Test Results After Differencing:")
print(adf_results_after_diff)

# Display time series plots of differenced data
print(diff_plot)

#===============================================================================
# Second Differencing for Oil Electricity Production
#===============================================================================

# Create second difference for oil electricity and keep other transformed variables
variables_for_test_diff2 <- variables_for_test_diff %>%
  mutate(
    oil_electricity_diff2 = c(NA, diff(oil_electricity_diff))
  )

#===============================================================================
# Perform ADF Tests After Second Differencing
#===============================================================================

# Test GDP (first difference - already stationary)
gdp_diff_adf <- perform_adf_test(variables_for_test_diff$gdp_pct_diff, 
                                 "GDP Change Percentage (Differenced)")

# Test oil production (original - already stationary)
oil_prod_adf <- perform_adf_test(variables_for_test$oil_prod_change_pct,
                                 "Oil Production Change Percentage")

# Test oil electricity (second difference)
oil_elec_diff2_adf <- perform_adf_test(variables_for_test_diff2$oil_electricity_diff2,
                                       "Oil Electricity Production (Second Difference)")

# Combine results after second differencing
adf_results_after_diff2 <- rbind(
  gdp_diff_adf,
  oil_prod_adf,
  oil_elec_diff2_adf
)

#===============================================================================
# Visualization of Second Differenced Time Series
#===============================================================================

# Convert data to long format for ggplot
variables_diff2_long <- variables_for_test_diff2 %>%
  select(year, gdp_pct_diff, oil_prod_change_pct, oil_electricity_diff2) %>%
  pivot_longer(
    cols = c(gdp_pct_diff, oil_prod_change_pct, oil_electricity_diff2),
    names_to = "variable",
    values_to = "value"
  )

# Create visualization
diff2_plot <- ggplot(variables_diff2_long, aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Time Series Analysis After Second Differencing",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold")
  )

#===============================================================================
# Display Results After Second Differencing
#===============================================================================

# Print ADF test results
print("ADF Test Results After Second Differencing:")
print(adf_results_after_diff2)

# Display time series plots
print(diff2_plot)




#===============================================================================
# Data Preparation
#===============================================================================

# Get differenced GDP data (first difference)
gdp_diff <- diff(china_data_cleaned$gdp_pct, differences = 1)

# Get original oil production data (already stationary)
oil_prod <- china_data_cleaned$oil_prod_change_pct

# Get differenced oil electricity data (second difference)
oil_elec_diff <- diff(china_data_cleaned$oil_electricity, differences = 2)

# Function to prepare data for EACF
prepare_for_eacf <- function(data) {
  # Remove any NA or infinite values
  clean_data <- na.omit(data)
  # Convert to numeric vector
  clean_data <- as.numeric(clean_data)
  return(clean_data)
}

#===============================================================================
# Apply EACF Analysis to Each Variable
#===============================================================================

# Function for safe EACF computation
safe_eacf <- function(data, variable_name) {
  tryCatch({
    cat("\nEACF Analysis for", variable_name, ":\n")
    # Prepare data
    clean_data <- prepare_for_eacf(data)
    
    # Use smaller maximum orders to avoid singularity
    ar.max <- min(5, floor(length(clean_data)/4))
    ma.max <- min(5, floor(length(clean_data)/4))
    
    # Perform EACF
    eacf(clean_data, ar.max = ar.max, ma.max = ma.max)
  }, error = function(e) {
    cat("\nCould not compute EACF for", variable_name, "\n")
    cat("Error:", conditionMessage(e), "\n")
    
    # Print summary statistics instead
    cat("\nProviding summary statistics instead:\n")
    print(summary(data))
    cat("\nStandard deviation:", sd(data, na.rm = TRUE), "\n")
  })
}

# GDP Analysis (First Difference)
cat("\n=== GDP Change Percentage Analysis (First Difference) ===\n")
gdp_data <- prepare_for_eacf(gdp_diff)
safe_eacf(gdp_data, "GDP Change Percentage")

# Oil Production Analysis
cat("\n=== Oil Production Change Analysis ===\n")
oil_prod_data <- prepare_for_eacf(oil_prod)
safe_eacf(oil_prod_data, "Oil Production Change")

# Oil Electricity Analysis (Second Difference)
cat("\n=== Oil Electricity Production Analysis (Second Difference) ===\n")
oil_elec_data <- prepare_for_eacf(oil_elec_diff)
safe_eacf(oil_elec_data, "Oil Electricity Production")

#===============================================================================
# Alternative Analysis
#===============================================================================

# Function to identify potential ARMA orders
suggest_arma_orders <- function(data, variable_name) {
  cat("\nSuggested ARMA Orders for", variable_name, ":\n")
  
  # Calculate ACF and PACF
  acf_result <- acf(data, plot = FALSE, lag.max = 10)
  pacf_result <- pacf(data, plot = FALSE, lag.max = 10)
  
  # Get significant lags
  acf_sig <- which(abs(acf_result$acf[-1]) > 2/sqrt(length(data)))
  pacf_sig <- which(abs(pacf_result$acf) > 2/sqrt(length(data)))
  
  cat("Suggested AR order (based on significant PACF lags):", 
      ifelse(length(pacf_sig) > 0, max(pacf_sig), 0), "\n")
  cat("Suggested MA order (based on significant ACF lags):", 
      ifelse(length(acf_sig) > 0, max(acf_sig), 0), "\n")
}

# Apply alternative analysis
cat("\n=== Alternative Analysis ===\n")
suggest_arma_orders(gdp_data, "GDP Change Percentage")
suggest_arma_orders(oil_prod_data, "Oil Production Change")
suggest_arma_orders(oil_elec_data, "Oil Electricity Production")

#===============================================================================
# Visual ACF/PACF Analysis
#===============================================================================

# Function to create ACF/PACF plots
create_acf_pacf_plots <- function(data, variable_name) {
  par(mfrow = c(2,1), mar = c(3,4,2,2))
  acf(data, main = paste("ACF -", variable_name))
  pacf(data, main = paste("PACF -", variable_name))
  par(mfrow = c(1,1))
}

# Create plots for each variable
dev.new(width = 10, height = 8)
create_acf_pacf_plots(gdp_data, "GDP Change Percentage")

dev.new(width = 10, height = 8)
create_acf_pacf_plots(oil_prod_data, "Oil Production Change")

dev.new(width = 10, height = 8)
create_acf_pacf_plots(oil_elec_data, "Oil Electricity Production")







#===============================================================================
# Fit ARIMA Models
#===============================================================================

# GDP Change Percentage - ARIMA(2,1,1)
gdp_model <- Arima(china_data_cleaned$gdp_pct,
                   order = c(2,1,1),
                   include.mean = TRUE)

# Oil Production Change - ARIMA(1,0,0)
oil_prod_model <- Arima(china_data_cleaned$oil_prod_change_pct,
                        order = c(1,0,0),
                        include.mean = TRUE)

# Oil Electricity Production - ARIMA(1,2,0)
oil_elec_model <- Arima(china_data_cleaned$oil_electricity,
                        order = c(1,2,0),
                        include.mean = TRUE)

#===============================================================================
# Model Diagnostics
#===============================================================================

# Function to print model summary and diagnostics
print_model_diagnostics <- function(model, variable_name) {
  cat("\n=== Model Summary for", variable_name, "===\n")
  print(summary(model))
  
  cat("\nAIC:", model$aic)
  cat("\nBIC:", model$bic)
  cat("\nLog Likelihood:", model$loglik)
  
  # Extract residuals for Ljung-Box test
  res <- residuals(model)
  lb_test <- Box.test(res, lag = 10, type = "Ljung-Box")
  cat("\n\nLjung-Box Test Results:")
  print(lb_test)
}

# Print diagnostics for each model
print_model_diagnostics(gdp_model, "GDP Change Percentage ARIMA(2,1,1)")
print_model_diagnostics(oil_prod_model, "Oil Production Change ARIMA(1,0,0)")
print_model_diagnostics(oil_elec_model, "Oil Electricity Production ARIMA(1,2,0)")

#===============================================================================
# Residual Analysis
#===============================================================================

# Function to create residual plots
plot_residuals <- function(model, title) {
  par(mfrow = c(2,2))
  # Standardized residuals
  plot(residuals(model), main = paste("Standardized Residuals -", title))
  abline(h = 0, col = "red")
  
  # ACF of residuals
  acf(residuals(model), main = "ACF of Residuals")
  
  # Normal Q-Q plot
  qqnorm(residuals(model))
  qqline(residuals(model), col = "red")
  
  # Histogram of residuals
  hist(residuals(model), main = "Histogram of Residuals", freq = FALSE)
  lines(density(residuals(model)), col = "red")
  
  par(mfrow = c(1,1))
}

# Create residual plots for each model
dev.new(width = 10, height = 8)
plot_residuals(gdp_model, "GDP Change Percentage")

dev.new(width = 10, height = 8)
plot_residuals(oil_prod_model, "Oil Production Change")

dev.new(width = 10, height = 8)
plot_residuals(oil_elec_model, "Oil Electricity Production")









#===============================================================================
# Data Preparation and Train-Test Split
#===============================================================================

# Create time indices
train_end <- 35
forecast_horizon <- 10  # Forecast from 35 to 45

# Prepare training data
train_data <- china_data_cleaned[1:train_end, ]

# Convert to time series
gdp_train <- ts(train_data$gdp_pct)
oil_prod_train <- ts(train_data$oil_prod_change_pct)
oil_elec_train <- ts(train_data$oil_electricity)

#===============================================================================
# Fit Models on Training Data
#===============================================================================

# GDP Change Percentage - ARIMA(2,1,1)
gdp_model <- Arima(gdp_train,
                   order = c(2,1,1),
                   include.mean = TRUE)

# Oil Production Change - ARIMA(1,0,0)
oil_prod_model <- Arima(oil_prod_train,
                        order = c(1,0,0),
                        include.mean = TRUE)

# Oil Electricity Production - ARIMA(1,2,0)
oil_elec_model <- Arima(oil_elec_train,
                        order = c(1,2,0),
                        include.mean = TRUE)

#===============================================================================
# Generate Forecasts
#===============================================================================

# Generate forecasts with prediction intervals
gdp_forecast <- forecast(gdp_model, h = forecast_horizon, level = c(80, 95))
oil_prod_forecast <- forecast(oil_prod_model, h = forecast_horizon, level = c(80, 95))
oil_elec_forecast <- forecast(oil_elec_model, h = forecast_horizon, level = c(80, 95))












#===============================================================================
# Improved Plotting Function
#===============================================================================

plot_forecast_with_actual <- function(forecast_obj, actual_data, variable_name) {
  # Create time index for actual data
  time_index <- 1:length(actual_data)
  
  # Get the last actual value at point 35
  last_actual <- actual_data[train_end]
  
  # Create forecast time points (should be 10 points from 36 to 45)
  forecast_times <- (train_end + 1):45
  
  # Create forecast data frame
  forecast_df <- data.frame(
    x = forecast_times,
    y = forecast_obj$mean
  )
  
  # Add the connection point (point 35)
  forecast_df <- rbind(
    data.frame(x = train_end, y = last_actual),
    forecast_df
  )
  
  # Create data frame for actual values
  actual_df <- data.frame(
    x = time_index,
    y = actual_data
  )
  
  # Create plot
  plot <- ggplot() +
    # Add prediction intervals
    geom_ribbon(aes(x = forecast_times, 
                    ymin = forecast_obj$lower[,2], 
                    ymax = forecast_obj$upper[,2]), 
                fill = "grey90") +
    geom_ribbon(aes(x = forecast_times, 
                    ymin = forecast_obj$lower[,1], 
                    ymax = forecast_obj$upper[,1]), 
                fill = "grey80") +
    # Add actual values
    geom_line(data = actual_df, aes(x = x, y = y), color = "black") +
    # Add forecast line with triangles
    geom_line(data = forecast_df, aes(x = x, y = y), 
              color = "red", size = 1) +
    geom_point(data = forecast_df, aes(x = x, y = y), 
               color = "red", shape = 17, size = 3) +
    # Add vertical line for train/test split
    geom_vline(xintercept = train_end, linetype = "dashed", color = "blue") +
    # Add labels
    labs(title = paste("Forecast for", variable_name),
         subtitle = paste("Training data (1-35), Forecast (35-45)",
                          "\nBlack: Actual, Red: Forecast, Shaded: Prediction Intervals"),
         x = "Time Period",
         y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plot)
}

#===============================================================================
# Generate Forecasts
#===============================================================================

# Generate forecasts with prediction intervals
gdp_forecast <- forecast(gdp_model, h = 10, level = c(80, 95))
oil_prod_forecast <- forecast(oil_prod_model, h = 10, level = c(80, 95))
oil_elec_forecast <- forecast(oil_elec_model, h = 10, level = c(80, 95))

#===============================================================================
# Generate and Display Plots
#===============================================================================

# GDP forecast plot
gdp_plot <- plot_forecast_with_actual(gdp_forecast, 
                                      china_data_cleaned$gdp_pct,
                                      "GDP Change Percentage")

# Oil production forecast plot
oil_prod_plot <- plot_forecast_with_actual(oil_prod_forecast,
                                           china_data_cleaned$oil_prod_change_pct,
                                           "Oil Production Change")

# Oil electricity forecast plot
oil_elec_plot <- plot_forecast_with_actual(oil_elec_forecast,
                                           china_data_cleaned$oil_electricity,
                                           "Oil Electricity Production")

# Display plots
print(gdp_plot)
print(oil_prod_plot)
print(oil_elec_plot)

