# Load libraries
library(dplyr)

# Load the dataset
file_path <- "/Users/ethan/Downloads/World Energy Consumption.csv"  
data <- read.csv(file_path, na.strings = c("", "NA"))  # Ensure blank cells are treated as blank

# Step 1: Filter data for the years 1982 to 2022
data_filtered_years <- data %>%
  filter(year >= 1982 & year <= 2022)

# Step 2: Calculate the average missing percentage per country based on columns
missing_summary_by_columns <- data_filtered_years %>%
  group_by(country) %>%
  summarise(
    average_missing_percent = mean(
      sapply(across(everything(), ~ sum(is.na(.)) / n() * 100), mean)
    )
  ) %>%
  arrange(average_missing_percent)  # Rank by average missing percentage

# Step 3: Save the ranked summary to a CSV file
write.csv(missing_summary_by_columns, "Ranked_Countries_By_Avg_Missing_Percent.csv", row.names = FALSE, na = "")

# Step 4: Filter out countries with less than 10% missing cells
valid_countries <- missing_summary_by_columns %>%
  filter(average_missing_percent < 10) %>%
  pull(country)  # Extract the list of valid countries

# Step 5: Filter the original dataset to include only these countries
filtered_low_missing_data <- data_filtered_years %>%
  filter(country %in% valid_countries)

# Step 6: Save the filtered dataset to a new CSV file
write.csv(filtered_low_missing_data, "Filtered_Countries_Low_Missing_By_Avg_1982_2022.csv", row.names = FALSE, na = "")

################################################################################

## The following is the U.S data for example, I have figure out what other columns we could use
## I only take the "gdp","Population","electricity_demand",and "electricity_generation".

install.packages("forecast")
install.packages("tidyr")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo) # For interpolation

# Load the dataset
file_path <- "Filtered_Countries_Low_Missing_By_Avg_1982_2022.csv" # Replace with your actual dataset file path
data <- read.csv(file_path, na.strings = c("", "NA"))

# Step 1: Filter the dataset for the United States only, retaining the 'year' column
us_data <- data %>%
  filter(country == "United States") %>%
  select(year, everything()) # Ensure 'year' is retained

us_data <- us_data %>%
  mutate(gdp_pct_change = (gdp / lag(gdp) - 1) * 100)


# Step 2: Filter columns ending with `_change_pct`
change_pct_columns <- grep("_change_pct$", colnames(us_data), value = TRUE)
change_pct_data <- us_data[, change_pct_columns, drop = FALSE]

# Step 3: Compute correlation matrix (excluding NA values)
cor_matrix <- cor(change_pct_data, use = "pairwise.complete.obs")

# Step 4: Select 7 least correlated `_change_pct` columns
selected_columns <- c()
remaining_columns <- colnames(cor_matrix)

for (i in 1:7) {
  if (i == 1) {
    # Select the column with the lowest overall correlation
    avg_cor <- colMeans(abs(cor_matrix), na.rm = TRUE)
    next_col <- names(which.min(avg_cor))
  } else {
    # Find the column with the lowest average correlation to already selected columns
    avg_cor <- colMeans(abs(cor_matrix[selected_columns, remaining_columns, drop = FALSE]), na.rm = TRUE)
    next_col <- names(which.min(avg_cor))
  }
  
  # Add to selected and remove from remaining
  selected_columns <- c(selected_columns, next_col)
  remaining_columns <- setdiff(remaining_columns, next_col)
}

# Exclude energy_* columns from the selected columns
selected_columns <- selected_columns[!grepl("^energy_", selected_columns)]

print(selected_columns)

# Step 5: Manually map the selected `_change_pct` columns to their corresponding `_electricity` columns
electricity_mapping <- list(
  "biofuel_cons_change_pct" = "biofuel_electricity",
  "hydro_cons_change_pct" = "hydro_electricity",
  "oil_prod_change_pct" = "oil_electricity",
  "other_renewables_cons_change_pct" = "other_renewable_electricity",
  "solar_cons_change_pct" = "solar_electricity",
  "nuclear_cons_change_pct" = "nuclear_electricity"
)

# Extract the manually defined corresponding electricity columns
corresponding_electricity_columns <- unlist(electricity_mapping[selected_columns])

# Print the results
print("Selected Change Percentage Columns:")
print(selected_columns)

print("Manually Defined Corresponding Electricity Columns:")
print(corresponding_electricity_columns)

# Step 6: Filter the dataset to include unique selected columns and their corresponding `_electricity` columns
unique_columns <- unique(c("year", selected_columns, corresponding_electricity_columns)) # Ensure 'year' is included
us_data <- us_data %>%
  select(all_of(unique_columns))

us_data <- us_data %>%
  left_join(
    data %>% filter(country == "United States") %>% select(year, gdp),
    by = "year"
  )
us_data <- us_data %>%
  mutate(gdp_pct = (gdp / lag(gdp) - 1) * 100)


# Output the final dataset
list(
  selected_change_pct_columns = selected_columns,
  corresponding_electricity_columns = corresponding_electricity_columns,
  final_data = us_data
)


#Plot energy generation trends from 1982 to 2022 for all 4 variables 
us_data_long <- us_data %>%
  pivot_longer(cols = c(biofuel_electricity, hydro_electricity, oil_electricity, other_renewable_electricity, solar_electricity, nuclear_electricity),
               names_to = "variable", values_to = "value")
dev.off() # Closes the current graphics device


ggplot(us_data_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Trends in Generation by Energy Type (1982-2022)",
       x = "Year", y = "Value") +
  theme_minimal()

#Plot % change in energy generation trends from 1982 to 2022 for all 4 variables 

us_data_long_pct <- us_data %>%
  pivot_longer(cols = c(biofuel_cons_change_pct, hydro_cons_change_pct, oil_prod_change_pct, other_renewables_cons_change_pct),
               names_to = "variable", values_to = "value")

ggplot(us_data_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Trends in % Change by Energy Type (1982-2022)",
       x = "Year", y = "Value") +
  theme_minimal()



# Step 3: Handle missing values by interpolation for all columns except 'year'
us_data_cleaned <- us_data %>%
  mutate(across(
    where(is.numeric) & !matches("year"),
    ~ na.approx(., na.rm = FALSE),
    .names = "{.col}"
  ))

print(us_data_cleaned)


# Ensure no remaining NA values for ADF test by dropping rows where interpolation failed
us_data_cleaned <- us_data_cleaned %>%
  drop_na(biofuel_cons_change_pct, biofuel_electricity, oil_prod_change_pct, oil_electricity, other_renewables_cons_change_pct, other_renewable_electricity, hydro_cons_change_pct, hydro_electricity, solar_cons_change_pct, solar_electricity, nuclear_cons_change_pct, nuclear_electricity, gdp, gdp_pct )


# Step 6: Plot ACF and PACF
par(mfrow = c(3, 4))
acf(us_data_cleaned$oil_prod_change_pct, main = "ACF - Oil_pct (Differenced)")
pacf(us_data_cleaned$oil_prod_change_pct, main = "PACF - Oil_pct (Differenced)")
acf(us_data_cleaned$nuclear_cons_change_pct, main = "ACF - Nuclear_pct (Differenced)")
pacf(us_data_cleaned$nuclear_cons_change_pct, main = "PACF - Nuclear_pct (Differenced)")
acf(us_data_cleaned$biofuel_cons_change_pct, main = "ACF - Biofuel_pct (Differenced)")
pacf(us_data_cleaned$biofuel_cons_change_pct, main = "PACF - Biofuel_pct (Differenced)")
acf(us_data_cleaned$gdp_pct, main = "ACF - Gdp_pct (Differenced)")
pacf(us_data_cleaned$gdp_pct, main = "PACF - Gdp_pct (Differenced)")
acf(us_data_cleaned$hydro_cons_change_pct, main = "ACF - Hydro_pct (Differenced)")
pacf(us_data_cleaned$hydro_cons_change_pct, main = "PACF - Hydro_pct (Differenced)")
acf(us_data_cleaned$other_renewables_cons_change_pct, main = "ACF - Other_pct (Differenced)")
pacf(us_data_cleaned$other_renewables_cons_change_pct, main = "PACF - Other_pct (Differenced)")






#We will first plot all the cleaned variables to see their trend and variance. 
# Reshape the dataset to long format for plotting
us_data_all <- us_data_cleaned %>%
  pivot_longer(
    cols = -year, # Exclude the 'year' column
    names_to = "variable",
    values_to = "value"
  )

# Plot the variables in separate subplots
ggplot(us_data_all, aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "Trends in Variables (Non-Year Columns)",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend


# Step 4.1: Check for stationarity of _electricity variables (Dickey-Fuller Test)
adf_biofuel <- adf.test(us_data_cleaned$biofuel_electricity, alternative = "stationary")
adf_hydro <- adf.test(us_data_cleaned$hydro_electricity, alternative = "stationary")
adf_oil <- adf.test(us_data_cleaned$oil_electricity, alternative = "stationary")
adf_other_renewables <- adf.test(us_data_cleaned$other_renewable_electricity, alternative = "stationary")

print("ADF Test Results:")
adf_results <- list(
  biofuel = adf_biofuel$p.value,
  hydro = adf_hydro$p.value,
  oil = adf_oil$p.value,
  other_renewables = adf_other_renewables$p.value
)
print(adf_results)

# Step 4.2: Check for stationarity of _change_pct variables (Dickey-Fuller Test)
adf_biofuel1 <- adf.test(us_data_cleaned$biofuel_cons_change_pct, alternative = "stationary")
adf_hydro1 <- adf.test(us_data_cleaned$hydro_cons_change_pct, alternative = "stationary")
adf_oil1 <- adf.test(us_data_cleaned$oil_prod_change_pct, alternative = "stationary")
adf_other_renewables1 <- adf.test(us_data_cleaned$other_renewables_cons_change_pct, alternative = "stationary")
adf_gdp1 <-  adf.test(us_data_cleaned$gdp_pct, alternative = "stationary")
adf_nuclear1 <- adf.test(us_data_cleaned$other_renewables_cons_change_pct, alternative = "stationary")

print("ADF Test Results for % change:")
adf_results <- list(
  biofuel = adf_biofuel1$p.value,
  hydro = adf_hydro1$p.value,
  oil = adf_oil1$p.value,
  other_renewables = adf_other_renewables1$p.value,
  nuclear = adf_nuclear1$p.value,
  gdp = adf_gdp1$p.value
)
print(adf_results)

#We see that based on ADF test, none of the variables are stationary. Based on the time series trends shown also, 
#every one of them requires transformation to at least get rid of the trend through differencing

# Step 5: Differencing _electricity and _pct_change to make data stationary

# Step 5: Log Transformation and Differencing to Make Data Stationary

# Log transform and differencing for _electricity variables
diff_biofuel <- diff(us_data_cleaned$biofuel_electricity, differences = 1)
diff_hydro <- diff(us_data_cleaned$hydro_electricity, differences = 1)
diff_oil <- diff(us_data_cleaned$oil_electricity, differences = 1)
diff_other <- diff(us_data_cleaned$other_renewable_electricity, differences = 1)
diff_nuclear <- diff(us_data_cleaned$nuclear_electricity, differences = 1)
diff_gdp <- diff(us_data_cleaned$gdp, differences =1 )

# Log transform and differencing for _change_pct variables
diff_biofuel_pct <- diff(us_data_cleaned$biofuel_cons_change_pct, differences = 1)
diff_hydro_pct <- diff(us_data_cleaned$hydro_cons_change_pct, differences = 1)
diff_oil_pct <- diff(us_data_cleaned$oil_prod_change_pct, differences = 1)
diff_other_pct <- diff(us_data_cleaned$other_renewables_cons_change_pct, differences = 1)
diff_nuclear_pct <- diff(us_data_cleaned$nuclear_cons_change_pct, differences = 1)
diff_gdp_pct <- diff(us_data_cleaned$gdp_pct, differences = 1)







# Handle potential issues with log transformation:
# If any values in the data are <= 0, the log transformation will fail.
# To avoid this, you may filter or shift the data to ensure positivity before taking the log.
print(log_diff_oil_pct)

#Plot the differences values now 
# Set up the plotting area for 4 rows and 2 columns
# Corrected plotting code
# Set up the plotting area for 4 rows and 2 columns
par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))  # Set up 4 rows, 2 columns layout

# Biofuel electricity (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_biofuel,
     type = "l", col = "blue", main = "Log-Diff Biofuel Electricity",
     xlab = "Year", ylab = "Log-Diff Value")

# Biofuel change % (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_biofuel_pct,
     type = "l", col = "blue", main = "Log-Diff Biofuel Change %",
     xlab = "Year", ylab = "Log-Diff Value")

# Hydro electricity (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_hydro,
     type = "l", col = "green", main = "Log-Diff Hydro Electricity",
     xlab = "Year", ylab = "Log-Diff Value")

# Hydro change % (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_hydro_pct,
     type = "l", col = "green", main = "Log-Diff Hydro Change %",
     xlab = "Year", ylab = "Log-Diff Value")

# Oil electricity (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_oil,
     type = "l", col = "red", main = "Log-Diff Oil Electricity",
     xlab = "Year", ylab = "Log-Diff Value")

# Oil change % (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_oil_pct,
     type = "l", col = "red", main = "Log-Diff Oil Change %",
     xlab = "Year", ylab = "Log-Diff Value")

# Other renewable electricity (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_other,
     type = "l", col = "purple", main = "Log-Diff Other Renewable Electricity",
     xlab = "Year", ylab = "Log-Diff Value")

# Other renewable change % (log-differenced)
plot(us_data_cleaned$year[-1], log_diff_other_pct,
     type = "l", col = "purple", main = "Log-Diff Other Renewable Change %",
     xlab = "Year", ylab = "Log-Diff Value")

# Reset to default
par(mfrow = c(1, 1))



#Check for stationarity of differenced _electricity variables (Dickey-Fuller Test)
adf_diff_biofuel <- adf.test(diff_biofuel, alternative = "stationary")
adf_diff_hydro <- adf.test(diff_hydro, alternative = "stationary")
adf_diff_oil <- adf.test(diff_oil, alternative = "stationary")
adf_diff_other <- adf.test(diff_other, alternative = "stationary")
adf_diff_nuclear <- adf.test(diff_nuclear, alternative = 'stationary')
adf_diff_gdp <- adf.test(diff_gdp, alternative = 'stationary')

print("ADF Test Results for Differenced Electricity Variables:")
adf_diff_electricity_results <- list(
  biofuel = adf_diff_biofuel$p.value,
  hydro = adf_diff_hydro$p.value,
  oil = adf_diff_oil$p.value,
  other_renewables = adf_diff_other$p.value,
  nuclear = adf_diff_nuclear$p.value,
  gdp = adf_diff_gdp$p.value
)
print(adf_diff_electricity_results)

#Check for stationarity of differenced _change_pct variables (Dickey-Fuller Test)
adf_diff_biofuel_pct <- adf.test(diff_biofuel_pct, alternative = "stationary")
adf_diff_hydro_pct <- adf.test(diff_hydro_pct, alternative = "stationary")
adf_diff_oil_pct <- adf.test(diff_oil_pct, alternative = "stationary")
adf_diff_other_pct <- adf.test(diff_other_pct, alternative = "stationary")
adf_diff_nuclear_pct <- adf.test(diff_nuclear_pct, alternative = "stationary")
adf_diff_gdp_pct <- adf.test(diff_gdp_pct, alternative='stationary')


print("ADF Test Results for Differenced Change Percentage Variables:")
adf_diff_pct_results <- list(
  biofuel = adf_diff_biofuel_pct$p.value,
  hydro = adf_diff_hydro_pct$p.value,
  oil = adf_diff_oil_pct$p.value,
  other_renewables = adf_diff_other_pct$p.value,
  nuclear = adf_diff_nuclear_pct$p.value,
  gdp = adf_diff_gdp_pct$p.value
)
print(adf_diff_pct_results)




library(tseries) # For adf.test

# Function to difference a variable until it becomes stationary
make_stationary <- function(variable, max_diff = 5) {
  diff_level <- 0 # Start with no differencing
  p_value <- 1    # Initialize p-value
  
  # Repeat differencing until variable becomes stationary or max_diff is reached
  while (p_value > 0.05 && diff_level < max_diff) {
    # Difference the variable
    diff_level <- diff_level + 1
    differenced_variable <- diff(variable, differences = diff_level)
    
    # Perform ADF test on the differenced variable (handle NA values)
    if (any(!is.na(differenced_variable))) {
      p_value <- adf.test(differenced_variable, alternative = "stationary")$p.value
    } else {
      break # Stop if all values are NA
    }
  }
  
  list(
    differenced_variable = differenced_variable,
    diff_level = diff_level,
    p_value = p_value
  )
}

# Variables to be checked for stationarity
variables <- list(
  biofuel_electricity = us_data_cleaned$biofuel_electricity,
  hydro_electricity = us_data_cleaned$hydro_electricity,
  oil_electricity = us_data_cleaned$oil_electricity,
  other_renewable_electricity = us_data_cleaned$other_renewable_electricity,
  nuclear_electricity = us_data_cleaned$nuclear_electricity,
  gdp = us_data_cleaned$gdp,
  biofuel_cons_change_pct = us_data_cleaned$biofuel_cons_change_pct,
  hydro_cons_change_pct = us_data_cleaned$hydro_cons_change_pct,
  oil_prod_change_pct = us_data_cleaned$oil_prod_change_pct,
  other_renewables_cons_change_pct = us_data_cleaned$other_renewables_cons_change_pct,
  nuclear_cons_change_pct = us_data_cleaned$nuclear_cons_change_pct,
  gdp_pct = us_data_cleaned$gdp_pct
)

# Apply the function to each variable
stationary_results <- lapply(variables, make_stationary)

# Display the results
results_summary <- lapply(names(stationary_results), function(var_name) {
  result <- stationary_results[[var_name]]
  list(
    variable = var_name,
    diff_level = result$diff_level,
    p_value = result$p_value
  )
})

# Convert to a data frame for easier viewing
results_summary_df <- do.call(rbind, lapply(results_summary, as.data.frame))
print(results_summary_df)


# Create a new list to store the differenced variables
differenced_data <- list()

# Apply the differencing levels based on the stationarity results
for (var_name in names(stationary_results)) {
  result <- stationary_results[[var_name]]
  
  # Extract the original variable and the number of differencing levels
  original_variable <- variables[[var_name]]
  diff_level <- result$diff_level
  
  # Apply the differencing to the original variable
  differenced_variable <- diff(original_variable, differences = diff_level)
  
  # Calculate the padding required for alignment
  padding <- rep(NA, diff_level)
  
  # Store the differenced variable, adding padding for alignment
  differenced_data[[var_name]] <- c(padding, differenced_variable)
}

# Combine all differenced variables into a data frame
aligned_data <- data.frame(
  year = us_data_cleaned$year,
  differenced_data
)

# Preview the final aligned data
print(head(aligned_data))



aligned_data <- aligned_data %>%
  drop_na(biofuel_cons_change_pct, biofuel_electricity, oil_prod_change_pct, oil_electricity, other_renewables_cons_change_pct, other_renewable_electricity, hydro_cons_change_pct, hydro_electricity, nuclear_cons_change_pct, nuclear_electricity, gdp, gdp_pct )



#We see that for _electricity oil and nuclear are stationary after 1st order differencing, and as for _pct_change nuclear oil and hydro are stationary. We will use oil and nuclear. 


# Step 6: Plot ACF and PACF
par(mfrow = c(3, 4))
acf(diff_oil_pct, main = "ACF - Oil_pct (Differenced)")
pacf(diff_oil_pct, main = "PACF - Oil_pct (Differenced)")
acf(diff_nuclear_pct, main = "ACF - Nuclear_pct (Differenced)")
pacf(diff_nuclear_pct, main = "PACF - Nuclear_pct (Differenced)")
acf(diff_biofuel_pct, main = "ACF - Biofuel_pct (Differenced)")
pacf(diff_biofuel_pct, main = "PACF - Biofuel_pct (Differenced)")
acf(diff_gdp_pct, main = "ACF - Gdp_pct (Differenced)")
pacf(diff_gdp_pct, main = "PACF - Gdp_pct (Differenced)")
acf(diff_hydro_pct, main = "ACF - Hydro_pct (Differenced)")
pacf(diff_hydro_pct, main = "PACF - Hydro_pct (Differenced)")
acf(diff_other_pct, main = "ACF - Other_pct (Differenced)")
pacf(diff_other_pct, main = "PACF - Other_pct (Differenced)")




# Step 7: Fit ARIMA model for forecasting
gdp_arima <- auto.arima(us_data_cleaned$gdp, seasonal = TRUE)
demand_arima <- auto.arima(us_data_cleaned$electricity_demand, seasonal = TRUE)
generation_arima <- auto.arima(us_data_cleaned$electricity_generation, seasonal = TRUE)

print("ARIMA Models Summary:")
arima_models <- list(
  gdp_model = gdp_arima,
  electricity_demand_model = demand_arima,
  electricity_generation_model = generation_arima
)
print(arima_models)

# Step 8: Forecasting
gdp_forecast <- forecast(gdp_arima, h = 10)
demand_forecast <- forecast(demand_arima, h = 10)
generation_forecast <- forecast(generation_arima, h = 10)

# Plot the forecasts
autoplot(gdp_forecast) +
  labs(title = "Forecast: GDP (United States)",
       x = "Year", y = "GDP")

autoplot(demand_forecast) +
  labs(title = "Forecast: Electricity Demand (United States)",
       x = "Year", y = "Electricity Demand")

autoplot(generation_forecast) +
  labs(title = "Forecast: Electricity Generation (United States)",
       x = "Year", y = "Electricity Generation")

# Step 9: Model diagnostics
checkresiduals(gdp_arima)
checkresiduals(demand_arima)
checkresiduals(generation_arima)

