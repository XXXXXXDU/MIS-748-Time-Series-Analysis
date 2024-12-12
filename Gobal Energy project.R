version
#Install libraries
install.packages("tidyverse")
install.packages("tsibble")
# Load libraries
library(tidyverse)
library(tsibble)

# Load the dataset
data <- read.csv("C:/Users/tbrag/OneDrive/Desktop/MIS 748/World Energy Consumption.csv")

# Inspect the structure
glimpse(data)

# Check for missing values
summary(data)

# Display column names
colnames(data)

# Subset relevant columns
selected_data <- data %>%
  select(country, year, gdp, primary_energy_consumption,biofuel_consumption, coal_consumption,fossil_fuel_consumption,gas_consumption, hydro_consumption, renewables_consumption, nuclear_consumption,oil_consumption,other_renewable_consumption,solar_consumption, wind_consumption, population)

# Replace NA with 0 for all columns
cleaned_data <- selected_data
cleaned_data[is.na(cleaned_data)] <- 0

# Check if any NA values remain
sum(is.na(cleaned_data))  # Should return 0 if all NAs were replaced

# Filter year
cleaned_data <- cleaned_data %>%
  filter(year >= 1985 & year <= 2021)

# Create 
cleaned_data <- cleaned_data %>%
  group_by(year) %>%
  mutate(total_energy = sum(primary_energy_consumption, na.rm = TRUE)) %>%
  ungroup()

# View the result
head(cleaned_data)


# Use 'country' and 'year' as key and index
global_ts <- cleaned_data %>%
  as_tsibble(key = country, index = year)

# Verify tsibble structure
global_ts

# Filter rows where country == "World" and year is between 1985 and 2021
world_data <- global_ts %>%
  filter(country == "World", year >= 1985 & year <= 2022)

# Plot total energy consumption over time
world_data %>%
  ggplot(aes(x = year, y = total_energy)) +
  geom_line(color = "blue") +
  labs(
    title = "Global Energy Consumption Over Time (1985–2021)",
    x = "Year", y = "Energy Consumption (TWh)"
  ) +
  theme_minimal()

# Ensure total_energy and gdp are numeric
world_data <- world_data %>%
  mutate(
    total_energy = as.numeric(total_energy),
    gdp = as.numeric(gdp)
  )
library(forecast)
install.packages("tseries")
library(tseries)


# Plot ACF for total_energy
acf(world_data$total_energy, main = "ACF of Total Energy Consumption")

# Plot PACF for total_energy
pacf(world_data$total_energy, main = "PACF of Total Energy Consumption")

# Perform ADF test
adf.test(world_data$total_energy, alternative = "stationary")

# Take the first difference of total_energy
world_data <- world_data %>%
  mutate(differenced_energy = c(NA, diff(total_energy)))

# Drop NA rows created by differencing
world_data <- world_data %>%
  filter(!is.na(differenced_energy))

# Perform ADF test on the differenced series
adf_result_diff <- adf.test(world_data$differenced_energy, alternative = "stationary")

# View the results
print(adf_result_diff)

# Apply second differencing to total_energy
world_data <- world_data %>%
  mutate(second_differenced_energy = c(NA, diff(differenced_energy)))

# Drop NA rows created by differencing
world_data <- world_data %>%
  filter(!is.na(second_differenced_energy))

# Perform ADF test on the second differenced series
adf_result_second_diff <- adf.test(world_data$second_differenced_energy, alternative = "stationary")

# View the results
print(adf_result_second_diff)

# Plot ACF and PACF for second differenced series
acf(world_data$second_differenced_energy, main = "ACF of Second Differenced Total Energy")
pacf(world_data$second_differenced_energy, main = "PACF of Second Differenced Total Energy")

install.packages("forecast")
library(forecast)

# Fit ARIMA model with GDP as regressor
arima_model <- auto.arima(
  world_data$total_energy,
  xreg = world_data$gdp,
  d = 2,  # Second differencing
  stepwise = TRUE,  # Perform exhaustive search for the best model
  approximation = FALSE
)

# Summarize the model
summary(arima_model)

# Define the cutoff year for training (exclude the last 5 years)
cutoff_year <- max(world_data$year) - 5

# Split the data
training_data <- world_data %>%
  filter(year <= cutoff_year)

test_data <- world_data %>%
  filter(year > cutoff_year)

# Forecast total energy for the last 10 years
forecasts <- forecast(
  arima_model,
  xreg = test_data$gdp,
  h = nrow(test_data)  # Number of years in the test set
)

# View the forecast
print(forecasts)

# Create a data frame for plotting
forecast_data <- data.frame(
  year = test_data$year,
  actual = test_data$total_energy,
  forecast = forecasts$mean
)

# Combine historical data and forecast data
combined_data <- bind_rows(
  data.frame(
    year = training_data$year,
    total_energy = training_data$total_energy,
    type = "Actual"
  ),
  data.frame(
    year = test_data$year,
    total_energy = test_data$total_energy,
    type = "Actual"
  ),
  data.frame(
    year = test_data$year,
    total_energy = forecasts$mean,
    type = "Forecast"
  )
)

library(ggplot2)

ggplot(combined_data, aes(x = year, y = total_energy, color = type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  labs(
    title = "Actual vs Forecasted Total Energy Consumption",
    x = "Year",
    y = "Total Energy Consumption (TWh)",
    color = "Legend"
  ) +
  theme_minimal()



### Compare energy sources ###
world_data %>%
  pivot_longer(
    cols = c(biofuel_consumption, coal_consumption, fossil_fuel_consumption,
             gas_consumption, hydro_consumption, renewables_consumption,
             nuclear_consumption, oil_consumption, other_renewable_consumption,
             solar_consumption, wind_consumption),
    names_to = "Energy_Type",
    values_to = "Consumption"
  ) %>%
  ggplot(aes(x = year, y = Consumption, color = Energy_Type)) +
  geom_line() +
  labs(
    title = "Energy Consumption by Type (1985–2021)",
    x = "Year", y = "Consumption (TWh)"
  ) +
  theme_minimal()

# List unique countries in the dataset
unique_countries <- unique(cleaned_data$country)

# Print unique countries
print(unique_countries)

# Create a named vector for country-to-region mapping
country_region_mapping <- c(
  "Algeria" = "Africa",
  "Ghana" = "Africa", "Egypt" = "Africa", "Morocco" = "Africa", "South Africa" = "Africa",
  "Canada" = "North America", "Mexico" = "North America", "United States" = "North America",
  "Argentina" = "South America","Brazil" = "South America", 
  "Chile" = "South America", "Colombia" = "South America",
  "Austria" = "Europe", "Belgium" = "Europe", "Denmark" = "Europe", "France" = "Europe",
  "China" = "Asia", "India" = "Asia", "Japan" = "Asia", 
  "South Korea" = "Asia",
  "Indonesia" = "South East Asia", 
  "Malaysia" = "South East Asia", "Singapore" = "South East Asia", "Thailand" = "South East Asia",
  "Australia" = "Oceana", "New Zealand" = "Oceana", "Iran" = "Middle East", "Iraq" = "Middle East", 
  "Israel" = "Middle East", "Saudi Arabia" = "Middle East"
)

# Add a 'region' column based on the mapping
By_region_data <- global_ts %>%
  mutate(region = country_region_mapping[country])

# Filter rows that belong to these specified countries
region_filtered_data <- By_region_data %>%
  filter(!is.na(region))

# View the filtered dataset
head(region_filtered_data)

# Convert tsibble to data frame
region_filtered_df <- as.data.frame(region_filtered_data)

# Group by region and year, then summarize
region_summary <- region_filtered_df %>%
  group_by(region, year) %>%
  summarize(
    total_energy = sum(primary_energy_consumption, na.rm = TRUE),
    .groups = "drop"
  )

# Plot energy consumption by region over time
region_summary %>%
  ggplot(aes(x = year, y = total_energy, color = region)) +
  geom_line() +
  labs(
    title = "Energy Consumption by Region (1985–2021)",
    x = "Year",
    y = "Energy Consumption (TWh)"
  ) +
  theme_minimal()

####Africa Model####
install.packages("fable")
install.packages("fabletools")

library(fable)
library(fabletools)

library(dplyr)

library(dplyr)
library(tsibble)

# Aggregate data by region and year
region_summary <- region_filtered_df %>%
  group_by(region, year) %>%
  summarize(
    gdp = sum(gdp, na.rm = TRUE),
    total_energy = sum(primary_energy_consumption, na.rm = TRUE),
    biofuel = sum(biofuel_consumption, na.rm = TRUE),
    coal = sum(coal_consumption, na.rm = TRUE),
    fossil_fuel = sum(fossil_fuel_consumption, na.rm = TRUE),
    gas = sum(gas_consumption, na.rm = TRUE),
    hydro = sum(hydro_consumption, na.rm = TRUE),
    renewables = sum(renewables_consumption, na.rm = TRUE),
    nuclear = sum(nuclear_consumption, na.rm = TRUE),
    oil = sum(oil_consumption, na.rm = TRUE),
    solar = sum(solar_consumption, na.rm = TRUE),
    wind = sum(wind_consumption, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate correlations for energy types and GDP by region
correlations <- region_summary %>%
  group_by(region) %>%
  summarize(
    total_energy_gdp_corr = cor(total_energy, gdp, use = "complete.obs"),
    biofuel_gdp_corr = cor(biofuel, gdp, use = "complete.obs"),
    coal_gdp_corr = cor(coal, gdp, use = "complete.obs"),
    fossil_fuel_gdp_corr = cor(fossil_fuel, gdp, use = "complete.obs"),
    gas_gdp_corr = cor(gas, gdp, use = "complete.obs"),
    hydro_gdp_corr = cor(hydro, gdp, use = "complete.obs"),
    renewables_gdp_corr = cor(renewables, gdp, use = "complete.obs"),
    nuclear_gdp_corr = cor(nuclear, gdp, use = "complete.obs"),
    oil_gdp_corr = cor(oil, gdp, use = "complete.obs"),
    solar_gdp_corr = cor(solar, gdp, use = "complete.obs"),
    wind_gdp_corr = cor(wind, gdp, use = "complete.obs"),
    .groups = "drop"
  )

# View correlation results
print(correlations)

library(ggplot2)

# Reshape data for plotting
correlation_plot_data <- correlations %>%
  pivot_longer(cols = -region, names_to = "energy_type", values_to = "correlation")

# Plot correlations
ggplot(correlation_plot_data, aes(x = region, y = correlation, fill = energy_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Correlation Between Energy Consumption and GDP by Region",
    x = "Region",
    y = "Correlation",
    fill = "Energy Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to tsibble
region_tsibble <- region_summary %>%
  as_tsibble(index = year, key = region)

library(fable)
install.packages("feasts")
install.packages("urca")

# Fit ARIMA model for total energy
region_models <- region_tsibble %>%
  model(
    ARIMA(total_energy ~ gdp)
  )

# Summarize the model results
glance(region_models)

#Print Results
print(region_models)

# Exclude years 2019, 2020, and 2021
filtered_gdp_data <- region_summary %>%
  filter(!year %in% c(2019, 2020, 2021))

# Calculate the mean annual GDP growth rate by region
gdp_growth <- filtered_gdp_data %>%
  group_by(region) %>%
  summarize(
    gdp_growth_rate = (last(gdp) / first(gdp))^(1 / (n() - 1)) - 1,
    .groups = "drop"
  )

# Generate future GDP values
future_gdp <- region_summary %>%
  filter(year == max(filtered_gdp_data$year)) %>%
  select(region, gdp) %>%
  inner_join(gdp_growth, by = "region") %>%
  mutate(year = list(seq(max(region_summary$year) + 1, max(region_summary$year) + 10))) %>%
  unnest(year) %>%
  mutate(gdp = gdp * (1 + gdp_growth_rate)^(year - max(filtered_gdp_data$year)))

# View the extrapolated future GDP data
head(future_gdp)

# Convert future_gdp to a tsibble
future_gdp_tsibble <- future_gdp %>%
  as_tsibble(index = year, key = region)

# Forecast using future GDP
region_forecasts <- region_models %>%
  forecast(new_data = future_gdp_tsibble)

# Combine all region forecasts into a single plot
region_forecasts %>%
  autoplot(region_tsibble, level = NULL) +  # Removes shaded uncertainty intervals for clarity
  aes(color = region) +  # Add color grouping by region
  labs(
    title = "Forecast of Total Energy Consumption by Region",
    x = "Year",
    y = "Total Energy Consumption (TWh)",
    color = "Region"
  ) +
  theme_minimal()

library(ggplot2)

# Combine historical and forecasted data into a single dataset
combined_data <- bind_rows(
  region_tsibble %>%
    mutate(type = "Historical"),
  region_forecasts %>%
    as_tibble() %>%
    mutate(type = "Forecast")
)

library(dplyr)

# Combine historical and forecast data
combined_data <- bind_rows(
  region_tsibble %>%
    mutate(type = "Historical"),
  region_forecasts %>%
    as_tibble() %>%
    mutate(
      total_energy = .mean,  # Use point forecasts
      type = "Forecast"
    ) %>%
    select(year, region, total_energy, type)  # Ensure columns match
)
library(ggplot2)

ggplot(combined_data, aes(x = year, y = total_energy, color = region, linetype = type)) +
  geom_line() +
  labs(
    title = "Forecast of Total Energy Consumption by Region",
    x = "Year",
    y = "Total Energy Consumption (TWh)",
    color = "Region",
    linetype = "Type"
  ) +
  theme_minimal()





