# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Read the CSV files
henry_hub <- read.csv("../data/Henry_Hub_Natural_Gas_Spot_Price.csv", skip = 5)
texas_elecgasprice <- read.csv("../data/Texas_Natural_Gas_Price_Sold_to_Electric_Power_Consumers (1).csv", skip = 4)

# Convert Henry Hub's Day column to Date format
henry_hub$Day <- as.Date(henry_hub$Day, format = "%m/%d/%y")  # Adjust if necessary
texas_elecgasprice$Month <- as.Date(texas_elecgasprice$Month, format = "%m/%d/%y")

# Add a Year-Month column for Henry Hub
henry_hub$Month <- format(henry_hub$Day, "%Y-%m")
texas_elecgasprice$Month <- format(texas_elecgasprice$Month, "%Y-%m")
texas_elecgasprice <- texas_elecgasprice %>%
  rename(TexasElectric = Texas.Natural.Gas.Price.Sold.to.Electric.Power.Consumers..Dollars.per.Thousand.Cubic.Feet)

# Filter Henry Hub data for 2022, 2023, and 2024
henry_hub_filtered <- henry_hub %>%
  filter(Month >= "2022-01") %>%
  group_by(Month) %>%
  summarise(Henry_Hub_Monthly_Avg = mean(Henry.Hub.Natural.Gas.Spot.Price.Dollars.per.Million.Btu, na.rm = TRUE))

# Merge the two datasets by Month
combined_data <- merge(henry_hub_filtered, texas_elecgasprice, by = "Month")

# Convert Month back to Date for plotting
combined_data$Month <- as.Date(paste0(combined_data$Month, "-01"))

# Plot the data with small points added
ggplot(combined_data, aes(x = Month)) +
  geom_line(aes(y = Henry_Hub_Monthly_Avg, color = "Henry Hub")) +
  geom_line(aes(y = TexasElectric, color = "Texas Electric")) +
  geom_point(aes(y = Henry_Hub_Monthly_Avg, color = "Henry Hub"), size = .5) +  # Add points for Henry Hub
  geom_point(aes(y = TexasElectric, color = "Texas Electric"), size = .5) +    # Add points for Texas Electric
  labs(
    title = "Henry Hub vs EIC Monthly Averages for Texas Electric Power Consumers",
    x = "Month",
    y = "Dollars per Million Btu or Thousand Cubic Feet",
    color = "Legend"
  ) +
  theme_minimal()

