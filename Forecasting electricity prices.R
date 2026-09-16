rm(list = ls())
# 1. Install and load necessary libraries
# install.packages("tidyverse")
# install.packages("readxl")  # This is the crucial package for reading Excel sheets
library(tidyverse)
library(readxl)
library(reticulate)

library(DescTools) 
library(haven) 
library(stargazer) 
library(tidyverse) 
library(car) 
library(lmtest) 
library(AER) 
library(quantmod) 
library(dynlm)
library(AER) 
library(stargazer) 
library(readr) 
library(dplyr)
library(xts) 
library(aTSA) 
library(forecast) 
library(ggplot2) 
library(vars) 
library(strucchange) 
library(urca) 
library(rstatix) 
library(tidyverse) 
library(fGarch) 
library(Quandl)

# 2. Set your working directory
setwd("C:/Users/franc/OneDrive - CBS - Copenhagen Business School/Documents/Francesco Lassi CBS/Second semester/Business Project/Forecasting prices")

# 3. Define the function for Excel files
merge_gme_excel <- function(filename) {
  
  # A. Read PREZZI (Prices) sheet
  df_prezzi <- read_excel(filename, sheet = "Prezzi-Prices") %>%
    select(Date = 1, Hour = 2, matches("PUN"), Price_SUD = SUD) %>%
    rename(PUN = 3) 
  
  # B. Read ACQUISTI (Purchases) sheet
  df_acquisti <- read_excel(filename, sheet = "Acquisti-Purchases") %>%
    select(Date = 1, Hour = 2, Purchases_Total_Italy = 3, Purchases_SUD = SUD)
  
  # C. Read VENDITE (Sales) sheet
  df_vendite <- read_excel(filename, sheet = "Vendite-Sales") %>%
    select(Date = 1, Hour = 2, Sales_Total_Italy = 3, Sales_SUD = SUD)
  
  # D. Read INVENDUTE (Unsold Volumes) sheet
  df_unsold <- read_excel(filename, sheet = "Q invendute-Unsold volumes") %>%
    select(Date = 1, Hour = 2, Unsold_Total_Italy = 3)
  
  # E. Read HHI sheet
  df_hhi <- read_excel(filename, sheet = "HHI") %>%
    select(Date = 1, Hour = 2, HHI_SUD = SUD)
  
  # F. Read IOR-RSI sheet
  df_ior <- read_excel(filename, sheet = "IOR-RSI") %>%
    select(Date = 1, Hour = 2, IOR_SUD = SUD)
  
  # 4. Merge all dataframes together
  merged_df <- list(df_prezzi, df_acquisti, df_vendite, df_unsold, df_hhi, df_ior) %>%
    reduce(full_join, by = c("Date", "Hour"))
  
  # 5. Clean the date column and FORCE Hour to be a number
  merged_df <- merged_df %>%
    mutate(
      Date = ymd(as.character(Date)),
      Hour = as.numeric(Hour)
    )
  
  # 6. Save as a new, clean CSV file
  output_name <- paste0("Merged_", str_replace(filename, "\\.xlsx", ""), ".csv")
  write_csv(merged_df, output_name)
  
  message("Successfully merged and saved: ", output_name)
  return(merged_df)
}

# --- RE-RUN THE FILES ---
df_2019 <- merge_gme_excel("Anno 2019_12.xlsx")
df_2020 <- merge_gme_excel("Anno 2020_12.xlsx")
df_2021 <- merge_gme_excel("Anno 2021_12.xlsx")
df_2022 <- merge_gme_excel("Anno 2022_12.xlsx")
df_2023 <- merge_gme_excel("Anno 2023_12.xlsx")
df_2024 <- merge_gme_excel("Anno 2024_12.xlsx")
df_2025 <- merge_gme_excel("Anno 2025_12_60.xlsx") 
df_2026 <- merge_gme_excel("Anno 2026_04_60.xlsx") 

# --- STACK THEM ALL TOGETHER ---
final_master_dataset <- bind_rows(df_2019, df_2020, df_2021, df_2022, df_2023, df_2024, df_2025, df_2026)
write_csv(final_master_dataset, "GME_Master_Dataset_2019_2026.csv")

view(final_master_dataset)

##############
#GAS
##############
# 2. Define a function to extract and clean the Gas data
process_gas_excel <- function(filename) {
  
  # Read the specific sheet. We assign our own clean column names immediately 
  # to override GME's messy multi-line headers.
  df <- read_excel(filename, 
                   sheet = "MGP-GAS - Negoziazione continua", 
                   col_names = c("Date_Raw", "Price_EUR_MWh", "Volume_MW", "Volume_MWh"))
  
  # Clean the data
  df_clean <- df %>%
    # MAGIC TRICK: Keep ONLY rows where the Date column starts with "20" (e.g., 20191001). 
    # This automatically deletes the 5 rows of blank space and text at the top!
    filter(str_detect(Date_Raw, "^20\\d{6}")) %>%
    mutate(
      Date = ymd(Date_Raw),                           # Convert to real Date format
      Price_EUR_MWh = as.numeric(Price_EUR_MWh),      # Force to number
      Volume_MWh = as.numeric(Volume_MWh)             # Force to number
    ) %>%
    # We drop the raw date and the MW column, keeping just the pure Date, Price, and MWh Volume
    select(Date, Price_EUR_MWh, Volume_MWh) 
  
  message("Successfully extracted gas data from: ", filename)
  return(df_clean)
}

# 3. Create a list of your exact file names
gas_files <- c(
  "Annotermico_2019-2020_09.xlsx",
  "Annotermico_2020-2021_09.xlsx",
  "Annotermico_2021-2022_09.xlsx",
  "Annotermico_2022-2023_09.xlsx",
  "Annotermico_2023-2024_09.xlsx",
  "Annotermico_2024-2025_09.xlsx",
  "Annotermico_2025-2026_04.xlsx"
)

# 4. Run the function on all files and stack them into one master dataset!
# map_dfr is a tidyverse function that runs a loop and binds the rows automatically
master_gas_dataset <- map_dfr(gas_files, process_gas_excel)

view(master_gas_dataset)

# 5. Save the final file to your folder
write_csv(master_gas_dataset, "Master_Gas_Prices_2019_2026.csv")

message("All gas files successfully merged! Final dataset has ", nrow(master_gas_dataset), " days of data.")

###########
#TOTAL LOAD; SOLAR AND WIND GENERATION
###########

#####
#TOTAL LOAD
#####

# 1. Load the required libraries
library(tidyverse)
library(lubridate)

# 2. Define the new function to process and pivot Total Load
process_load_wide <- function(filename) {
  
  # A. Read using read_excel
  df_hourly <- read_excel(filename) %>%
    mutate(
      # THE MAGIC FIX: Use as.POSIXct to save the 00:00:00 midnight rows!
      Datetime = as.POSIXct(Date, tz = "UTC"),
      Date_Clean = as.Date(floor_date(Datetime, "hour")),
      Hour_Clean = hour(floor_date(Datetime, "hour")) + 1 
    ) %>%
    # Remove any hidden footer rows at the bottom of the Excel files
    filter(!is.na(Date_Clean)) %>%
    # Group by Date, Hour, AND Bidding Zone so we keep Italy/South separated
    group_by(Date_Clean, Hour_Clean, `Bidding Zone`) %>%
    summarise(
      Total_Load = mean(`Total Load [MW]`, na.rm = TRUE),
      Forecast_Load = mean(`Forecast Total Load [MW]`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # B. PIVOT from Long to Wide!
  df_wide <- df_hourly %>%
    pivot_wider(
      names_from = `Bidding Zone`,
      values_from = c(Total_Load, Forecast_Load),
      names_glue = "{.value}_{`Bidding Zone`}" 
    ) %>%
    # C. Rename the columns exactly as you requested
    select(
      Date = Date_Clean,
      Hour = Hour_Clean,
      `Total Load South` = Total_Load_South,
      `Forecast Total Load South` = Forecast_Load_South,
      `Total Load Italy` = Total_Load_Italy,
      `Forecast Total Load Italy` = Forecast_Load_Italy
    ) %>%
    # D. Sort perfectly chronologically
    arrange(Date, Hour)
  
  return(df_wide)
}

# 3. Find your load files
load_files <- list.files(pattern = "totalload.*\\.xlsx$", ignore.case = TRUE)

print(load_files)

master_load_wide <- map_dfr(load_files, process_load_wide) %>%
  distinct(Date, Hour, .keep_all = TRUE) %>%
  arrange(Date, Hour)

# 2. Filter out any row where the Date is NA
master_load_wide <- master_load_wide %>%
  filter(!is.na(Date))

# 3. Overwrite the file with the clean version
write_csv(master_load_wide, "Master_Total_Load_Wide_2021_2026.csv")

view(master_load_wide)
message("Success! Check your folder for Master_Total_Load_Wide_2021_2026.csv")

#####
#SOLAR
#####

# 3. Define the function to process Solar files (UPDATED to read_excel)
process_solar <- function(filename) {
  
  # Read using read_excel
  df_raw <- read_excel(filename) 
  
  # Process and clean the data
  df_clean <- df_raw %>%
    mutate(
      # THE MAGIC FIX: Use as.POSIXct instead of ymd_hms to save Midnight!
      Datetime = as.POSIXct(Date, tz = "UTC"),
      Date_Clean = as.Date(floor_date(Datetime, "hour")),
      Hour_Clean = hour(floor_date(Datetime, "hour")) + 1
    ) %>%
    # Group by Date and Hour to compress any 15-min data into Hourly averages
    group_by(Date_Clean, Hour_Clean) %>%
    summarise(
      Solar_MW = mean(`Renewable Generation`, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    # Remove any hidden footer rows at the bottom of the files!
    filter(!is.na(Date_Clean)) %>%
    # Rename columns to match our master formats
    rename(Date = Date_Clean, Hour = Hour_Clean)
  
  return(df_clean)
}

# 4. Find all the Wind CSV files in your folder
solar_files <- list.files(pattern = "solar.*\\.xlsx$", ignore.case = TRUE)

# Optional: Print to verify R sees all 6 files!
print(solar_files)

# Now run the mapping again!
master_solar <- map_dfr(solar_files, process_solar) %>%
  distinct(Date, Hour, .keep_all = TRUE) %>%
  arrange(Date, Hour)

write_csv(master_solar, "Master_Solar_Generation_2021_2026.csv")
message("Success! Total solar hours successfully aligned and sorted: ", nrow(master_solar))

view(master_solar)

#####
#WIND
#####

# 3. Define the function to process Wind files (Midnight Bug Fixed!)
process_wind <- function(filename) {
  
  # Read using read_excel!
  df_raw <- read_excel(filename) 
  
  # Process and clean the data
  df_clean <- df_raw %>%
    mutate(
      # THE MAGIC FIX: Use as.POSIXct to save the 00:00:00 midnight rows!
      Datetime = as.POSIXct(Date, tz = "UTC"),
      Date_Clean = as.Date(floor_date(Datetime, "hour")),
      Hour_Clean = hour(floor_date(Datetime, "hour")) + 1
    ) %>%
    # Group by Date and Hour to compress any 15-min data into Hourly averages
    group_by(Date_Clean, Hour_Clean) %>%
    summarise(
      Wind_MW = mean(`Renewable Generation`, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    # Remove any hidden footer rows at the bottom of the files!
    filter(!is.na(Date_Clean)) %>%
    # Rename columns to match our master formats
    rename(Date = Date_Clean, Hour = Hour_Clean)
  
  return(df_clean)
}

# 4. Find all the Wind Excel files in your folder
wind_files <- list.files(pattern = "wind.*\\.xlsx$", ignore.case = TRUE)

print(wind_files)

# 5. Process all files, stack them, and remove any New Year's timezone duplicates
master_wind <- map_dfr(wind_files, process_wind) %>%
  distinct(Date, Hour, .keep_all = TRUE) %>%
  arrange(Date, Hour)

# 6. Save the final dataset
write_csv(master_wind, "Master_Wind_Generation_2021_2026.csv")

message("Success! Total wind hours successfully aligned and sorted: ", nrow(master_wind))

view(master_wind)

###########
#FINAL DATASET
###########

# 1. Load your building blocks (assuming they are in your working directory)
df_gme <- read_csv("GME_Master_Dataset_2019_2026.csv", show_col_types = FALSE)
df_load <- read_csv("Master_Total_Load_Wide_2021_2026.csv", show_col_types = FALSE)
df_solar <- read_csv("Master_Solar_Generation_2021_2026.csv", show_col_types = FALSE)
df_wind <- read_csv("Master_Wind_Generation_2021_2026.csv", show_col_types = FALSE)
df_gas <- read_csv("Master_Gas_Prices_2019_2026.csv", show_col_types = FALSE)

# 2. Merge all the HOURLY data together first
master_hourly <- df_gme %>%
  # Use left_join to keep the GME hours as the absolute spine of your dataset
  left_join(df_load, by = c("Date", "Hour")) %>%
  left_join(df_solar, by = c("Date", "Hour")) %>%
  left_join(df_wind, by = c("Date", "Hour"))

# 3. Merge the DAILY Gas data (The "Broadcast" Step)
final_econometric_dataset <- master_hourly %>%
  # Notice we ONLY join by "Date" here! 
  left_join(df_gas, by = "Date") %>%
  # 4. Calculate your Residual Load variable!
  mutate(
    # If solar or wind is missing (NA), treat it as 0 to prevent math errors
    Solar_MW = replace_na(Solar_MW, 0),
    Wind_MW = replace_na(Wind_MW, 0),
    # Calculate Residual Load for Italy
    Residual_Load_Italy = `Total Load Italy` - (Solar_MW + Wind_MW)
  ) %>%
  arrange(Date, Hour)

# 5. Save your ultimate Master Dataset
write_csv(final_econometric_dataset, "FINAL_DATASET_2019_2026.csv")
view(final_econometric_dataset)

message("Boom! Final merge complete. Your dataset has ", nrow(final_econometric_dataset), " rows and ", ncol(final_econometric_dataset), " columns.")

# Load the master dataset
master_data <- read_csv("FINAL_DATASET_2019_2026.csv")

# Create a subset for your multivariate regressions (2021-2026 only)
model_data <- master_data %>%
  filter(year(Date) >= 2021) 
# Note: Requires the lubridate package for the year() function

view(model_data)
view(master_data)

###########
# EXPLORATORY DATA ANALYSIS (EDA)
###########

# 1. Check number of observations and variables
cat("\n--- DIMENSIONS ---\n")
cat("Number of rows (observations):", nrow(model_data), "\n")
cat("Number of columns (variables):", ncol(model_data), "\n")

cat("\n--- VARIABLE TYPES & FIRST LOOK ---\n")
# glimpse() is a tidyverse function that shows every column, its type (dbl = numeric, Date = date), and first values
glimpse(model_data) 

# 2 & 3. Identify Data Issues and Count Missing Values (NAs)
cat("\n--- MISSING VALUES (NAs) ---\n")
# This counts exactly how many missing values exist in every single column
missing_values <- colSums(is.na(model_data))
# We filter to only show columns that actually have missing data
print(missing_values[missing_values > 0])

cat("\n--- SUMMARY STATISTICS ---\n")
# summary() is incredibly powerful. It shows the Min, Max, Mean, and Quartiles for every numeric variable.
# Look closely at the Min values: Are there any impossible negative numbers in Load or Generation?
summary(model_data)

# 1. Install and load the zoo package if you don't have it
# install.packages("zoo")
library(zoo)

# 2. Interpolate the missing values in your model data
model_data <- model_data %>%
  # We group by nothing to ensure it interpolates across the whole column
  mutate(
    `Total Load South` = na.approx(`Total Load South`, na.rm = FALSE),
    `Forecast Total Load South` = na.approx(`Forecast Total Load South`, na.rm = FALSE),
    `Total Load Italy` = na.approx(`Total Load Italy`, na.rm = FALSE),
    `Forecast Total Load Italy` = na.approx(`Forecast Total Load Italy`, na.rm = FALSE),
    
    # Recalculate the Residual Load now that the base Load is fixed!
    Residual_Load_Italy = `Total Load Italy` - (Solar_MW + Wind_MW)
  )

# 3. Verify it worked! (This should now print nothing, meaning 0 NAs)
cat("\n--- MISSING VALUES AFTER INTERPOLATION ---\n")
missing_values_fixed <- colSums(is.na(model_data))
print(missing_values_fixed[missing_values_fixed > 0])

# 4. Distribution of the Target Variable (PUN)
# Plot 1: Histogram (The Overall Shape)
p1 <- ggplot(model_data, aes(x = Price_SUD)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "darkblue", alpha = 0.7) +
  labs(
    title = "Distribution of Electricity Price in the South",
    subtitle = "Years 2021-2026",
    x = "Price (€/MWh)",
    y = "Frequency (Number of Hours)"
  ) +
  theme_minimal()

print(p1)

# Plot 2: Boxplot by Year (To spot volatility, outliers, and structural breaks!)
p2 <- ggplot(model_data, aes(x = as.factor(year(Date)), y = Price_SUD, fill = as.factor(year(Date)))) +
  # We highlight outliers in red because extreme price spikes are the hardest part of forecasting!
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16, outlier.alpha = 0.5) +
  labs(
    title = "South Price Volatility by Year",
    x = "Year",
    y = "Price (€/MWh)",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

# Make sure you have the scales package loaded for nice date axes
# install.packages("scales")
library(scales)

# First, we need to combine your 'Date' and 'Hour' into a single continuous time variable
model_data <- model_data %>%
  mutate(
    # Create a true continuous Datetime column for plotting
    Datetime = as.POSIXct(paste(Date, Hour - 1, "00", "00", sep = ":"), format = "%Y-%m-%d:%H:%M:%S", tz = "UTC")
  )

cat("\n--- GENERATING TIME SERIES PLOTS ---\n")

# Plot 1: The Full Hourly Time Series (Macro View)
p_timeseries <- ggplot(model_data, aes(x = Datetime, y = Price_SUD)) +
  geom_line(color = "midnightblue", alpha = 0.6, linewidth = 0.2) +
  # Add a smoothed trend line to cut through the extreme hourly noise
  geom_smooth(method = "loess", color = "red", span = 0.1, se = FALSE, linewidth = 1) +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "South Electricity Price Over Time",
    subtitle = "Hourly spot prices with smoothed macro trend (2021-2026)",
    x = "Date",
    y = "Price (€/MWh)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_timeseries)

# Plot 2: The Average Daily Profile (Micro View)
# This shows the classic "duck curve" of solar impact
daily_profile <- model_data %>%
  group_by(Hour) %>%
  summarise(Average_Price_SUD = mean(Price_SUD, na.rm = TRUE))

p_daily <- ggplot(daily_profile, aes(x = Hour, y = Average_Price_SUD)) +
  geom_line(color = "darkorange", linewidth = 1.5) +
  geom_point(color = "darkred", size = 3) +
  scale_x_continuous(breaks = 1:24) +
  labs(
    title = "Average Daily Price Profile",
    subtitle = "How prices behave throughout a typical 24-hour period",
    x = "Hour of the Day (1 = Midnight, 14 = 2 PM)",
    y = "Average Price (€/MWh)"
  ) +
  theme_minimal()

print(p_daily)

###########
# ADVANCED EDA: DISTRIBUTIONS & CORRELATIONS
###########

# 1. Isolate only the numeric variables for math/plotting
numeric_data <- model_data %>%
  select(where(is.numeric)) %>%
  # Drop 'Hour' if it's numeric because we already have 'Hour_Factor' for models, 
  # but keeping it is fine for the correlation matrix. Let's drop Year/Month/Date if they snuck in.
  select(-any_of(c("Year", "Month_Num"))) 

cat("\n--- PLOTTING DISTRIBUTIONS ---\n")

# 2. Plot the distribution of EVERY single numeric variable
# We use pivot_longer to stack all variables so ggplot can create a massive grid
long_numeric <- numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

p_all_dist <- ggplot(long_numeric, aes(x = Value)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  # facet_wrap creates a separate mini-chart for every variable!
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distributions of All Numeric Variables",
    subtitle = "Check for extreme right-skews (tails) or bimodal (two-peak) shapes",
    x = "Value",
    y = "Frequency"
  )

print(p_all_dist)

cat("\n--- PLOTTING CORRELATION HEATMAP ---\n")

# 3. Calculate the mathematical correlation matrix
# use = "complete.obs" ensures it doesn't break if a stray NA exists
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Convert the matrix into a format ggplot can read
cor_df <- as.data.frame(as.table(cor_matrix))

# 4. Plot the Full Correlation Heatmap
p_heatmap <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  # Dark blue = positive correlation, Dark red = negative correlation
  scale_fill_gradient2(low = "darkred", high = "darkblue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(
    title = "Full Correlation Heatmap",
    subtitle = "Look out for variables that are highly correlated with EACH OTHER (Multicollinearity)",
    x = "", y = ""
  )

print(p_heatmap)

# 5. BONUS: Focused "Price_SUD" Correlation Bar Chart
# The heatmap is great, but a bar chart showing exactly how everything relates to PUN is better for modeling!
sud_cor <- as.data.frame(cor_matrix) %>%
  select(Price_SUD) %>%
  rownames_to_column(var = "Variable") %>%
  filter(Variable != "Price_SUD") # Remove Price_SUD vs Price_SUD (which is obviously 1.0)

p_sud_cor <- ggplot(sud_cor, aes(x = reorder(Variable, Price_SUD), y = Price_SUD, fill = Price_SUD)) +
  geom_col(color = "black", alpha = 0.8) +
  coord_flip() + # Flip to horizontal for easy reading
  scale_fill_gradient2(low = "darkred", high = "darkblue", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Variable Correlation with South Electricity Price",
    subtitle = "Which variables drive the price up (Blue) or down (Red)?",
    x = "Exogenous Variable",
    y = "Correlation Coefficient (Pearson)"
  )

print(p_sud_cor)

# Drop the leaky and multicollinear columns
model_data <- model_data %>%
  select(
    -PUN,
    -`Forecast Total Load South`,
    -`Forecast Total Load Italy`
  )

# Verify they have been successfully removed
cat("\n--- REMAINING COLUMNS ---\n")
print(colnames(model_data))

# 1. Add the missing Calendar / Seasonal Features
model_data <- model_data %>%
  mutate(
    # Extract the month (1 to 12) and turn it into a category
    Month = as.factor(month(Date)),
    
    # Extract the day of the week (Monday, Tuesday, etc.)
    DayOfWeek = as.factor(wday(Date, label = TRUE, abbr = FALSE, week_start = 1)), 
    
    # Create a binary flag: 1 if it's Saturday/Sunday, 0 if it's a weekday
    IsWeekend = as.factor(ifelse(wday(Date, week_start = 1) %in% c(6, 7), 1, 0)),
    
    # Turn the numeric Hour (1-24) into a category so the model learns the "Daily Shape"
    Hour_Factor = as.factor(Hour)
  )

cat("\n--- CALENDAR FEATURES SUCCESSFULLY ADDED ---\n")
# Check the columns to verify they are there
print(tail(colnames(model_data), 4))

view(model_data)
summary(model_data)
str(model_data)


############
#ANALYSIS
############

###Price SUD has as min 0, so we have to transform differently
#log(1+x)
#I transform even other variables
#After checking for the distributions, I have decided to transform just the gas
#and the wind, even if it is not necessary. There rest is pointless

#model_data$Price_SUD <- log1p(model_data$Price_SUD)-log1p(lag(model_data$Price_SUD, 1))
model_data$Price_EUR_MWh <- log(model_data$Price_EUR_MWh)
#model_data$Solar_MW <- log1p(model_data$Solar_MW)
model_data$Wind_MW <- log1p(model_data$Wind_MW)
#model_data$IOR_SUD = log1p(model_data$IOR_SUD)

summary(model_data)

cat("\n--- CHECKING DISTRIBUTIONS OF TRANSFORMED VARIABLES ---\n")

# 1. Isolate the 5 transformed variables and stack them for a grid plot
transformed_vars <- model_data %>%
  select(Price_SUD, Price_EUR_MWh, Solar_MW, Wind_MW, IOR_SUD) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# 2. Plot the new distributions
p_transformed_dist <- ggplot(transformed_vars, aes(x = Value)) +
  # Using green to easily distinguish these as the "transformed" plots
  geom_histogram(bins = 50, fill = "seagreen", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distributions of Transformed Variables",
   # subtitle = "Checking the effect of log() and log1p() transformations",
    x = "Transformed Value",
    y = "Frequency"
  )

print(p_transformed_dist)


cat("\n--- PLOTTING TIME SERIES OF TRANSFORMED PRICE_SUD ---\n")

# 3. Plot the time series for the new log1p(Price_SUD)
#p_transformed_timeseries <- ggplot(model_data, aes(x = Datetime, y = Price_SUD)) +
#  geom_line(color = "midnightblue", alpha = 0.6, linewidth = 0.2) +
#  # Add a smoothed trend line to cut through the noise
#  geom_smooth(method = "loess", color = "red", span = 0.1, se = FALSE, linewidth = 1) +
#  scale_x_datetime(date_breaks = "6 months", date_labels = "%b %Y") +
#  labs(
#    title = "Transformed South Electricity Price Over Time",
#    subtitle = "Target Variable: log1p(Price_SUD). Notice how the 2022 extreme spikes are now compressed!",
#    x = "Date",
#    y = "Log Price (log1p)"
#  ) +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#print(p_transformed_timeseries)
#

write_csv(model_data, "MODEL_DATA.csv")

par(mfrow=c(1,1))
acf(model_data$Price_SUD)
pacf(model_data$Price_SUD)

# 1. Install the package if you haven't already
# install.packages("tseries")
library(tseries)

cat("\n--- AUGMENTED DICKEY-FULLER TEST (RAW PUN) ---\n")

# 2. Run the ADF Test on the raw PUN column
# Note: We use na.omit() to ensure no stray missing values break the math
adf_raw <- tseries::adf.test(na.omit(model_data$Price_SUD), alternative = "stationary")
print(adf_raw)

########
model_data$logPrice_SUD <- log1p(model_data$Price_SUD)-log1p(lag(model_data$Price_SUD, 1))
acf(na.omit(model_data$logPrice_SUD))
pacf(na.omit(model_data$logPrice_SUD))
par(mfrow=c(1,1))

adf_raw <- tseries::adf.test(na.omit(model_data$logPrice_SUD), alternative = "stationary")
print(adf_raw)
########
acf(na.omit(model_data$Price_EUR_MWh))
pacf(na.omit(model_data$Price_EUR_MWh))
par(mfrow=c(1,1))

adf_raw <- tseries::adf.test(na.omit(model_data$Price_EUR_MWh), alternative = "stationary")
print(adf_raw)

view(model_data)
# ---------------------------------------------------------
# BASELINE MODEL: AUTO ARIMA
# ---------------------------------------------------------
library(forecast)

cat("\n--- PREPARING DATA FOR ARIMA ---\n")

# 1. Isolate the target variable and drop the NA created by the lag/differencing
# auto.arima cannot handle missing values at the start of the series
y_returns_clean <- na.omit(model_data$logPrice_SUD)

# 2. Convert the raw vector into a Time Series (ts) object
# frequency = 24 tells the model there is a daily seasonal cycle in your hourly data
y_ts <- ts(y_returns_clean, frequency = 24)

cat("\n--- ESTIMATING AUTO.ARIMA (This may take a few minutes...) ---\n")

# 3. Run the auto.arima algorithm
auto_model <- auto.arima(
  y_ts,
  stepwise = TRUE,       # Uses a smart search path instead of testing every single combination
  approximation = TRUE,  # Uses a faster, approximate estimation method during the search
  trace = TRUE           # Prints the AICc score of each tested model to the console so you can watch it work
)

cat("\n--- ARIMA MODEL SUMMARY ---\n")
# 4. Print the final "winning" model coefficients and error metrics
summary(auto_model)

cat("\n--- RESIDUAL DIAGNOSTICS ---\n")
# 5. Check if the model captured all the information (residuals should look like white noise)
checkresiduals(auto_model)

# ---------------------------------------------------------
# CROSS-CORRELATION FUNCTION (CCF) FOR ALL VARIABLES
# ---------------------------------------------------------

cat("\n--- GENERATING CCF PLOTS ---\n")

# 1. Define your target variable and the list of exogenous variables
target_var <- "logPrice_SUD"

# Note: I am including the key variables you processed in your script.
# You can add or remove exact column names from this list as needed.
exo_vars <- c("Wind_MW", "Solar_MW", "Residual_Load_Italy", "Price_EUR_MWh", "Unsold_Total_Italy")

# 2. Set up a plotting grid so they all appear in one window 
# (2 rows, 3 columns will fit up to 6 plots)
par(mfrow = c(2, 3))

# 3. Loop through each exogenous variable and plot its CCF against the target
for (var in exo_vars) {
  
  # Temporarily isolate the target and the current exogenous variable
  # We use na.omit() on the pair to ensure no NAs break the math
  temp_data <- model_data %>%
    select(all_of(target_var), all_of(var)) %>%
    na.omit()
  
  # Calculate and plot the CCF
  ccf(temp_data[[target_var]], 
      temp_data[[var]], 
      lag.max = 48, # Looking back up to 48 hours
      main = paste(target_var, "vs", var),
      ylab = "Cross-Correlation")
}

# 4. Reset the plot window back to standard 1x1 layout
par(mfrow = c(1, 1))

# ---------------------------------------------------------
# MULTIVARIATE ADL MODEL: 1-HOUR & 24-HOUR LAGS
# ---------------------------------------------------------

cat("\n--- PREPARING DATA FOR ADL MODEL ---\n")

# 1. Create all the lagged variables directly in the dataframe
adl_data <- model_data %>%
  mutate(
    # Target Variable Lags (Price)
    logPrice_SUD_lag1 = lag(logPrice_SUD, 1),
    logPrice_SUD_lag24 = lag(logPrice_SUD, 24),
    
    # Wind Generation Lags
    Wind_lag1 = lag(Wind_MW, 1),
    Wind_lag24 = lag(Wind_MW, 24),
    
    # Solar Generation Lags
    Solar_lag1 = lag(Solar_MW, 1),
    Solar_lag24 = lag(Solar_MW, 24),
    
    # Residual Load Lags
    ResLoad_lag1 = lag(Residual_Load_Italy, 1),
    ResLoad_lag24 = lag(Residual_Load_Italy, 24),
    
    # Gas Price Lags (Note: Price_EUR_MWh was log-transformed earlier in your script)
    Gas_lag1 = lag(Price_EUR_MWh, 1),
    Gas_lag24 = lag(Price_EUR_MWh, 24),
    
    # Unsold Volumes Lags
    Unsold_lag1 = lag(Unsold_Total_Italy, 1),
    Unsold_lag24 = lag(Unsold_Total_Italy, 24)
  ) %>%
  # Select only the columns needed for the regression to keep data clean
  select(
    logPrice_SUD, 
    logPrice_SUD_lag1, logPrice_SUD_lag24,
    Wind_lag1, Wind_lag24,
    Solar_lag1, Solar_lag24,
    ResLoad_lag1, ResLoad_lag24,
    Gas_lag1, Gas_lag24,
    Unsold_lag1, Unsold_lag24
  ) %>%
  # Drop the NAs created by shifting the data backwards by 24 hours
  na.omit()

cat("\n--- ESTIMATING ADL MODEL ---\n")

# 2. Run the Ordinary Least Squares (OLS) regression
adl_model_full <- lm(
  logPrice_SUD ~ logPrice_SUD_lag1 + logPrice_SUD_lag24 + 
    Wind_lag1 + Wind_lag24 + 
    Solar_lag1 + Solar_lag24 + 
    ResLoad_lag1 + ResLoad_lag24 + 
    Gas_lag1 + Gas_lag24 + 
    Unsold_lag1 + Unsold_lag24,
  data = adl_data
)

# 3. Print the comprehensive summary
summary(adl_model_full)

# ---------------------------------------------------------
# HEAD-TO-HEAD EXPANDING WINDOW BACKTEST: ADL vs ARIMA
# ---------------------------------------------------------
library(forecast)

cat("\n--- STARTING HEAD-TO-HEAD BACKTEST ---\n")

# 1. Define window parameters
total_obs <- nrow(adl_data)
initial_train_size <- floor(0.8 * total_obs)
step_size <- 720 # 1 week (168 hours), 720 is one month

# 2. Create empty vectors to store predictions and actuals
actuals <- c()
preds_adl <- c()
preds_arima <- c()

cat("Running loop. This may take a few minutes...\n")

# 3. The Walk-Forward Loop
for (i in seq(initial_train_size, total_obs - step_size, by = step_size)) {
  
  # Define Train and Test sets
  #train_set <- adl_data[1:i, ] #expanding window
  train_set <- adl_data[(i - 8760 + 1):i, ] #rolling window
  test_set <- adl_data[(i + 1):(i + step_size), ]
  
  # --- A. FIT & PREDICT ADL MODEL ---
  temp_adl <- lm(
    logPrice_SUD ~ logPrice_SUD_lag1 + logPrice_SUD_lag24 + 
      Wind_lag1 + Wind_lag24 + 
      Solar_lag1 + Solar_lag24 + 
      ResLoad_lag1 + ResLoad_lag24 + 
      Gas_lag1 + Gas_lag24 + 
      Unsold_lag1 + Unsold_lag24,
    data = train_set
  )
  temp_adl_pred <- predict(temp_adl, newdata = test_set)
  
  # --- B. FIT & PREDICT ARIMA MODEL ---
  # Convert training target to time series
  y_train_ts <- ts(train_set$logPrice_SUD, frequency = 24)
  
  # Fit the pre-selected ARIMA(5,0,0)(2,0,0)[24] zero mean model
  temp_arima <- Arima(
    y_train_ts, 
    order = c(5, 0, 0), 
    seasonal = list(order = c(2, 0, 0), period = 24), 
    include.mean = FALSE
  )
  
  # Forecast the next 'step_size' hours
  # $mean extracts just the point forecasts (ignoring confidence intervals)
  temp_arima_pred <- as.numeric(forecast(temp_arima, h = step_size)$mean)
  
  # --- C. STORE RESULTS ---
  actuals <- c(actuals, test_set$logPrice_SUD)
  preds_adl <- c(preds_adl, temp_adl_pred)
  preds_arima <- c(preds_arima, temp_arima_pred)
}

cat("\n--- BACKTEST COMPLETE ---\n")

# ---------------------------------------------------------
# CALCULATING & COMPARING METRICS
# ---------------------------------------------------------

# Total Sum of Squares (SST) for R-squared calculations
sst <- sum((actuals - mean(actuals))^2)

# ADL Metrics
mse_adl <- mean((actuals - preds_adl)^2)
rmse_adl <- sqrt(mse_adl)
ssr_adl <- sum((actuals - preds_adl)^2)
r2_adl <- 1 - (ssr_adl / sst)

# ARIMA Metrics
mse_arima <- mean((actuals - preds_arima)^2)
rmse_arima <- sqrt(mse_arima)
ssr_arima <- sum((actuals - preds_arima)^2)
r2_arima <- 1 - (ssr_arima / sst)

# ---------------------------------------------------------
# PRINT THE FINAL SCORECARD
# ---------------------------------------------------------

cat("\n=========================================\n")
cat("      OUT-OF-SAMPLE MODEL COMPARISON       \n")
cat("=========================================\n\n")

cat("--- BASELINE: ARIMA(5,0,0)(2,0,0)[24] ---\n")
cat("RMSE: ", round(rmse_arima, 5), "\n")
cat("MSE:  ", round(mse_arima, 5), "\n")
cat("R2:   ", round(r2_arima, 5), "\n\n")

cat("--- MULTIVARIATE: ADL MODEL ---\n")
cat("RMSE: ", round(rmse_adl, 5), "\n")
cat("MSE:  ", round(mse_adl, 5), "\n")
cat("R2:   ", round(r2_adl, 5), "\n\n")

# Calculate the "Edge"
edge_pct <- ((rmse_arima - rmse_adl) / rmse_arima) * 100

cat("=========================================\n")
if (rmse_adl < rmse_arima) {
  cat("SUCCESS: ADL Model beat the Baseline!\n")
  cat("Error Reduction (Edge):", round(edge_pct, 2), "%\n")
} else {
  cat("RESULT: ARIMA Baseline won. The physical variables \n")
  cat("did not provide a measurable predictive edge out-of-sample.\n")
}
cat("=========================================\n")

# ---------------------------------------------------------
# EXPANDING WINDOW BACKTEST: RIDGE & LASSO ONLY
# ---------------------------------------------------------
# Make sure glmnet is loaded
library(glmnet)

cat("\n--- STARTING RIDGE & LASSO BACKTEST ---\n")

# 1. Create empty vectors for the penalized models
preds_ridge <- c()
preds_lasso <- c()

cat("\n--- STARTING RIDGE & LASSO BACKTEST (FIXED) ---\n")

# 1. RESET ALL VECTORS (This fixes the length mismatch!)
actuals <- c()
preds_ridge <- c()
preds_lasso <- c()

cat("\n--- STARTING RIDGE & LASSO BACKTEST (FIXED) ---\n")

# 1. RESET ALL VECTORS (This fixes the length mismatch!)
actuals <- c()
preds_ridge <- c()
preds_lasso <- c()

cat("Running Cross-Validation loop for penalized models...\n")

# 2. The Walk-Forward Loop 
for (i in seq(initial_train_size, total_obs - step_size, by = step_size)) {
  
  train_set <- adl_data[1:i, ]
  test_set <- adl_data[(i + 1):(i + step_size), ]
  
  x_train <- as.matrix(train_set %>% select(-logPrice_SUD))
  y_train <- train_set$logPrice_SUD
  x_test <- as.matrix(test_set %>% select(-logPrice_SUD))
  
  # Ridge
  cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, standardize = TRUE)
  temp_ridge_pred <- predict(cv_ridge, s = "lambda.min", newx = x_test)
  
  # Lasso
  cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, standardize = TRUE)
  temp_lasso_pred <- predict(cv_lasso, s = "lambda.min", newx = x_test)
  
  # RECORD ALL THREE TOGETHER
  actuals <- c(actuals, test_set$logPrice_SUD)
  preds_ridge <- c(preds_ridge, as.numeric(temp_ridge_pred))
  preds_lasso <- c(preds_lasso, as.numeric(temp_lasso_pred))
}

cat("\n--- BACKTEST COMPLETE ---\n")

# ---------------------------------------------------------
# CALCULATE METRICS & FINAL SCORECARD
# ---------------------------------------------------------

# 1. Helper function for clean metric calculation
calc_metrics <- function(actual, pred, sst_val) {
  mse <- mean((actual - pred)^2)
  rmse <- sqrt(mse)
  ssr <- sum((actual - pred)^2)
  r2 <- 1 - (ssr / sst_val)
  return(c(RMSE = rmse, MSE = mse, R2 = r2))
}

# 2. Total Sum of Squares (reusing the 'actuals' vector from your previous run)
sst <- sum((actuals - mean(actuals))^2)

# 3. Calculate metrics for the new models
metrics_ridge <- calc_metrics(actuals, preds_ridge, sst)
metrics_lasso <- calc_metrics(actuals, preds_lasso, sst)

# 4. Recalculate metrics for the old models so everything is uniform
metrics_arima <- calc_metrics(actuals, preds_arima, sst)
metrics_adl <- calc_metrics(actuals, preds_adl, sst)

# ---------------------------------------------------------
# FORMATTED SCORECARD OUTPUT
# ---------------------------------------------------------

cat("\n======================================================\n")
cat("            FINAL OUT-OF-SAMPLE SCORECARD             \n")
cat("======================================================\n\n")

# Print headers
cat(sprintf("%-25s %-10s %-10s %-10s\n", "Model", "RMSE", "MSE", "R2"))
cat("------------------------------------------------------\n")

# Print rows for each model
cat(sprintf("%-25s %-10.5f %-10.5f %-10.5f\n", 
            "1. ARIMA (Baseline)", metrics_arima["RMSE"], metrics_arima["MSE"], metrics_arima["R2"]))

cat(sprintf("%-25s %-10.5f %-10.5f %-10.5f\n", 
            "2. ADL (Standard OLS)", metrics_adl["RMSE"], metrics_adl["MSE"], metrics_adl["R2"]))

cat(sprintf("%-25s %-10.5f %-10.5f %-10.5f\n", 
            "3. RIDGE (Alpha = 0)", metrics_ridge["RMSE"], metrics_ridge["MSE"], metrics_ridge["R2"]))

cat(sprintf("%-25s %-10.5f %-10.5f %-10.5f\n", 
            "4. LASSO (Alpha = 1)", metrics_lasso["RMSE"], metrics_lasso["MSE"], metrics_lasso["R2"]))
cat("======================================================\n")

# ---------------------------------------------------------
# VISUALIZING OUT-OF-SAMPLE PERFORMANCE (ACTUAL VS LASSO)
# ---------------------------------------------------------
library(ggplot2)

cat("\n--- GENERATING FINAL PREDICTION PLOT ---\n")

# 1. Create a clean dataframe for plotting
# We use the length of the actuals to pull the correct dates from the end of adl_data
plot_data <- data.frame(
  # Get the exact dates/hours that correspond to our test set
  Datetime = model_data$Datetime[(total_obs - length(actuals) + 1):total_obs],
  Actual_Price = actuals,
  Lasso_Prediction = preds_lasso
)

# 2. To make the chart readable, let's plot just the final 2 weeks (336 hours)
plot_data_recent <- tail(plot_data, 336)

# 3. Generate the ggplot
p_final <- ggplot(plot_data_recent, aes(x = Datetime)) +
  geom_line(aes(y = Actual_Price, color = "Actual Price"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = Lasso_Prediction, color = "Lasso Forecast"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Actual Price" = "black", "Lasso Forecast" = "red")) +
  labs(
    title = "Out-of-Sample Performance: Actual vs Lasso Forecast",
    subtitle = "Final 2 Weeks of Testing Window",
    x = "Date",
    y = "Log Return (Electricity Price)",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_final)

# Save the plot to your folder so you can print it or put it on an iPad
ggsave("Final_Lasso_Forecast_Plot.png", plot = p_final, width = 10, height = 5, dpi = 300)

cat("Success! Plot saved as 'Final_Lasso_Forecast_Plot.png'\n")

# ---------------------------------------------------------
# LASSO MODEL SUMMARY & FEATURE SELECTION
# ---------------------------------------------------------

cat("\n--- LASSO FEATURE SELECTION SUMMARY ---\n")

# 1. Print the optimal Lambda (the exact penalty size that minimized your error)
cat("Optimal Penalty (Lambda Min):", cv_lasso$lambda.min, "\n\n")

# 2. Extract and print the coefficients at that optimal Lambda
# The 's' argument tells R to only show the coefficients for the winning model
lasso_coefs <- coef(cv_lasso, s = "lambda.min")

cat("Coefficients at Optimal Lambda:\n")
# We convert it to a standard matrix so it prints cleanly in the console
print(as.matrix(lasso_coefs))

# ---------------------------------------------------------
# VISUALIZING THE LASSO PENALTY
# ---------------------------------------------------------

# 3. Plot the cross-validation curve
# This creates a chart showing how the error changed as the penalty increased.
plot(cv_lasso)
title("Lasso Cross-Validation Curve", line = 2.5)
