if (!require("tidyverse")) install.packages("tidyverse")

# Let's start with Fare Revenue
library(tidyverse)
FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(
    -`State/Parent NTD ID`,
    -`Reporter Type`,
    -`Reporting Module`,
    -`TOS`,
    -`Passenger Paid Fares`,
    -`Organization Paid Fares`
  ) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`) |>
  group_by(
    `NTD ID`, # Sum over different `TOS` for the same `Mode`
    `Agency Name`, # These are direct operated and sub-contracted
    `Mode`
  ) |> # of the same transit modality
  # Not a big effect in most munis (significant DO
  # tends to get rid of sub-contractors), but we'll sum
  # to unify different passenger experiences
  summarize(`Total Fares` = sum(`Total Fares`)) |>
  ungroup()

# Next, expenses
EXPENSES <- readr::read_csv("MP01/2022_expenses.csv") |>
  select(
    `NTD ID`,
    `Agency`,
    `Total`,
    `Mode`
  ) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

# Monthly Transit Numbers
library(tidyverse)
TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet = "UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(
    -`Legacy NTD ID`,
    -`Reporter Type`,
    -`Mode/Type of Service Status`,
    -`UACE CD`,
    -`TOS`
  ) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`),
               names_to = "month",
               values_to = "UPT"
  ) |>
  drop_na() |>
  mutate(month = my(month)) # Parse _m_onth _y_ear date specs
MILES <- readxl::read_xlsx("ridership.xlsx", sheet = "VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(
    -`Legacy NTD ID`,
    -`Reporter Type`,
    -`Mode/Type of Service Status`,
    -`UACE CD`,
    -`TOS`
  ) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`),
               names_to = "month",
               values_to = "VRM"
  ) |>
  drop_na() |>
  group_by(
    `NTD ID`, `Agency`, `UZA Name`,
    `Mode`, `3 Mode`, month
  ) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month = my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))

# End of data ingestion and setup

if (!require("DT")) install.packages("DT")
library(DT)

sample_n(USAGE, 1000) |>
  mutate(month = as.character(month)) |>
  DT::datatable()

# Task 1
USAGE <- USAGE |>
  rename(metro_area = `UZA Name`)

# Task 2
library(dplyr)
unique_modes <- USAGE |>
  distinct(Mode)
print(unique_modes)
# DR, FB, MB, SR, TB, VP, CB, RB, LR, YR, MG, CR, AR, TR, HR, IP, PB, CC

USAGE <- USAGE |>
  mutate(Mode = case_when(
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "MB" ~ "Bus",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "VP" ~ "Vanpool",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "LR" ~ "Light Rail",
    Mode == "YR" ~ "Hybrid Rail",
    Mode == "MG" ~ "Monorail and Automated Guideway modes",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "PB" ~ "Publico",
    Mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"
  ))

# modify table output
datatable(
  sample_n(USAGE, 1000) |>
    mutate(month = as.character(month)) |>
    select(-`NTD ID`, -`3 Mode`) |> # exclude ntd id and 3 mode in visual table
    rename(
      `Metro Area` = metro_area, # rename for table output to look cleaner
      `Unlinked Passenger Trips` = UPT, # rename acronym in visual table
      `Vehicle Revenue Miles` = VRM # rename acronym in visual table
    )
)

# for sanity check that the ingestion dropped all NA values, so I wont be using that function every time
na_count <- sum(is.na(USAGE))
print(na_count)
# output is 0 so we are good so I will be dropping na.rm = TRUE in any aggregate functions

# Task 3
# 1. What transit agency had the most total VRM in this sample?
most_vrm_agency <- USAGE |>
  group_by(Agency) |>
  summarize(total_vrm = sum(VRM)) |>
  arrange(desc(total_vrm))
print(most_vrm_agency) 
# I chose to not use the slice function so I could get a look at how much more the first one had than the others

# If I wanted to output just the top rank, I would do the following
most_vrm_agency1 <- USAGE |>
  group_by(Agency) |>
  summarize(total_vrm = sum(VRM)) |>
  arrange(desc(total_vrm)) |>
  slice_head(n = 1)
print(most_vrm_agency1)

# 2. What transit mode had the most total VRM in this sample?
most_vrm_mode <- USAGE |>
  group_by(Mode) |>
  summarize(total_vrm = sum(VRM)) |>
  arrange(desc(total_vrm))
print(most_vrm_mode)

# 3. How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
nyc_subway_trips <- USAGE |>
  filter(
    Agency == "MTA New York City Transit",
    Mode == "Heavy Rail",
    month >= as.Date("2024-05-01") & month <= as.Date("2024-05-31")
  )
print(nyc_subway_trips)
# this confirmed there's only one entry for the month by the agency and mode which i found was a good sanity check

# Summarize and print the total number of trips (UPT)
total_trips <- nyc_subway_trips |>
  summarize(total_UPT = sum(UPT, na.rm = TRUE))
print(total_trips)

# 4. How much did NYC subway ridership fall between April 2019 and April 2020?
ride_fall <- USAGE |>
  filter(Mode == "Heavy Rail") |>
  filter(Agency == "MTA New York City Transit") |> # this is the agency that runs nyc subway
  filter(month %in% c(as.Date("2019-04-01"), as.Date("2020-04-01"))) |>
  group_by(month) |>
  summarize(total_rides = sum(UPT)) |>
  summarize(difference = total_rides[month == as.Date("2020-04-01")] -
              total_rides[month == as.Date("2019-04-01")])
print(ride_fall)

# Task 4 - Find three more interesting transit facts in this data other than those above.

# What UZA Name / metro area had the most UPT in January 2022?
popular_area <- USAGE |>
  filter(month %in% c(as.Date("2022-01-01"))) |>
  group_by(metro_area) |>
  summarize(total_trips = sum(UPT)) |>
  arrange(desc(total_trips))
print(popular_area)

# What month and year had the most UPT through the bus (MB) in the entire sample?
bus_trips <- USAGE |>
  filter(Mode == "Bus") |>
  group_by(month) |>
  summarize(total_bus_trips = sum(UPT)) |>
  arrange(desc(total_bus_trips))
print(bus_trips)

# What is the average amount of trips taken in the New York--Jersey City--Newark, NY--NJ area based on the season from 2018 - 2022?

seasonal_variation <- USAGE |>
  filter(metro_area == "New York--Jersey City--Newark, NY--NJ") |>
  filter(month >= as.Date("2018-01-01") & month <= as.Date("2022-12-01")) |>
  mutate(
    month_num = as.numeric(format(month, "%m")), # Extract the month as a number from the date column
    season = case_when( # Use case_when to categorize into seasons
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring",
      month_num %in% c(6, 7, 8) ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ "Unknown"
    )
  ) |>
  group_by(season) |>
  summarize(avg_trips = mean(UPT)) |>
  arrange(desc(avg_trips))
print(seasonal_variation)

# Task 5
# Calculate average UPT per agency per year to only consider agencies with an average UPT of 400,000 or more
agencies_avg_upt <- USAGE |>
  mutate(Year = year(month)) |> # Extract year from month
  group_by(`NTD ID`, Agency, Year) |> # Group by agency and year
  summarize(avg_upt = mean(UPT, na.rm = TRUE)) |> # Calculate average UPT per year
  filter(avg_upt >= 400000) |> # Keep agencies with avg UPT >= 400,000
  ungroup() |> # Ungroup to prepare for next operation
  distinct(Agency) # Get distinct agencies

# Filter the 2022 data for only those agencies
USAGE_2022_ANNUAL <- USAGE |>
  filter(year(month) == 2022) |> # Only data from 2022
  filter(Agency %in% agencies_avg_upt$Agency) |> # Filter agencies that meet avg UPT condition
  group_by(`NTD ID`, Agency, metro_area, Mode) |> # Group by relevant columns
  summarize(
    UPT = sum(UPT), # Sum UPT for 2022
    VRM = sum(VRM) # Sum VRM for 2022
  ) |>
  ungroup() |>
  mutate(`NTD ID` = as.double(`NTD ID`)) # Convert NTD ID to double for joining

print(USAGE_2022_ANNUAL) # Output the filtered table

# Need to update the Mode column in Financials to match the updated values done in task 2 for joining tables
FINANCIALS <- FINANCIALS |>
  mutate(Mode = case_when(
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "MB" ~ "Bus",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "VP" ~ "Vanpool",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "LR" ~ "Light Rail",
    Mode == "YR" ~ "Hybrid Rail",
    Mode == "MG" ~ "Monorail and Automated Guideway modes",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "PB" ~ "Publico",
    Mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"
  ))

# merge to financials data frame to create new table that combines both information for 2022
USAGE_AND_FINANCIALS <- inner_join(
  USAGE_2022_ANNUAL,
  FINANCIALS,
  join_by(`NTD ID`, Mode)
)

# Task 6
# You may wish to restrict your answer to major transit systems, which you can define as those with 400,000 UPT per annum.

# 1. Which transit system (agency and mode) had the most UPT in 2022?
most_UPT_2022 <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(most_UPT = UPT, .groups = 'drop') |>
  arrange(desc(most_UPT)) 
print(most_UPT_2022)

# 2. Which transit system (agency and mode) had the highest farebox recovery, defined as the highest ratio of Total Fares to Expenses?
highest_farebox <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_fares = `Total Fares`, total_expenses = Expenses, .groups = 'drop') |>
  mutate(recovery = total_fares / total_expenses) |>
  arrange(desc(recovery))
print(highest_farebox)
# 3. Which transit system (agency and mode) has the lowest expenses per UPT?
low_expense_UPT <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_expenses = Expenses, total_UPT = UPT, .groups = 'drop') |>
  mutate(lowestUPT = total_expenses/total_UPT) |>
  arrange(lowestUPT)
print(low_expense_UPT)
# 4. Which transit system (agency and mode) has the highest total fares per UPT?
high_fare_UPT <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_fares = `Total Fares`, total_UPT = UPT, .groups = 'drop') |>
  mutate(fare_UPT_ratio = total_fares/total_UPT) |>
  arrange(desc(fare_UPT_ratio))
print(high_fare_UPT)
# 5. Which transit system (agency and mode) has the lowest expenses per VRM?
low_expense_VRM <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_expenses = Expenses, total_VRM = VRM, .groups = 'drop') |>
  mutate(lowestVRM = total_expenses/total_VRM) |>
  arrange(lowestVRM)
print(low_expense_VRM)
# 6. Which transit system (agency and mode) has the highest total fares per VRM?
high_fare_VRM <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_fares = `Total Fares`, total_VRM = VRM, .groups = 'drop') |>
  mutate(fare_VRM_ratio = total_fares/total_VRM) |>
  arrange(desc(fare_VRM_ratio))
print(high_fare_VRM)