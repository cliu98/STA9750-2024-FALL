######################################################################
library(dplyr)
library(tidyr)
library(httr2)
library(lubridate)
library(DT)
library(ggplot2)

# Task 1 & 2
# import API keys
alpha_vantage_key <- readLines("AlphaVantageAPIKey.txt", warn = FALSE)[1] # api key located on first line
FRED_key <- readLines("FREDAPIKey.txt", warn = FALSE)[1]

# Task 3
# import data of interest

# AlphaVantage API documentation lets us know exactly what data we can find

# US Equity Market total returns, represented by SPY for our case

# Function to get TIME_SERIES_DAILY data
get_sp500_data <- function(api_key) {
  response <- request("https://www.alphavantage.co/query") |>
    req_url_query(
      `function` = "TIME_SERIES_DAILY", # Backticks for reserved word
      symbol = "SPY", # S&P 500 ETF symbol
      apikey = api_key,
      outputsize = "full", # Full dataset
      datatype = "json" # JSON format
    ) |>
    req_perform()

  # Parse the response as JSON
  data <- resp_body_json(response, simplifyVector = TRUE)
  return(data)
}

sp500_data <- get_sp500_data(alpha_vantage_key)

# Extract the time series data
spy_time_series <- sp500_data$`Time Series (Daily)`

# Convert the nested list to a data frame
spy <- as.data.frame(do.call(rbind, spy_time_series))

# Add the dates as a column
spy <- cbind(date = rownames(spy), spy)

# Reset rownames
rownames(spy) <- NULL

# Rename columns for the regular time series data
colnames(spy) <- c(
  "date",
  "open",
  "high",
  "low",
  "close",
  "volume"
)

# Convert columns to correct types
spy <- transform(spy,
  date = as.Date(date),
  open = as.numeric(open),
  high = as.numeric(high),
  low = as.numeric(low),
  close = as.numeric(close),
  volume = as.numeric(volume)
)

# View the first few rows of the processed data
head(spy)


################################
# International Equity Market total returns
# Function to get TIME_SERIES_DAILY data
get_FDIVX_data <- function(api_key) {
  response <- request("https://www.alphavantage.co/query") |>
    req_url_query(
      `function` = "TIME_SERIES_DAILY", # Backticks for reserved word
      symbol = "FDIVX", # Fidelity International Growth Fund symbol
      apikey = api_key,
      outputsize = "full", # Full dataset
      datatype = "json" # JSON format
    ) |>
    req_perform()

  # Parse the response as JSON
  data <- resp_body_json(response, simplifyVector = TRUE)
  return(data)
}

FDIVX_data <- get_FDIVX_data(alpha_vantage_key)

# Extract the time series data
FDIVX_time_series <- FDIVX_data$`Time Series (Daily)`

# Convert the nested list to a data frame
FDIVX <- as.data.frame(do.call(rbind, FDIVX_time_series))

# Add the dates as a column
FDIVX <- cbind(date = rownames(FDIVX), FDIVX)

# Reset rownames
rownames(FDIVX) <- NULL

# Rename columns for the regular time series data
colnames(FDIVX) <- c(
  "date",
  "open",
  "high",
  "low",
  "close",
  "volume"
)

# Convert numeric columns
FDIVX <- transform(FDIVX,
  date = as.Date(date), # Convert date to Date type
  open = as.numeric(open),
  high = as.numeric(high),
  low = as.numeric(low),
  close = as.numeric(close),
  volume = as.numeric(volume)
)

# View the first few rows of the processed data
head(FDIVX)


####################
# Inflation
get_inflation_data <- function(api_key) {
  response <- request("https://www.alphavantage.co/query") |>
    req_url_query(
      `function` = "CPI", # Backticks for reserved word, CPI is widely regarded as the barometer of inflation levels in the broader economy
      apikey = api_key,
      outputsize = "full", # Full dataset
      datatype = "json" # JSON format
    ) |>
    req_perform()

  # Parse the response as JSON
  data <- resp_body_json(response, simplifyVector = TRUE)
  return(data)
}

inflation_data <- get_inflation_data(alpha_vantage_key)

inflation <- inflation_data$data

colnames(inflation) <- c(
  "date",
  "index_value"
)

inflation <- transform(inflation,
  date = as.Date(date),
  index_value = as.numeric(index_value)
)


#################
# short-term debt returns, using the 2 year us treasury yield

get_short_term_yield_data <- function(api_key) {
  response <- request("https://www.alphavantage.co/query") |>
    req_url_query(
      `function` = "TREASURY_YIELD", # Backticks for reserved word
      maturity = "2year",
      apikey = api_key,
      outputsize = "full", # Full dataset
      datatype = "json" # JSON format
    ) |>
    req_perform()
  
  # Parse the response as JSON
  data <- resp_body_json(response, simplifyVector = TRUE)
  return(data)
}

short_term_yield_data <- get_short_term_yield_data(alpha_vantage_key)

short_term_yield <- short_term_yield_data$data

colnames(short_term_yield) <- c(
  "date",
  "percent_value"
)

short_term_yield <- transform(short_term_yield,
  date = as.Date(date),
  percent_value = as.numeric(percent_value)
)

##################
# FRED API documentation isn't as direct as Alpha Vantage's

# Need to search specifically for data of interest to find what we want. This can be done with the API or on the website itself.

# Wage growth

search_fred_series <- function(search_text, api_key) {
  response <- request("https://api.stlouisfed.org/fred/series/search") |>
    req_url_query(
      api_key = api_key,
      file_type = "json", # Response in JSON format
      search_text = search_text # keyword to search
    ) |>
    req_perform()

  # Parse the JSON response
  data <- resp_body_json(response, simplifyVector = TRUE)

  # Extract and return the relevant series data
  series_data <- data$seriess
  return(series_data)
}

# Search for wage growth-related series using "earnings", tried "wage growth" but the data returned are reflected as a percent change from a year ago in units
wage_series <- search_fred_series("wage growth", FRED_key)

# View the first few results
head(wage_series[, c("id", "title", "frequency", "units")])
# going with FRBATLWGTUMHWGO after looking into it more on FRED site

# function that imports FRED data with series_id
fetch_fred_data <- function(series_id, api_key) {
  response <- request("https://api.stlouisfed.org/fred/series/observations") |>
    req_url_query(
      series_id = series_id, # Use the provided series_id
      api_key = api_key, # Use the provided API key
      file_type = "json" # Desired file type (json in this case)
    ) |>
    req_perform()

  # Parse the JSON response
  data <- response |>
    resp_body_json()

  # Extract the observations data from the response
  observations <- data$observations

  # Convert the data into a data frame for easier manipulation
  df <- data.frame(
    date = sapply(observations, function(x) x$date),
    value = sapply(observations, function(x) as.numeric(x$value))
  )

  # Return the data frame
  return(df)
}

# Call the function with a specific series_id LES1252881600Q: Median weekly earnings quarterly
wage_growth <- fetch_fred_data("FRBATLWGTUMHWGO", FRED_key)

colnames(wage_growth) <- c(
  "date",
  "wage_percent_change_from_a_year_ago"
)

wage_growth <- transform(wage_growth,
  date = as.Date(date)
)

########################
# bond market total returns, will use GS10 as it is a good indicator in the bond market overall

bond_returns <- fetch_fred_data("GS10", FRED_key)

colnames(bond_returns) <- c(
  "date",
  "percent_value"
)

bond_returns <- transform(bond_returns,
  date = as.Date(date)
)

# lets bring these together all into one data frame, monthly should be fine since we're doing a long run analysis over 20 years and will bootstrap
# defining 20 year range to be 10/1/2004 to 10/1/2024

# adjust SPY to monthly and return per share
spy_monthly <- spy |>
  filter(date > as.Date("2004-09-30") & date < as.Date("2024-10-02")) |>
  group_by(date = floor_date(date, "month")) |>
  summarise(spy_average_close = round(mean(close), 2), .groups = 'drop') |>
  mutate(US_Equity_Total_Returns_USD = 100 * (spy_average_close - lag(spy_average_close)) / lag(spy_average_close)) |>
  mutate(US_Equity_Total_Returns_USD = round(ifelse(is.na(US_Equity_Total_Returns_USD), 0, US_Equity_Total_Returns_USD), 2)) # Rounding after replacing NA
head(spy_monthly)

# adjust FDIVX to monthly and return per share
FDIVX_monthly <- FDIVX |>
  filter(date > as.Date("2004-09-30") & date < as.Date("2024-10-02")) |>
  group_by(date = floor_date(date, "month")) |>
  summarise(FDIVX_average_close = round(mean(close), 2), .groups = 'drop') |>
  mutate(International_Equity_Total_Returns_USD = 100 * (FDIVX_average_close - lag(FDIVX_average_close)) / lag(FDIVX_average_close)) |>
  mutate(International_Equity_Total_Returns_USD = round(ifelse(is.na(International_Equity_Total_Returns_USD), 0, International_Equity_Total_Returns_USD), 2)) # Rounding after replacing NA
head(FDIVX_monthly)

# adjust inflation to join, should in theory be an average since its a monthly value & convert to a percentage
inflation_monthly <- inflation |>
  filter(date > as.Date("2004-09-30") & date < as.Date("2024-10-02")) |>
  rename(inflation_index_value = index_value) |>
  mutate(inflation_percent_change = round((inflation_index_value / lag(inflation_index_value) - 1) * 100, 2))

# adjust short term yield to join, should in theory be an average since its a monthly value
short_term_yield_monthly <- short_term_yield |>
  filter(date > as.Date("2004-09-30") & date < as.Date("2024-10-02")) |>
  rename(short_term_yield_percent_value = percent_value)

# adjust wage growth 
wage_growth_monthly <- wage_growth |>
  filter(date > as.Date("2004-09-30") & date < as.Date("2024-10-02"))

# adjust bond returns, should be an average in theory
bond_returns_monthly <- bond_returns |>
  filter(date > as.Date("2004-09-30") & date < as.Date("2024-10-02")) |>
  rename(bond_return_percent_value = percent_value)

# join all into one df
historical_financial_series <- spy_monthly |>
  left_join(FDIVX_monthly, by = "date") |>
  left_join(inflation_monthly, by = "date") |>
  left_join(short_term_yield_monthly, by = "date") |>
  left_join(wage_growth_monthly, by = "date") |>
  left_join(bond_returns_monthly, by = "date") |>
  mutate(spy_return_percent = round(US_Equity_Total_Returns_USD / lag(spy_average_close),4),
         spy_return_percent = replace(spy_return_percent, is.na(spy_return_percent), 0)) |>
  mutate(FDIVX_return_percent = round(International_Equity_Total_Returns_USD / lag(FDIVX_average_close),4),
         FDIVX_return_percent = replace(FDIVX_return_percent, is.na(FDIVX_return_percent), 0))
head(historical_financial_series)

sample_n(historical_financial_series, 10) |> 
  rename(
    Date = date,
    `SPY Value USD` = spy_average_close,
    `US Equity Return USD Per Share` = US_Equity_Total_Returns_USD,
    `US Equity Return % Per Share` = spy_return_percent,
    `FDIVX Value USD` = FDIVX_average_close,
    `International Equity Return USD Per Share` = International_Equity_Total_Returns_USD,
    `International Equity Return % Per Share` = FDIVX_return_percent,
    `Inflation Index Value` = inflation_index_value,
    `Inflation Change %` = inflation_percent_change,
    `Short Term Yield Return %` = short_term_yield_percent_value,
    `Wage Growth % Change From Last Year` = wage_percent_change_from_a_year_ago,
    `Bond Return %` = bond_return_percent_value
  ) |> 
  DT::datatable(caption = "Sample Overview of Historical Financial Data")

# task 4 

# average grouped by months
long_run_monthly_averages <- historical_financial_series |>
  summarize(
    avg_us_equity_return = 100*round(mean(spy_return_percent, na.rm = TRUE),4),
    avg_international_equity_return = 100*round(mean(FDIVX_return_percent, na.rm = TRUE) ,4),
    avg_bond_return = round(mean(bond_return_percent_value),2), # a percentage
    avg_short_term_yield_return = round(mean(short_term_yield_percent_value),2), # a percentage
    avg_inflation_change = round(abs(mean(inflation_percent_change, na.rm = TRUE)),2), # a percentage
    avg_median_wage_change = round(mean(wage_percent_change_from_a_year_ago, na.rm = TRUE),2) # a percentage
  )

# creating new df for renaming purposes for data table display only, keeping original since it'll be easier to type the variables out later for other analysis
long_run_monthly_averages_dt <- long_run_monthly_averages |>
  rename(
    `Average US Equity Return (%)` = avg_us_equity_return,
    `Average International Equity Return (%)` = avg_international_equity_return,
    `Average Bond Return (%)` = avg_bond_return,
    `Average Short Term Yield Return (%)` = avg_short_term_yield_return,
    `Average Inflation Change (%)` = avg_inflation_change,
    `Average Median Wage Change From a Year Ago(%)` = avg_median_wage_change,
  )

datatable(long_run_monthly_averages_dt, caption = "Long Run Monthly Averages")
  

# correlations
correlation_data <- historical_financial_series |>
  select(-date, -inflation_percent_change, -US_Equity_Total_Returns_USD, -International_Equity_Total_Returns_USD, -spy_return_percent, -FDIVX_return_percent) |>
  rename(`SPY Share Value` = spy_average_close,
         `FDIVX Share Value` = FDIVX_average_close,
         `Inflation Index` = inflation_index_value,
         `Short Term Yield` = short_term_yield_percent_value,
         `Wage Growth` = wage_percent_change_from_a_year_ago,
         `Bond Return` = bond_return_percent_value)

correlation_matrix <- round(cor(correlation_data),2)

# convert matrix to a long-format data frame for plotting
correlation_df <- as.data.frame(as.table(correlation_matrix))

# heatmap with ggplot2
ggplot(correlation_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +  # Tile borders
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  labs(x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# data table setup instead
datatable(correlation_matrix,
          caption = "Correlation Matrix Between Data Series") |>
  formatStyle(
    'SPY Share Value',  
    backgroundColor = styleInterval(c(0.75), c('white', 'lightgreen')), # highlighting values above 0.75 to represent strong correlation
    color = styleInterval(c(0.75), c('black', 'black'))
  ) |>
  formatStyle(
    'FDIVX Share Value',  
    backgroundColor = styleInterval(c(0.75), c('white', 'lightgreen')),
    color = styleInterval(c(0.75), c('black', 'black'))
  ) |>
  formatStyle(
    'Inflation Index',  
    backgroundColor = styleInterval(c(0.75), c('white', 'lightgreen')),
    color = styleInterval(c(0.75), c('black', 'black'))
  ) |>
  formatStyle(
    'Short Term Yield',  
    backgroundColor = styleInterval(c(0.75), c('white', 'lightgreen')),
    color = styleInterval(c(0.75), c('black', 'black'))
  ) |>
  formatStyle(
    'Wage Growth',  
    backgroundColor = styleInterval(c(0.75), c('white', 'lightgreen')),
    color = styleInterval(c(0.75), c('black', 'black'))
  ) |>
  formatStyle(
    'Bond Return',  
    backgroundColor = styleInterval(c(0.75), c('white', 'lightgreen')),
    color = styleInterval(c(0.75), c('black', 'black'))
  )

# variances
# probably makes sense to conduct variance analysis on percentages/returns only
`US Equity Total Returns Variance` <- round(var(historical_financial_series$US_Equity_Total_Returns_USD),2)
`International Equity Total Returns Variance` <- round(var(historical_financial_series$International_Equity_Total_Returns_USD),2)
`Inflation Variance` <- round(var(historical_financial_series$inflation_percent_change, na.rm = TRUE),2)
`Wage Growth Variance` <- round(var(historical_financial_series$wage_percent_change_from_a_year_ago, na.rm = TRUE),2)
`Short Term Yield Variance` <- round(var(historical_financial_series$short_term_yield_percent_value),2)
`Bond Return Variance` <- round(var(historical_financial_series$bond_return_percent_value),2)

series_names <- c("US Equity Total Returns Variance", 
                    "International Equity Total Returns Variance", 
                    "Inflation Variance", 
                    "Wage Growth Variance", 
                    "Short Term Yield Variance", 
                    "Bond Return Variance")

# combine into a data frame
variance_data <- data.frame(Series = series_names,
                            Variance = c(
                              `US Equity Total Returns Variance`,
                              `International Equity Total Returns Variance`,
                              `Inflation Variance`,
                              `Wage Growth Variance`,
                              `Short Term Yield Variance`,
                              `Bond Return Variance`
                            ))

datatable(variance_data, caption = "Data Series Variance")

# Task 5: Historical Comparison

# set starting salary
starting_salary <- 60000

# define tenure of job before retirement (start of data to end of data)
years_worked <- floor(as.numeric(difftime(max(historical_financial_series$date), min(historical_financial_series$date)))/365)

# define salary earned in entire tenure, based on the long run monthly average of wage growth of 3.51%
salary_growth_rate <- long_run_monthly_averages$avg_median_wage_change/100
tenured_salary <- starting_salary * (1 + salary_growth_rate)^(0:years_worked)


# TRS Plan
# assuming the employee gets the full benefit of (35% + 2%*N) * FAS

# define FAS as the average of the last 3 years of salary before retirement
FAS = mean(tail(tenured_salary,3))

# the annual retirement benefit
annual_retirement_benefit = (0.35 + 0.02*years_worked)*FAS

# inflation adjustment is made to the annual retirement benefit annually, this will be taken into consideration later in the analysis

# first month's retirement income
TRS_first_month_value = round(annual_retirement_benefit/12,0) # round to the nearest dollar
print(paste("The estimated benefit during the first month of retirement with the TRS plan is $", TRS_first_month_value))

# OPR Plan
# making assumptions here, lets say the employee is hired at age 42 and retires at 62

# initialize the retirement data frame
retirement_account <- data.frame(
  period = 1:nrow(historical_financial_series), # of months contributions are made  
  
  # initialize columns for each asset return
  allocation_us = rep(0, nrow(historical_financial_series)), 
  allocation_international = rep(0, nrow(historical_financial_series)),
  allocation_bonds = rep(0, nrow(historical_financial_series)),
  allocation_short_term = rep(0, nrow(historical_financial_series)),
  
  # define the assets returns based on historical data, dividing to get decimal value
  us_returns = historical_financial_series$spy_return_percent/100, 
  international_returns = historical_financial_series$FDIVX_return_percent/100,
  bonds_returns = historical_financial_series$bond_return_percent_value/100,
  short_term_returns = historical_financial_series$short_term_yield_percent_value/100,
  
  # initialize columns for contributions made
  contribution_employee = rep(0, nrow(historical_financial_series)), 
  contribution_employer = rep(0, nrow(historical_financial_series))
)

# populate contributions and allocations as before
for (month in 1:nrow(retirement_account)) {
  year <- ceiling(month / 12) # round to the closest year to define number of years worked
  age <- 42 + (year - 1) # assuming employee starts at age 42
  annual_salary <- tenured_salary[year] # grab the salary dependent on work year

  # calculate contributions based on salary bracket
  if (annual_salary <= 45000) {
    retirement_account$contribution_employee[month] <- annual_salary * 0.03 / 12
  } else if (annual_salary <= 55000) {
    retirement_account$contribution_employee[month] <- annual_salary * 0.035 / 12
  } else if (annual_salary <= 75000) {
    retirement_account$contribution_employee[month] <- annual_salary * 0.045 / 12
  } else if (annual_salary <= 100000) {
    retirement_account$contribution_employee[month] <- annual_salary * 0.0575 / 12
  } else {
    retirement_account$contribution_employee[month] <- annual_salary * 0.06 / 12
  }
  retirement_account$contribution_employer[month] <- if (year <= 7) annual_salary * 0.08 / 12 else annual_salary * 0.10 / 12

  # define allocations based on age
  if (age <= 49) {
    retirement_account$allocation_us[month] <- 0.54
    retirement_account$allocation_international[month] <- 0.36
    retirement_account$allocation_bonds[month] <- 0.10
    retirement_account$allocation_short_term[month] <- 0.00
  } else if (age <= 59) {
    retirement_account$allocation_us[month] <- 0.47
    retirement_account$allocation_international[month] <- 0.32
    retirement_account$allocation_bonds[month] <- 0.21
    retirement_account$allocation_short_term[month] <- 0.00
  } else if (age <= 74) {
    retirement_account$allocation_us[month] <- 0.34
    retirement_account$allocation_international[month] <- 0.23
    retirement_account$allocation_bonds[month] <- 0.43
    retirement_account$allocation_short_term[month] <- 0.00
  } else {
    retirement_account$allocation_us[month] <- 0.19
    retirement_account$allocation_international[month] <- 0.13
    retirement_account$allocation_bonds[month] <- 0.62
    retirement_account$allocation_short_term[month] <- 0.06
  }
}

# calculate future value using compound interest
retirement_account_value <- retirement_account |>
  mutate(
    # total contribution per month
    contribution_total = contribution_employee + contribution_employer,

    # net total return factors for each asset with compounding
    net_return_us = order_by(desc(period), cumprod(1 + lead(us_returns, default = 0))),
    net_return_international = order_by(desc(period), cumprod(1 + lead(international_returns, default = 0))),
    net_return_bonds = order_by(desc(period), cumprod(1 + lead(bonds_returns, default = 0))),
    net_return_short_term = order_by(desc(period), cumprod(1 + lead(short_term_returns, default = 0))),

    # future value of contributions per asset class
    fv_us = contribution_total * allocation_us * net_return_us,
    fv_international = contribution_total * allocation_international * net_return_international,
    fv_bonds = contribution_total * allocation_bonds * net_return_bonds,
    fv_short_term = contribution_total * allocation_short_term * net_return_short_term
  ) |>
  summarize(
    account_value = sum(fv_us + fv_international + fv_bonds + fv_short_term) # total future value of the retirement account
  ) |>
  pull(account_value)

print(paste("The estimated retirement account value at the first month of retirement with the OPR plan is $", round(retirement_account_value,0)))


# Task 6: Fixed-Rate Analysis
# define the life expectancy to be 80 years old

# TRS inflation adjusted for the next 18 years
# The benefit is increased annually by 50% of the CPI, rounded up to the nearest tenth of a percent
CPI_benefit = round((long_run_monthly_averages$avg_inflation_change*12)/2,1)/100
annual_retirement_benefit_inflation_adjusted = round(annual_retirement_benefit * (1 + CPI_benefit)^(0:17), 0) # (0:17) creates a vector for the next 18 years of inflation adjusted income

# monthly retirement income in each year
monthly_annual_retirement_benefit_inflation_adjusted = round(annual_retirement_benefit_inflation_adjusted/12,0)

# average monthly income post retirement
TRS_avg_monthly_income = round(mean(monthly_annual_retirement_benefit_inflation_adjusted),0)

# convert the numeric vector to a data frame with ages
annual_retirement_benefit_inflation_adjusted <- data.frame(
  age = 63:80, # Create a column for ages from 63 to 80
  annual_benefit = annual_retirement_benefit_inflation_adjusted,
  monthly_benefit = round(annual_retirement_benefit_inflation_adjusted/12,0)
)

# new variable for display purposes only
annual_retirement_benefit_inflation_adjusted_dt <- annual_retirement_benefit_inflation_adjusted |>
  rename(
    Age = age,
    `Annual Benefit (USD)` = annual_benefit,
    `Monthly Benefit (USD)` = monthly_benefit
  )

datatable(annual_retirement_benefit_inflation_adjusted_dt, caption = "TRS Benefit During Retirement")

# OPR for the next 18 years

# Initial account value at retirement
post_retirement_account_value <- retirement_account_value

# Define the withdrawal rate (4%)
withdrawal_rate <- 0.04

# Initialize an empty data frame to store results
post_retirement_account_df <- data.frame(age = integer(), account_value = numeric())

# Initialize previous_account_value to store the starting value
previous_account_value <- post_retirement_account_value

# Create a loop for each year of retirement (from age 62 to 80)
for (age in 62:80) {
  
  # Calculate the annual withdrawal based on the previous year's account value
  annual_withdrawal <- previous_account_value * withdrawal_rate
  
  # Subtract the withdrawal before applying growth
  post_retirement_account_value <- previous_account_value - annual_withdrawal
  
  # Define allocations based on age
  if (age <= 74) {
    post_allocation_us <- 0.34
    post_allocation_international <- 0.23
    post_allocation_bonds <- 0.43
    post_allocation_short_term <- 0.00
  } else {
    post_allocation_us <- 0.19
    post_allocation_international <- 0.13
    post_allocation_bonds <- 0.62
    post_allocation_short_term <- 0.06
  }
  
  # Define the asset returns (adjusted for decimal values)
  us_returns <- historical_financial_series$spy_return_percent / 100
  international_returns <- historical_financial_series$FDIVX_return_percent / 100
  bonds_returns <- historical_financial_series$bond_return_percent_value / 100
  short_term_returns <- historical_financial_series$short_term_yield_percent_value / 100
  
  # Adjust for monthly growth using returns (assuming monthly data for growth)
  post_net_return_us <- cumprod(1 + lead(us_returns, default = 0))  
  post_net_return_international <- cumprod(1 + lead(international_returns, default = 0))  
  post_net_return_bonds <- cumprod(1 + lead(bonds_returns, default = 0))  
  post_net_return_short_term <- cumprod(1 + lead(short_term_returns, default = 0))  
  
  # Calculate the future value per asset class for the current year
  post_fv_us <- post_retirement_account_value * post_allocation_us * post_net_return_us
  post_fv_international <- post_retirement_account_value * post_allocation_international * post_net_return_international
  post_fv_bonds <- post_retirement_account_value * post_allocation_bonds * post_net_return_bonds
  post_fv_short_term <- post_retirement_account_value * post_allocation_short_term * post_net_return_short_term
  
  # Calculate the total value after growth
  post_retirement_account_value <- post_fv_us + post_fv_international + post_fv_bonds + post_fv_short_term
  
  # Store the account value for the current year
  post_retirement_account_df <- rbind(post_retirement_account_df, data.frame(age = age, account_value = round(post_retirement_account_value,0)))
  
  # Update previous_account_value for the next year
  previous_account_value <- post_retirement_account_value
}

# add columns for annual and monthly incomes at each age
post_retirement_account_df <- post_retirement_account_df |>
  mutate(annual_income = round(account_value*0.04,0),
         monthly_income = round(annual_income/12,0))

# average monthly income post retirement
OPR_avg_monthly_income = round(mean(post_retirement_account_df$monthly_income),0)

# adjusted for display purposes only
post_retirement_account_df_table <- post_retirement_account_df |>
  filter(age != 62) |>
  rename(
    Age = age,
    `Retirement Account Value (USD)` = account_value,
    `Annual Withdrawal (USD)` = annual_income,
    `Monthly Income (USD)` = monthly_income
  )

datatable(post_retirement_account_df_table, caption = "ORP Income During Retirement")

# plot the comparison
plan_comparison <- annual_retirement_benefit_inflation_adjusted |>
  left_join(
    post_retirement_account_df,
    join_by(age == age)
  ) |>
  select(age, monthly_benefit, monthly_income)

ggplot(plan_comparison, aes(x = age)) +
  geom_line(aes(y = monthly_benefit, color = "TRS Plan")) +
  geom_line(aes(y = monthly_income, color = "ORP Plan")) +
  labs(title = "Comparison of TRS & ORP Plans by Age",
       x = "Age",
       y = "Monthly Income",
       color = "Retirement Plan") +
  scale_color_manual(values = c("blue", "red")) +  # Customize colors
  theme_minimal()

# are there funds left after death?
ORP_funds <- post_retirement_account_df_table |>
  filter(Age == 80) |>
  select(Age, `Retirement Account Value (USD)`)

datatable(ORP_funds, caption = "Funds Left at Death")

# average income per month each plan
print(paste("The average monthly income with the TRS plan is", TRS_avg_monthly_income, "dollars."))
print(paste("The average monthly income with the ORP plan is", OPR_avg_monthly_income, "dollars."))

# Maximum and minimum gap in monthly income between TRS and ORP
income_gap <- annual_retirement_benefit_inflation_adjusted |>
  left_join(
    post_retirement_account_df,
    join_by(age == age)
  ) |>
  mutate(income_difference = monthly_benefit - monthly_income) |>
  select(age, income_difference) |>
  filter(income_difference == max(income_difference) | income_difference == min(income_difference))

# extract the two ages for plotting use
ages_to_display <- unique(income_gap$age)

# display the two specific ages on the plot
income_gap <- income_gap |>
  mutate(age = factor(age, levels = ages_to_display))

ggplot(income_gap, aes(x = age, y = income_difference)) + 
  geom_bar(stat = "identity", fill = "cadetblue2" ) +
  labs(title = "Maximum & Minimum Income Difference by Age", x = "Age", y = "Income Difference") +
  theme_minimal()

# Task 7: Monte Carlo Analysis

# Calculate the probability of savings exhaustion

# initialize parameters
set.seed(123)
n_bootstrap <- 200
savings_exhausted <- numeric(n_bootstrap) # set vector for any bootstrap accounts exhausted
post_retirement_results <- vector("list", n_bootstrap) # combine all iterations into one list

# Main loop for bootstrap samples
for (i in 1:n_bootstrap) {
  # Bootstrap sample from historical financial series
  bootstrap_sample <- historical_financial_series |> 
    slice_sample(n = nrow(historical_financial_series), replace = TRUE)
  
  # Initialize retirement data frame
  retirement_account <- data.frame(
    period = 1:nrow(bootstrap_sample),
    allocation_us = rep(0, nrow(bootstrap_sample)),
    allocation_international = rep(0, nrow(bootstrap_sample)),
    allocation_bonds = rep(0, nrow(bootstrap_sample)),
    allocation_short_term = rep(0, nrow(bootstrap_sample)),
    us_returns = bootstrap_sample$spy_return_percent / 100,
    international_returns = bootstrap_sample$FDIVX_return_percent / 100,
    bonds_returns = bootstrap_sample$bond_return_percent_value / 100,
    short_term_returns = bootstrap_sample$short_term_yield_percent_value / 100,
    contribution_employee = rep(0, nrow(bootstrap_sample)),
    contribution_employer = rep(0, nrow(bootstrap_sample))
  )
  
  
  bootstrap_salary_growth_rate <- mean(bootstrap_sample$wage_percent_change_from_a_year_ago/100)
  bootstrap_tenured_salary <- starting_salary * (1 + bootstrap_salary_growth_rate)^(0:years_worked)
  
  # Populate contributions and allocations based on age and salary
  for (month in 1:nrow(retirement_account)) {
    year <- ceiling(month / 12)
    age <- 42 + (year - 1)
    annual_salary <- bootstrap_tenured_salary[year]
    
    # calculate contributions based on salary bracket
    if (annual_salary <= 45000) {
      retirement_account$contribution_employee[month] <- annual_salary * 0.03 / 12
    } else if (annual_salary <= 55000) {
      retirement_account$contribution_employee[month] <- annual_salary * 0.035 / 12
    } else if (annual_salary <= 75000) {
      retirement_account$contribution_employee[month] <- annual_salary * 0.045 / 12
    } else if (annual_salary <= 100000) {
      retirement_account$contribution_employee[month] <- annual_salary * 0.0575 / 12
    } else {
      retirement_account$contribution_employee[month] <- annual_salary * 0.06 / 12
    }
    retirement_account$contribution_employer[month] <- if (year <= 7) annual_salary * 0.08 / 12 else annual_salary * 0.10 / 12
    
    # Allocations based on age
    if (age <= 49) {
      retirement_account$allocation_us[month] <- 0.54
      retirement_account$allocation_international[month] <- 0.36
      retirement_account$allocation_bonds[month] <- 0.10
      retirement_account$allocation_short_term[month] <- 0.00
    } else if (age <= 59) {
      retirement_account$allocation_us[month] <- 0.47
      retirement_account$allocation_international[month] <- 0.32
      retirement_account$allocation_bonds[month] <- 0.21
      retirement_account$allocation_short_term[month] <- 0.00
    } else if (age <= 74) {
      retirement_account$allocation_us[month] <- 0.34
      retirement_account$allocation_international[month] <- 0.23
      retirement_account$allocation_bonds[month] <- 0.43
      retirement_account$allocation_short_term[month] <- 0.00
    } else {
      retirement_account$allocation_us[month] <- 0.19
      retirement_account$allocation_international[month] <- 0.13
      retirement_account$allocation_bonds[month] <- 0.62
      retirement_account$allocation_short_term[month] <- 0.06
    }
  }
  
  # calculate future value using compound interest
  retirement_account_value <- retirement_account |>
    mutate(
      # total contribution per month
      contribution_total = contribution_employee + contribution_employer,
      
      # net total return factors for each asset with compounding
      net_return_us = order_by(desc(period), cumprod(1 + lead(us_returns, default = 0))),
      net_return_international = order_by(desc(period), cumprod(1 + lead(international_returns, default = 0))),
      net_return_bonds = order_by(desc(period), cumprod(1 + lead(bonds_returns, default = 0))),
      net_return_short_term = order_by(desc(period), cumprod(1 + lead(short_term_returns, default = 0))),
      
      # future value of contributions per asset class
      fv_us = contribution_total * allocation_us * net_return_us,
      fv_international = contribution_total * allocation_international * net_return_international,
      fv_bonds = contribution_total * allocation_bonds * net_return_bonds,
      fv_short_term = contribution_total * allocation_short_term * net_return_short_term
    ) |>
    summarize(
      account_value = sum(fv_us + fv_international + fv_bonds + fv_short_term) # total future value of the retirement account
    ) |>
    pull(account_value)
  
  # Post-retirement calculations for ages 62 to 80
  post_retirement_account_df <- data.frame(age = integer(), account_value = numeric())
  withdrawal_rate <- 0.07
  previous_account_value <- retirement_account_value
  
  # Loop through ages 62 to 80, applying withdrawals and growth
  for (age in 62:80) {
    # Calculate the withdrawal amount based on withdrawal rate
    computed_withdrawal <- previous_account_value * withdrawal_rate
    
    # Set the annual withdrawal to at least $60,000 for minimum living expenses
    annual_withdrawal <- max(computed_withdrawal, 60000)
    
    # Deduct withdrawal from account
    post_retirement_account_value <- previous_account_value - annual_withdrawal
    
    # Define allocations based on age
    if (age <= 74) {
      post_allocation_us <- 0.34
      post_allocation_international <- 0.23
      post_allocation_bonds <- 0.43
      post_allocation_short_term <- 0.00
    } else {
      post_allocation_us <- 0.19
      post_allocation_international <- 0.13
      post_allocation_bonds <- 0.62
      post_allocation_short_term <- 0.06
    }
    
    # Calculate future value of the remaining balance after applying returns for the year
    # Apply returns based on age (mapping current year returns for the remaining account value)
    post_fv_us <- post_retirement_account_value * post_allocation_us * (1 + retirement_account$us_returns[age - 61])
    post_fv_international <- post_retirement_account_value * post_allocation_international * (1 + retirement_account$international_returns[age - 61])
    post_fv_bonds <- post_retirement_account_value * post_allocation_bonds * (1 + retirement_account$bonds_returns[age - 61])
    post_fv_short_term <- post_retirement_account_value * post_allocation_short_term * (1 + retirement_account$short_term_returns[age - 61])
    
    # Total value after applying returns to the remaining balance
    post_retirement_account_value <- post_fv_us + post_fv_international + post_fv_bonds + post_fv_short_term
    
    # Store the account value at the end of each year
    post_retirement_account_df <- rbind(post_retirement_account_df, data.frame(age = age, account_value = round(post_retirement_account_value, 0)))
    
    # Update the previous account value for the next loop iteration
    previous_account_value <- post_retirement_account_value
  }
  
  # Add income columns for post-retirement
  post_retirement_account_df <- post_retirement_account_df |> 
    mutate(annual_income = round(account_value * withdrawal_rate, 0),
           monthly_income = round(annual_income / 12, 0))
  
  # Store the results
  post_retirement_results[[i]] <- post_retirement_account_df
  
  # Check if any savings were exhausted
  if (any(post_retirement_account_df$account_value <= 0)) {
    savings_exhausted[i] <- 1
  }
}

print(paste("The probability of exhausting the ORP plan before death is", mean(savings_exhausted),"."))

ORP_bootstrap_example <- post_retirement_results[[1]] |> 
  rename(
    Age = age,
    `Account Value (USD)` = account_value,
    `Annual Withdrawal (USD)` = annual_income,
    `Monthly Income (USD)` = monthly_income
  )

datatable(ORP_bootstrap_example, caption = "Sample Bootstrapped ORP Output")

########################################## working on this
# probability that an ORP employee has a higher monthly income in retirement than a TRS employee


# get TRS dataframe list from bootstrap history first to compare the ORP already calculated 
set.seed(123)
n_bootstrap <- 200

# Initialize an empty list to store the results
bootstrap_TRS_results <- vector("list", n_bootstrap)

for (i in 1:n_bootstrap) {
  # Bootstrap sample from historical financial series
  bootstrap_sample <- historical_financial_series |> 
    slice_sample(n = nrow(historical_financial_series), replace = TRUE)
  
  # Calculate growth rate and tenured salary
  bootstrap_salary_growth_rate <- mean(bootstrap_sample$wage_percent_change_from_a_year_ago) / 100
  bootstrap_tenured_salary <- starting_salary * (1 + bootstrap_salary_growth_rate)^(0:years_worked)
  
  # Calculate the Final Average Salary (FAS)
  bootstrap_FAS <- mean(tail(bootstrap_tenured_salary, 3))
  
  # Calculate the annual retirement benefit
  bootstrap_annual_retirement_benefit <- (0.35 + 0.02 * years_worked) * bootstrap_FAS
  
  # Calculate CPI benefit
  bootstrap_CPI_benefit <- abs(mean(bootstrap_sample$inflation_percent_change, na.rm = TRUE)) * 12 /100
  bootstrap_annual_retirement_benefit_inflation_adjusted <- round(bootstrap_annual_retirement_benefit * (1 + bootstrap_CPI_benefit)^(0:17), 0) # (0:17) creates a vector for the next 18 years of inflation adjusted income
  
  # Convert the numeric vector to a data frame with ages
  bootstrap_annual_retirement_benefit_inflation_adjusted_df <- data.frame(
    age = 63:80, # Create a column for ages from 63 to 80
    annual_benefit = bootstrap_annual_retirement_benefit_inflation_adjusted,
    monthly_benefit = round(bootstrap_annual_retirement_benefit_inflation_adjusted / 12, 0)
  )
  
  # Store the data frame in the list
  bootstrap_TRS_results[[i]] <- bootstrap_annual_retirement_benefit_inflation_adjusted_df
}


# mean monthly income of each TRS bootstrap history dataframe
mean_TRS_monthly_benefits <- sapply(bootstrap_TRS_results, function(df) mean(df$monthly_benefit, na.rm = TRUE))

# mean monthly income of each ORP bootstrap history dataframe
mean_ORP_monthly_income <- sapply(post_retirement_results, function(df) mean(df$monthly_income, na.rm = TRUE))

# compare the two plans
# Calculate the number of times ORP monthly income is greater than TRS monthly benefits
greater_than_count <- sum(mean_ORP_monthly_income > mean_TRS_monthly_benefits)

# Calculate the total number of comparisons (which should be the same length for both vectors)
total_comparisons <- length(mean_TRS_monthly_benefits)

# Calculate the probability
probability_ORP_greater <- (greater_than_count / total_comparisons)*100

# Print the results
cat("The probability that the ORP plan has a greater monthly income than TRS is:", probability_ORP_greater, "%.")


# jitter plot
average_monthly_plan_incomes <- data.frame(
  Value = c(mean_TRS_monthly_benefits, mean_ORP_monthly_income),
  Group = c(rep("TRS", length(mean_TRS_monthly_benefits)), rep("ORP", length(mean_ORP_monthly_income)))
)

ggplot(average_monthly_plan_incomes, aes(x = Group, y = Value)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  labs(title = "Jitter Plot of TRS vs. ORP Plan Results", x = "Plan Type", y = "Average Monthly Income") +
  theme_minimal()

# plot distribution of monthly income

ggplot(data.frame(Income = mean_ORP_monthly_income), aes(x = Income)) +
  geom_histogram(binwidth = 100, fill = "darkseagreen3", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Average Monthly Income (ORP)",
       subtitle = "Generated From 200 Bootstrap Histories",
       x = "Average Monthly Income (USD)",
       y = "Frequency") +
  theme_minimal()

ggplot(data.frame(Income = mean_TRS_monthly_benefits), aes(x = Income)) +
  geom_histogram(binwidth = 100, fill = "palevioletred", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Average Monthly Income (TRS)",
       subtitle = "Generated From 200 Bootstrap Histories",
       x = "Average Monthly Income (USD)",
       y = "Frequency") +
  theme_minimal()


# look at other withdrawal rates

