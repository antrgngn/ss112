library(dplyr)
library(tidyr)
library(readr)

# STEP 1: Load your World Bank data
# (Assuming you exported to CSV from Excel)
raw_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJ2e_onrJcvj7ySfQv7ZZCsKT_SkrAn6GGGmEb9jnmy02S3IThLCkwaSt8ofyusoVRC9WoX-dpQVzd/pub?gid=608784404&single=true&output=csv")
raw_data

# STEP 2: Define exact indicators you want to include
selected_vars <- c(
  "GDP per capita (current US$)",
  "Inflation, GDP deflator (annual %)",
  "Poverty headcount ratio at national poverty lines (% of population)",
  "Income share held by lowest 20%",
  "School enrollment, secondary (% gross)"
)

# STEP 3: Filter to selected variables
filtered_data <- raw_data %>%
  filter(`Series Name` %in% selected_vars)

# STEP 4: Convert to long format, clean column types
long_data_by_year <- filtered_data %>%
  pivot_longer(
    cols = starts_with("20"),  # all year columns
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(substr(Year, 1, 4)),
    Value = as.numeric(Value),  # convert "..", blanks, etc. to NA
    Variable = `Series Name`,
    Country = `Country Name`
  ) %>%
  select(Country, Year, Variable, Value)

# STEP 5: Optional — filter to years of interest (e.g., 2016–2023)
long_data_by_year <- long_data_by_year %>%
  filter(Year >= 2016, Year <= 2023)

covariate_wide <- long_data_by_year %>%
  pivot_wider(names_from = Variable, values_from = Value)

covariate_wide <- covariate_wide %>%
  rename_with(~ case_when(
    . == "GDP per capita (current US$)" ~ "gdp_current",
    . == "Inflation, GDP deflator (annual %)" ~ "inflation",
    . == "Poverty headcount ratio at national poverty lines (% of population)" ~ "poverty_rate",
    . == "Income share held by lowest 20%" ~ "income_share_bottom20",
    . == "School enrollment, secondary (% gross)" ~ "secondary_enrollment",
    TRUE ~ make.names(., unique = TRUE)  # fallback for any extra columns
  ))

# View the resulting disaggregated covariate data
head(covariate_wide)

library(Synth)
library(dplyr)
library(ggplot2)
library(lubridate)

# Use your covariate_df from the earlier cleaned data
covariate_data <- covariate_df

outcome_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJ2e_onrJcvj7ySfQv7ZZCsKT_SkrAn6GGGmEb9jnmy02S3IThLCkwaSt8ofyusoVRC9WoX-dpQVzd/pub?gid=204188519&single=true&output=csv")
outcome_data$Event.month

# Read in monthly outcome data 
outcome_data <- outcome_data %>%
  rename(
    `Event_month` = Event.month,
    `Protest_Events` = Events
  ) 

outcome_data <- outcome_data %>% 
  mutate(
    Event_month = ymd(Event_month),
    Year        = year(Event_month),
    Month       = month(Event_month),
    time        = Year + (Month - 1)/12
  ) %>%
  select(Country, Year, Month, time, Protest_Events)

tail(outcome_data)
  

# Check time values
print("Sample time values:")
print(head(outcome_data %>% select(Country, Event_month, Year, Month, time)))

# Merge monthly outcome with annual covariates
panel_data <- outcome_data %>%
  inner_join(covariate_data, by = c("Country"))

# Check if there are missing values after merging
missing_check <- panel_data %>%
  summarise(across(everything(), ~sum(is.na(.))))
print("Missing values after merge:")
print(missing_check)

# Create numeric ID for each country
unit_ids <- data.frame(
  Country  = unique(panel_data$Country),
  unit_num = 1:length(unique(panel_data$Country))
)
print("Unit IDs:")
print(unit_ids)

# Join unit IDs to panel data
panel_data <- panel_data %>%
  left_join(unit_ids, by = "Country")

# Define treatment parameters
treatment_country <- "Colombia"
treatment_id      <- unit_ids$unit_num[unit_ids$Country == treatment_country]
treatment_time    <- 2021  # Use 2021.33 for April 2021 if you want to be precise
control_ids       <- unit_ids$unit_num[unit_ids$Country != treatment_country]

# Verify treatment parameters
print(paste("Treatment ID:", treatment_id))
print(paste("Treatment country:", treatment_country))
print(paste("Number of control countries:", length(control_ids)))

# Get exact pre-treatment times from the data
pre_treat_times <- panel_data %>%
  filter(Country == treatment_country, time < treatment_time) %>%
  pull(time) %>%
  unique() %>%
  sort()

pre_treat_times

print("Pre-treatment time periods for optimization:")
print(head(pre_treat_times))
print(paste("Total pre-treatment periods:", length(pre_treat_times)))

# Make sure these exist in the dataset
time_check <- panel_data %>%
  filter(time %in% pre_treat_times) %>%
  count()
print(paste("Number of observations with pre-treatment times:", time_check$n))

tail(panel_data)

head(covariate_wide)

# Start with your data cleaning code
# [your previous data cleaning code here]

# Load required libraries
library(tidyverse)
library(zoo)
library(Synth)
library(lubridate)

# 1. Define the good countries we want to include
good_countries <- c("Ecuador", "Costa Rica", "Dominican Republic", "Paraguay", 
                    "Peru", "Bolivia", "Brazil", "Chile", 
                    "Colombia", "Mexico", "Panama", "Uruguay")

# 2. Filter the outcome data to include only good countries
outcome_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJ2e_onrJcvj7ySfQv7ZZCsKT_SkrAn6GGGmEb9jnmy02S3IThLCkwaSt8ofyusoVRC9WoX-dpQVzd/pub?gid=204188519&single=true&output=csv") %>%
  rename_with(~"Event_month", matches("^Event")) %>%
  rename_with(~"Protest_Events", matches("^Events$")) %>%
  mutate(
    Event_month = ymd(Event_month),
    Year = year(Event_month),
    Month = month(Event_month),
    time = Year + (Month - 1)/12
  ) %>%
  select(Country, Year, Month, time, Protest_Events) %>%
  # Filter to include only good countries
  filter(Country %in% good_countries)

# Print countries in the filtered outcome data
print("Countries in filtered outcome data:")
print(unique(outcome_data$Country))

# 3. Filter covariate data to include only good countries
covariate_wide <- covariate_wide %>%
  filter(Country %in% good_countries)

# 4. Join outcome and covariate data
panel_data <- outcome_data %>%
  left_join(covariate_wide, by = c("Country", "Year"))

# Define the good countries we want to include
good_countries <- c("Ecuador", "Costa Rica", "Dominican Republic", "Paraguay", 
                    "Peru", "Bolivia", "Brazil", "Chile", 
                    "Colombia", "Mexico", "Panama", "Uruguay")

# Filter panel_data to include only good countries
panel_data <- panel_data %>%
  filter(Country %in% good_countries)

head(panel_data)

# Print countries in the filtered panel data
print("Countries in filtered panel data:")
print(unique(panel_data$Country))

# Create fresh unit IDs for the filtered countries
unit_ids <- data.frame(
  Country = unique(panel_data$Country),
  unit_num = 1:length(unique(panel_data$Country))
)

print("Unit IDs for analysis:")
print(unit_ids)

# Remove any existing unit_num column (if it exists) and add the new unit_num
if("unit_num" %in% names(panel_data)) {
  panel_data <- panel_data %>% select(-unit_num)
}

# Add unit_num to panel_data
panel_data <- panel_data %>%
  left_join(unit_ids, by = "Country")

# Create month_id if it doesn't exist
if(!"month_id" %in% names(panel_data)) {
  panel_data <- panel_data %>%
    mutate(month_id = (Year - 2018) * 12 + Month)
}

# Define treatment parameters
treatment_country <- "Colombia"
treatment_id <- unit_ids$unit_num[unit_ids$Country == treatment_country]
control_ids <- unit_ids$unit_num[unit_ids$Country != treatment_country]
treatment_month <- (2021 - 2018) * 12 + 4  # April 2021 = month_id 40

print("Treatment settings:")
print(paste("Treatment country:", treatment_country))
print(paste("Treatment ID:", treatment_id))
print(paste("Control countries:", paste(good_countries[good_countries != treatment_country], collapse=", ")))
print(paste("Number of control countries:", length(control_ids)))
print(paste("Treatment month:", treatment_month))

# Check which country is unit 11
country_11 <- unit_ids$Country[unit_ids$unit_num == 11]
print(paste("Country with missing secondary_enrollment data is:", country_11))

# Check all predictors for all countries
predictor_check <- panel_data %>%
  group_by(Country, unit_num) %>%
  summarize(
    total_obs = n(),
    missing_gdp = sum(is.na(gdp_current)),
    missing_inflation = sum(is.na(inflation)),
    missing_income = sum(is.na(income_share_bottom20)),
    missing_enrollment = sum(is.na(secondary_enrollment)),
    .groups = "drop"
  ) %>%
  arrange(desc(missing_enrollment))

print("Predictor completeness by country:")
print(predictor_check)

# Define good countries - updating based on the latest findings
# Exclude country 11 from the controls
good_unit_ids <- unit_ids %>%
  filter(unit_num != 11 | Country == treatment_country)

# Update control IDs
control_ids <- good_unit_ids$unit_num[good_unit_ids$Country != treatment_country]

print("Updated control countries:")
print(good_unit_ids$Country[good_unit_ids$Country != treatment_country])
print(paste("Number of control countries:", length(control_ids)))

# Try dataprep again with the updated control list
dp <- dataprep(
  foo = panel_data,
  predictors = c("gdp_current", "inflation", 
                 "income_share_bottom20", "secondary_enrollment"),
  predictors.op = "mean",
  special.predictors = list(
    list("Protest_Events", seq(1,12,1), "mean"),  # 2018
    list("Protest_Events", seq(13,24,1), "mean"), # 2019
    list("Protest_Events", seq(25,36,1), "mean")  # 2020
  ),
  dependent = "Protest_Events",
  unit.variable = "unit_num",
  time.variable = "month_id",
  treatment.identifier = treatment_id,
  controls.identifier = control_ids,
  time.predictors.prior = seq(1,36,1),            # months 1-36 = 2018-2020
  time.optimize.ssr = seq(1, treatment_month-1),  # up to month 39 (Mar 2021)
  unit.names.variable = "Country",
  time.plot = seq(1, max(panel_data$month_id), by = 1)
)

# Run Synth and extract weights
synth_out <- synth(dp)
w <- synth_out$solution.w

w

control_countries <- unit_ids$Country[unit_ids$unit_num %in% control_ids]
country_weights <- data.frame(
  Country = control_countries,
  Weight = as.numeric(w)
) %>%
  arrange(desc(Weight))

print("Synthetic control weights:")
print(country_weights)


print("Synthetic control weights:")
print(country_weights)

# Build synthetic control time series
synthetic_series <- as.numeric(dp$Y0plot %*% w)
actual_series <- dp$Y1plot
time_axis <- dp$tag$time.plot

# Prepare data for plotting
df_plot <- data.frame(
  month_id = rep(time_axis, 2),
  time = rep(2018 + (time_axis-1)/12, 2),  # Convert back to decimal years
  events = c(actual_series, synthetic_series),
  Series = rep(c("Colombia", "Synthetic Colombia"), each = length(time_axis))
)

# Plot Actual vs Synthetic Protest Events
p1 <- ggplot(df_plot, aes(x = time, y = events, color = Series)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2021.25, linetype = "dashed") +  # April 2021
  labs(x = "Year", y = "Monthly Protest Events",
       title = "Colombia vs Synthetic Colombia (April 2021 Tax Reform)") +
  theme_minimal()

p1

# Create a clean plot with only years shown
path.plot(
  synth.res = synth_out,
  dataprep.res = dp,
  tr.intake = treatment_month,
  Ylab = "Monthly Protest Events", 
  Xlab = "Year", 
  Legend = c("Colombia", "Synthetic Colombia"),
  Main = "Colombia vs Synthetic Colombia (April 2021 Tax Reform)"
)
abline(v = treatment_month, lty = "dashed")

# Clear the existing x-axis
axis(1, at = seq(0, 100, by = 10), labels = FALSE, tck = 0)

# Add a clean x-axis with just years
year_points <- c(1, 13, 25, 37, 49, 61) # Jan of each year (2018-2023)
year_labels <- 2018:2023
axis(1, at = year_points, labels = year_labels)

# Recreate the gap dataframe
gap_df <- data.frame(
  month_id = dp$tag$time.plot,
  time = 2018 + (dp$tag$time.plot - 1)/12,  # Convert to decimal years
  gap = dp$Y1plot - as.numeric(dp$Y0plot %*% w)  # Colombia - Synthetic Colombia
)

# Check the structure of the dataframe
str(gap_df)
head(gap_df)

# Now plot the gap
p2 <- ggplot(gap_df, aes(x = time, y = X3)) +
  geom_line(size = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2021.25, linetype = "dashed") +
  scale_x_continuous(breaks = 2018:2023, limits = c(2018, 2023)) +
  labs(x = "Year", y = "Gap (Colombia − Synthetic Colombia)",
       title = "Estimated Treatment Effect on Protest Events") +
  theme_classic()

p2



# Calculate pre/post treatment effect statistics
gap_summary <- gap_df %>%
  mutate(period = ifelse(time < 2021.25, "Pre-Treatment", "Post-Treatment")) %>%
  group_by(period) %>%
  summarise(
    mean_gap = mean(X3),
    median_gap = median(X3),
    min_gap = min(X3),
    max_gap = max(X3)
  )

print("Gap summary statistics:")
print(gap_summary)

# Save results
write_csv(df_plot, "colombia_synthetic_control_results.csv")
write_csv(gap_df, "colombia_treatment_effect_gap.csv")
write_csv(country_weights, "synthetic_colombia_weights.csv")

# Create a plot showing which countries contribute to synthetic Colombia
top_donors <- country_weights %>%
  filter(Weight > 0.01) %>%  # Only show countries with >1% contribution
  mutate(Country = reorder(Country, Weight))

ggplot(top_donors, aes(x = Weight, y = Country)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Countries Contributing to Synthetic Colombia",
    subtitle = "Countries with >1% weight in synthetic control",
    x = "Weight",
    y = NULL
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(Weight*100, 1), "%")), 
            hjust = -0.1, size = 3.5) +
  expand_limits(x = max(top_donors$Weight) * 1.15)



# Define treatment parameters
treatment_country <- "Peru"
treatment_id <- unit_ids$unit_num[unit_ids$Country == treatment_country]
control_ids <- unit_ids$unit_num[unit_ids$Country != treatment_country]
treatment_month <- (2021 - 2018) * 12 + 4  # April 2021 = month_id 40

print("Treatment settings:")
print(paste("Treatment country:", treatment_country))
print(paste("Treatment ID:", treatment_id))
print(paste("Control countries:", paste(good_countries[good_countries != treatment_country], collapse=", ")))
print(paste("Number of control countries:", length(control_ids)))
print(paste("Treatment month:", treatment_month))
# Check which country is unit 11
country_11 <- unit_ids$Country[unit_ids$unit_num == 11]
print(paste("Country with missing secondary_enrollment data is:", country_11))

# Check all predictors for all countries
predictor_check <- panel_data %>%
  group_by(Country, unit_num) %>%
  summarize(
    total_obs = n(),
    missing_gdp = sum(is.na(gdp_current)),
    missing_inflation = sum(is.na(inflation)),
    missing_income = sum(is.na(income_share_bottom20)),
    missing_enrollment = sum(is.na(secondary_enrollment)),
    .groups = "drop"
  ) %>%
  arrange(desc(missing_enrollment))

print("Predictor completeness by country:")
print(predictor_check)

# Define good countries - updating based on the latest findings
# Exclude country 11 from the controls
good_unit_ids <- unit_ids %>%
  filter(unit_num != 11 | Country == treatment_country)

# Update control IDs
control_ids <- good_unit_ids$unit_num[good_unit_ids$Country != treatment_country]

print("Updated control countries:")
print(good_unit_ids$Country[good_unit_ids$Country != treatment_country])
print(paste("Number of control countries:", length(control_ids)))

# Try dataprep again with the updated control list
dp <- dataprep(
  foo = panel_data,
  predictors = c("gdp_current", "inflation", 
                 "income_share_bottom20", "secondary_enrollment"),
  predictors.op = "mean",
  special.predictors = list(
    list("Protest_Events", seq(1,12,1), "mean"),  # 2018
    list("Protest_Events", seq(13,24,1), "mean"), # 2019
    list("Protest_Events", seq(25,36,1), "mean")  # 2020
  ),
  dependent = "Protest_Events",
  unit.variable = "unit_num",
  time.variable = "month_id",
  treatment.identifier = treatment_id,
  controls.identifier = control_ids,
  time.predictors.prior = seq(1,36,1),            # months 1-36 = 2018-2020
  time.optimize.ssr = seq(1, treatment_month-1),  # up to month 39 (Mar 2021)
  unit.names.variable = "Country",
  time.plot = seq(1, max(panel_data$month_id), by = 1)
)

# Run Synth and extract weights
synth_out <- synth(dp)
w <- synth_out$solution.w

w

control_countries <- unit_ids$Country[unit_ids$unit_num %in% control_ids]
country_weights <- data.frame(
  Country = control_countries,
  Weight = as.numeric(w)
) %>%
  arrange(desc(Weight))

print("Synthetic control weights:")
print(country_weights)


print("Synthetic control weights:")
print(country_weights)

# Build synthetic control time series
synthetic_series <- as.numeric(dp$Y0plot %*% w)
actual_series <- dp$Y1plot
time_axis <- dp$tag$time.plot

# Prepare data for plotting
df_plot <- data.frame(
  month_id = rep(time_axis, 2),
  time = rep(2018 + (time_axis-1)/12, 2),  # Convert back to decimal years
  events = c(actual_series, synthetic_series),
  Series = rep(c("Peru", "Synthetic Peru"), each = length(time_axis))
)

# Plot Actual vs Synthetic Protest Events
p1 <- ggplot(df_plot, aes(x = time, y = events, color = Series)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2021.25, linetype = "dashed") +  # April 2021
  labs(x = "Year", y = "Monthly Protest Events",
       title = "Peru vs Synthetic Peru (In-space placebo)") +
  theme_minimal()

p1

# Recreate the gap dataframe
gap_df <- data.frame(
  month_id = dp$tag$time.plot,
  time = 2018 + (dp$tag$time.plot - 1)/12,  # Convert to decimal years
  gap = dp$Y1plot - as.numeric(dp$Y0plot %*% w)  # Colombia - Synthetic Colombia
)

# Check the structure of the dataframe
str(gap_df)
head(gap_df)

# Now plot the gap
p2 <- ggplot(gap_df, aes(x = time, y = X8)) +
  geom_line(size = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2021.25, linetype = "dashed") +
  scale_x_continuous(breaks = 2018:2023, limits = c(2018, 2023)) +
  labs(x = "Year", y = "Gap (Peru − Synthetic Peru)",
       title = "Estimated Treatment Effect for In-place Placebo") +
  theme_classic()

p2

# Calculate pre/post treatment effect statistics
gap_summary <- gap_df %>%
  mutate(period = ifelse(time < 2021.25, "Pre-Treatment", "Post-Treatment")) %>%
  group_by(period) %>%
  summarise(
    mean_gap = mean(X8),
    median_gap = median(X8),
    min_gap = min(X8),
    max_gap = max(X8)
  )

print("Gap summary statistics:")
print(gap_summary)




# Define treatment parameters
treatment_country <- "Colombia"
treatment_id <- unit_ids$unit_num[unit_ids$Country == treatment_country]
control_ids <- unit_ids$unit_num[unit_ids$Country != treatment_country]
treatment_month <- (2019 - 2018) * 12 + 4  # April 2021 = month_id 40

print("Treatment settings:")
print(paste("Treatment country:", treatment_country))
print(paste("Treatment ID:", treatment_id))
print(paste("Control countries:", paste(good_countries[good_countries != treatment_country], collapse=", ")))
print(paste("Number of control countries:", length(control_ids)))
print(paste("Treatment month:", treatment_month))

# Prepare data for Synth
dp <- dataprep(
  foo = panel_data,
  predictors = c("gdp_current", "inflation", 
                 "income_share_bottom20", "secondary_enrollment"),
  predictors.op = "mean",
  special.predictors = list(
    list("Protest_Events", seq(1,12,1), "mean"),  # 2018
    list("Protest_Events", seq(13,24,1), "mean"), # 2019
    list("Protest_Events", seq(25,36,1), "mean")  # 2020
  ),
  dependent = "Protest_Events",
  unit.variable = "unit_num",
  time.variable = "month_id",
  treatment.identifier = treatment_id,
  controls.identifier = control_ids,
  time.predictors.prior = seq(1,36,1),            # months 1-36 = 2018-2020
  time.optimize.ssr = seq(1, treatment_month-1),  # up to month 39 (Mar 2021)
  unit.names.variable = "Country",
  time.plot = seq(1, max(panel_data$month_id), by = 1)
)

# Run Synth and extract weights
synth_out <- synth(dp)
w <- synth_out$solution.w

w

control_countries <- unit_ids$Country[unit_ids$unit_num %in% control_ids]
country_weights <- data.frame(
  Country = control_countries,
  Weight = as.numeric(w)
) %>%
  arrange(desc(Weight))

print("Synthetic control weights:")
print(country_weights)


print("Synthetic control weights:")
print(country_weights)

# Build synthetic control time series
synthetic_series <- as.numeric(dp$Y0plot %*% w)
actual_series <- dp$Y1plot
time_axis <- dp$tag$time.plot

# Prepare data for plotting
df_plot <- data.frame(
  month_id = rep(time_axis, 2),
  time = rep(2018 + (time_axis-1)/12, 2),  # Convert back to decimal years
  events = c(actual_series, synthetic_series),
  Series = rep(c("Colombia", "Synthetic Colombia"), each = length(time_axis))
)

# Plot Actual vs Synthetic Protest Events
p1 <- ggplot(df_plot, aes(x = time, y = events, color = Series)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2019.25, linetype = "dashed") +  # April 2021
  labs(x = "Year", y = "Monthly Protest Events",
       title = "Colombia vs Synthetic Colombia (April 2021 Tax Reform)") +
  theme_minimal()

p1


