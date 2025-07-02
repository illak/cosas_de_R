library(tidyverse)

bike_share_rides <- read_rds("data/bike_share_rides_ch1_1.rds")

# Converting data types ====

# Glimpse at bike_share_rides
glimpse(bike_share_rides)

# Summary of user_birth_year
summary(bike_share_rides$user_birth_year)

# Convert user_birth_year to factor: user_birth_year_fct
bike_share_rides <- bike_share_rides %>%
  mutate(user_birth_year_fct = factor(user_birth_year))

# Summary of user_birth_year_fct
summary(bike_share_rides$user_birth_year_fct)

glimpse(bike_share_rides)



## Trimming STRINGS ====
bike_share_rides <- bike_share_rides %>%
  # Remove 'minutes' from duration: duration_trimmed
  mutate(duration_trimmed = str_remove(duration, "minutes"),
         # Convert duration_trimmed to numeric: duration_mins
         duration_mins = as.numeric(duration_trimmed))

# Glimpse at bike_share_rides
glimpse(bike_share_rides)

# Calculate mean duration
mean(bike_share_rides$duration_mins)


## Ride duration constraints ====
# Create breaks
breaks <- c(min(bike_share_rides$duration_mins), 0, 1440, max(bike_share_rides$duration_mins))

# Create a histogram of duration_min
ggplot(bike_share_rides, aes(x = duration_mins)) +
  geom_histogram(breaks = breaks)



## duration_min_const: replace vals of duration_min > 1440 with 1440
bike_share_rides <- bike_share_rides %>%
  mutate(duration_min_const = replace(duration_mins, duration_mins > 1440, 1440))


## Back to the future (DATES) ====

library(lubridate)
# Convert date to Date type
bike_share_rides <- bike_share_rides %>%
  mutate(date = as.Date(date))


# Filter for rides that occurred before or on today's date
bike_share_rides_past <- bike_share_rides %>%
  filter(date <= today())


## Full duplicates ====
# Count the number of full duplicates
sum(duplicated(bike_share_rides))

# Remove duplicates
bike_share_rides_unique <- distinct(bike_share_rides)

# Count the full duplicates in bike_share_rides_unique
sum(duplicated(bike_share_rides_unique))


## Removing partial duplicates ====

#Find duplicated ride_ids
bike_share_rides %>% 
  count(ride_id) %>% 
  filter(n > 1)

# Remove full and partial duplicates
bike_share_rides_unique <- bike_share_rides %>%
  # Only based on ride_id instead of all cols
  distinct(ride_id, .keep_all = TRUE)

# Find duplicated ride_ids in bike_share_rides_unique
bike_share_rides_unique %>%
  # Count the number of occurrences of each ride_id
  count(ride_id) %>%
  # Filter for rows with a count > 1
  filter(n > 1)



## Aggregating partial duplicates ====

bike_share_rides %>%
  # Group by ride_id and date
  group_by(ride_id, date) %>%
  # Add duration_min_avg column
  mutate(duration_min_avg = mean(duration_mins)) %>%
  # Remove duplicates based on ride_id and date, keep all cols
  distinct(ride_id, date, .keep_all = TRUE) %>%
  # Remove duration_min column
  select(-duration_mins)


# Categorical Data ====
sfo_survey <- read_rds("data/sfo_survey_ch2_1.rds")
dest_sizes <- data.frame(
  dest_size = c("Small","Medium","Large","Hub"),
  passengers_per_day = c("0-20K","20K-70K","70K-100K","100K+")
)

## Not a member ====

# Count the number of occurrences of dest_size
sfo_survey %>%
  count(dest_size)


# Find bad dest_size rows
sfo_survey %>% 
  # Join with dest_sizes data frame to get bad dest_size rows
  anti_join(dest_sizes, by = "dest_size") %>%
  # Select id, airline, destination, and dest_size cols
  select(id, airline, destination, dest_size)


# Remove bad dest_size rows
sfo_survey %>% 
  # Join with dest_sizes
  semi_join(dest_sizes, by = "dest_size") %>%
  # Count the number of each dest_size
  count(dest_size)

## Identifying inconsistency ====

# Count dest_size
sfo_survey %>%
  count(dest_size)


# Count cleanliness
sfo_survey %>%
  count(cleanliness)

## Correcting inconsistency ====

# Add new columns to sfo_survey
sfo_survey <- sfo_survey %>%
  # dest_size_trimmed: dest_size without whitespace
  mutate(dest_size_trimmed = str_trim(dest_size),
         # cleanliness_lower: cleanliness converted to lowercase
         cleanliness_lower = tolower(cleanliness))

# Count values of dest_size_trimmed
sfo_survey %>%
  count(dest_size_trimmed)

# Count values of cleanliness_lower
sfo_survey %>%
  count(cleanliness_lower)


## Collapsing categories ====

# Count categories of dest_region
sfo_survey %>%
  count(dest_region)

# Categories to map to Europe
europe_categories <- c("EU", "Europ", "eur")

# Add a new col dest_region_collapsed
sfo_survey %>%
  # Map all categories in europe_categories to Europe
  mutate(dest_region_collapsed = fct_collapse(dest_region, 
                                              Europe = europe_categories)) %>%
  # Count categories of dest_region_collapsed
  count(dest_region_collapsed)


## Detecting inconsistent text data ====

# Filter for rows with "-" in the phone column
sfo_survey %>%
  filter(str_detect(phone,"-"))

# Filter for rows with "(" or ")" in the phone column
sfo_survey %>%
  filter(str_detect(phone, fixed("(")) | str_detect(phone, fixed(")")))


## Replacing and removing ====

# Remove parentheses from phone column
phone_no_parens <- sfo_survey$phone %>%
  # Remove "("s
  str_remove_all(fixed("(")) %>%
  # Remove ")"s
  str_remove_all(fixed(")"))

# Add phone_no_parens as column
sfo_survey %>%
  mutate(phone_no_parens = phone_no_parens,
         # Replace all hyphens in phone_no_parens with spaces
         phone_clean = str_replace_all(phone_no_parens, "-", ""))


## Invalid phone numbers ====

# Check out the invalid numbers
sfo_survey %>%
  filter(str_length(phone) != 12)

# Remove rows with invalid numbers
sfo_survey %>%
  filter(str_length(phone) == 12)

# Advanced Data Problems ====

# Crear vectores con los datos
id <- c(
  "A880C79F","BE8222DF","19F9E113","A2FE52A3","F6DC2C08","D2E55799","53AE87EF",
  "3E97F253","4AE79EA1","2322DFB4","645335B2","D5EB0F00","1EB593F7","DDBA03D9",
  "40E4A2F4","39132EEA","387F8E4D","11C3C3C0","C2FC91E1","FB8F01C1","0128D2D0",
  "BE6E4B3F","7C6E2ECC","02E63545","4399C98B","98F4CF0F","247222A6","420985EE",
  "0E3903BA","64EF994F","CCF84EDB","51C21705","C868C6AD","92C237C6","9ECEADB2",
  "DF0AFE50","5CD605B3","402839E2","78286CE7","168E071B","466CCDAA","8DE1ECB9",
  "E19FE6B5","1240D39C","A7BFAA72","C3D24436","FAD92F0F","236A1D51","A6DDDC4C",
  "DDFD0B3D","D13375E9","AC50B796","290319FD","FC71925A","7B0F3685","BE411172",
  "58066E39","EA7FF83A","14A2DDB7","305EEAA8","8F25E54C","19DD73C6","ACB8E6AF",
  "91BFCC40","86ACAF81","77E85C14","C5C6B79D","0E5B69F5","5275B518","17217048",
  "E7496A7F","41BBB7B4","F6C7ABA1","E699DF01","BACA7378","84A4302F","F8A78C27",
  "8BADDF6A","9FB57E68","5C98E8F5","6BB53C2A","E23F2505","0C121914","3627E08A",
  "A94493B3","0682E9DE","49931170","A154F63B","3690CCED","48F5E6D8","515FAD84",
  "59794264","2038185B","65EAC615","6C7509C9","BD969A9D","B0CDCE3D","33A7F03E"
)

office <- c(
  "New York","New York","Tokyo","Tokyo","New York","Tokyo","Tokyo","Tokyo",
  "Tokyo","New York","New York","New York","New York","Tokyo","New York",
  "Tokyo","Tokyo","New York","New York","Tokyo","Tokyo","Tokyo","New York",
  "New York","New York","Tokyo","New York","New York","New York","New York",
  "New York","Tokyo","Tokyo","Tokyo","New York","Tokyo","New York","New York",
  "Tokyo","New York","Tokyo","New York","New York","Tokyo","New York","New York",
  "Tokyo","Tokyo","New York","Tokyo","Tokyo","Tokyo","New York","New York",
  "New York","Tokyo","Tokyo","Tokyo","Tokyo","Tokyo","New York","Tokyo",
  "New York","New York","Tokyo","Tokyo","New York","Tokyo","New York","New York",
  "New York","New York","New York","Tokyo","New York","New York","New York",
  "New York","New York","New York","New York","New York","New York","New York",
  "Tokyo","New York","New York","New York","New York","New York","New York",
  "New York","New York","New York","New York","New York","New York","New York"
)

# Combinar en un data.frame
account_offices <- data.frame(
  id     = id,
  office = office,
  stringsAsFactors = FALSE
)

## Date uniformity -----------------------------------------------
accounts <- read_rds("data/ch3_1_accounts.rds")

# Check out the accounts data frame
head(accounts)

# Define the date formats
formats <- c("%Y-%m-%d", "%B %d, %Y")

# Convert dates to the same format
accounts_v2 <- accounts %>%
  mutate(date_opened_clean = parse_date_time(date_opened, formats))


glimpse(accounts_v2)



## Currency uniformity -----------------------------------------------------

# Scatter plot of opening date and total amount
accounts %>%
  ggplot(aes(x = date_opened, y = total)) +
  geom_point()

# Left join accounts to account_offices by id
accounts %>%
  left_join(account_offices, by = "id") %>%
  # Convert totals from the Tokyo office to USD
  mutate(total_usd = ifelse(office == "Tokyo", total / 104, total)) %>%
  # Scatter plot of opening date vs total_usd
  ggplot(aes(x = date_opened, y = total_usd)) +
  geom_point()


## Validating totals -------------------------------------------------------
# Find invalid totals
accounts %>%
  # theoretical_total: sum of the three funds
  mutate(theoretical_total = fund_A + fund_B + fund_C) %>%
  # Find accounts where total doesn't match theoretical_total
  filter(total != theoretical_total)



## Validating age ----------------------------------------------------------
# Find invalid acct_age
accounts %>%
  # theoretical_age: age of acct based on date_opened
  mutate(theoretical_age = floor(as.numeric(date_opened %--% today(), "years"))) %>%
  # Filter for rows where acct_age is different from theoretical_age
  filter(acct_age != theoretical_age)



## Visualizing missing data ------------------------------------------------
# Visualize the missing values by column
library(visdat)

vis_miss(accounts)

accounts %>%
  # missing_inv: Is inv_amount missing?
  mutate(missing_inv = is.na(inv_amount)) %>%
  # Group by missing_inv
  group_by(missing_inv) %>%
  # Calculate mean age for each missing_inv group
  summarize(avg_age = mean(age))

# Sort by age and visualize missing vals
accounts %>%
  arrange(age) %>%
  vis_miss()


## Treating missing data --------------------------------------------------
# Create accounts_clean
accounts_clean <- accounts %>%
  # Filter to remove rows with missing cust_id
  filter(!is.na(cust_id)) %>%
  # Add new col acct_amount_filled with replaced NAs
  mutate(acct_amount_filled = ifelse(is.na(acct_amount), inv_amount * 5, acct_amount))

# Assert that cust_id has no missing vals
assert_all_are_not_na(accounts_clean$cust_id)

# Assert that acct_amount_filled has no missing vals
assert_all_are_not_na(accounts_clean$acct_amount_filled)


