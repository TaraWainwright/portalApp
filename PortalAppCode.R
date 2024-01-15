# Creating an app to visualise NHM data portal queries
## Data wrangling


rm(list=ls())
setwd("C:/Users/taraw/OneDrive - Natural History Museum/Documents/Data Accelerator")

library(tidyverse)
library(lubridate)

# Download data
queries <- read.csv("query_doi.csv")
actions <- read.csv("query_doi_stat.csv")
resourcenames <- read.csv("resources.csv")


# Join data and extract date from timestamp
joined_queries <- left_join(actions, queries, by ="doi")
joined_queries$date <- substr(joined_queries$timestamp.y, 1, 10)
str(joined_queries, max.level = 1)


# Select columns of interest
narrowed_d <- joined_queries %>% 
  select("resources_and_versions", "doi", "date", "identifier", "query", "action", "count") %>% 
  rename(search_count = count)


# Change class of date column
narrowed_d$date <- as.Date(narrowed_d$date, format = "%Y-%m-%d")
# Add column with shortened date (Month/Year) for visuals
narrowed_d <- narrowed_d %>% 
  mutate(floored_date = floor_date(date, "month"))


# Separate rows and extract resources into new row 'resource_id'
resources_data <- narrowed_d %>% 
  separate_longer_delim(resources_and_versions, delim = ',') %>%
  mutate(resource_id = str_extract(resources_and_versions, '"(.*)"', 1))
# str_extract extracts elements of column based on following regex
# '"(.*)"' looks for any number/character within quotation marks
# '1' (or group = 1) indicates group within regex - so extracts the group we want to extract


# Extract queries - first 'q' queries
querydata <- resources_data %>% 
  mutate(searches = str_extract(query, '"(?:search|q)": "([^"]+)"', group = 1))
# could also use group = 2 and get rid of the ?: bit


# Join data by resource id
df <- left_join(querydata, resourcenames, by ="resource_id") %>% 
# Select all columns except for resources_and_versions
  select(-resources_and_versions) %>% 
# Replace column name for count of datapoints in each resource
  rename(resource_count = count) %>% 
# Calculate percentage of resource that is downloaded
  mutate(percent = round(search_count/resource_count * 100, 2))


# Group queries under each doi together
qs <- df %>% 
  group_by(resource_id, floored_date) %>%
  select(resource_id, resource_name, floored_date, searches) %>% 
  separate_rows(searches, sep = ' ') %>% 
  # create count for each word within each resource
  add_count(searches) %>% 
  distinct() %>% 
  drop_na(searches)

# Remove punctuation from searches column
qs$searches <- gsub("[[:punct:][:blank:]]+", " ", qs$searches)
# Remove capitalisation
qs$searches <- tolower(qs$searches)

# Save search data
write_csv(qs, "search_data.csv")

# Count data: 

# Number of downloads per resource per month
dlcount <- df %>% 
  group_by(resource_id, resource_name, floored_date) %>% 
  count(action, name = "download_count") %>% 
  spread(key = action, value = download_count) %>% 
  mutate_all(~replace(., is.na(.), 0))
# split downloads and saves into separate column so only one row per month

# Number of unique dois generated for each resource per month
dcount <- df %>% 
  group_by(resource_id, resource_name, floored_date) %>% 
  summarise(doi_count = n_distinct(doi)) 

# Check sum of points is correct
sum(dcount$doi_count)

# Number of unique users using each resource per month
ucount <- df %>% 
  group_by(resource_id, resource_name, floored_date) %>% 
  summarise(user_count = n_distinct(identifier))


# Join data together 
list_df = list(dcount,ucount,dlcount)
final <- list_df %>% 
  reduce(left_join) %>% 
  arrange(resource_name, floored_date)

# Export final dataset
write_csv(final, "final_data2.csv")

