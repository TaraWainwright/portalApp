# Creating an app to visualise NHM data portal queries
## Data wrangling


rm(list=ls())
setwd("C:/Users/taraw/OneDrive - Natural History Museum/Documents/Data Accelerator")

library(tidyverse)
library(lubridate)
library(zoo)

# Download data
queries <- read.csv("query_doi.csv")
actions <- read.csv("query_doi_stat.csv")
resourcenames <- read.csv("resources.csv")


# Join data and extract date from timestamp
joined_queries <- left_join(actions, queries, by ="doi")
joined_queries$date <- substr(joined_queries$timestamp.y, 1, 10)
str(joined_queries, max.level = 1)


# Extract complicated row with multiple resources in same row for testing
# dodgy <- joined_queries[19358, ]
# write.csv(dodgy, file = "complicated_row.csv", row.names = F)


# Select columns of interest
narrowed_d <- joined_queries %>% 
  select("resources_and_versions", "doi", "date", "identifier", "query", "action", "count") %>% 
  rename(search_count = count)


# Change class of date column
narrowed_d$date <- as.Date(narrowed_d$date, format = "%Y-%m-%d")
# Add column with shortened date (Month/Year) for visuals
narrowed_d <- narrowed_d %>% 
  mutate(floored_date = floor_date(date, "month"))

# narrowed_d <- narrowed_d %>% 
#   mutate(sdate = zoo::as.yearmon(date))
# str(narrowed_d)


# Separate rows and extract resources into new row 'resource_id'
resources_data <- narrowed_d %>% 
  separate_longer_delim(resources_and_versions, delim = ',') %>%
  mutate(resource_id = str_extract(resources_and_versions, '"(.*)"', 1))
# separate_longer_delim separates row into multiple rows based on delimiter
# mutate adds new column
# str_extract extracts elements of column based on following regex
# '"(.*)"' looks for any number/character within quotation marks
# '1' (or group = 1) indicates group within regex - so extracts the group we want to extract

# Another way to extract string in quotation
# library(stringi) 
# resources_data[ , 'resource_id'] <- NA
# resources_data$resource_id <- stringi::stri_extract_all_regex(resources_data$resources_and_versions, '(?<=").*?(?=")')


# Extract queries - first 'q' queries
querydata <- resources_data %>% 
  mutate(searches = str_extract(query, '"(?:search|q)": "([^"]+)"', group = 1))
# could also use group = 2 and get rid of the ?: bit


# extracts string after q": " and excludes strings after comma: '"q": "(.+)"(, )*"', group = 1
# extracts string between quotations but includes filer section: '(?<=q": ").+(?=")'
# extracts string after colon: '(?<=:).+' e.g. "platichthys"}
# extracts the first bit, want to do the opposite: "^[^\\:]+" e.g. {"q"
# extracts last string: '\\S+$' 
# extracts everything after first word: '\\b"(.+)"' e.g., ": "platichthys"



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

qs2 <- df %>% 
  group_by(resource_id, floored_date) %>%
  select(resource_id, resource_name, floored_date, searches) %>% 
  # create count for each word within each resource
  add_count(searches) %>% 
  distinct() %>% 
  drop_na(searches)


# qs2 <- df %>% 
#   group_by(resource_id, floored_date) %>%
#   select(resource_id, resource_name, floored_date, searches) %>% 
#   # create count for each word within each resource
#   mutate(wordcount = str_count(searches)) %>% 
# # only keep unique rows (remove duplicates)
#   distinct() %>% 
#   drop_na(wordcount)


# Save search data
write_csv(qs, "search_data.csv")
write_csv(qs2, "search_data_full.csv")


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

