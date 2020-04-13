# load libraries
library(hrbrthemes)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(janitor)

# load helper functions
source("R/Helpers.R")

# Data Cleaning/Manipulation ----------------------------------------------

# read in state mobility trends
USStates <- read_csv("data/Social Distancing - States.csv")

# read in state governmental actions and basic covid-19 data
USStateActions <- read_csv("data/Social Distancing - State Actions.csv") %>%
  janitor::clean_names()

# read in confirmed cases
ConfirmedCases <- read_csv("data/Social Distancing - Sheet4.csv") %>%
  select(1:3) %>%
  pivot_longer(cols = c(2:3), names_to = "date", values_to = "confirmed_cases_through_date", names_prefix = "cases_through") %>%
  mutate(date = mdy(date))

# join dataframes and remove unnecessary data
USStates <- left_join(USStates, USStateActions, by = "state") %>%
  mutate_at(vars(starts_with("date"), state_mandated_school_closures, emergency_declaration), mdy) %>%
  left_join(ConfirmedCases, by = c("state", "date"))
rm(USStateActions, ConfirmedCases)

# find days between 1st case and 1st death
USStates <- USStates %>%
  mutate(days_from_case_to_death = date_of_1st_death - date_of_1st_case,
         cases_per_capita = confirmed_cases_through_date/population)

# find most unique states in terms of mobility trends
USStates <- find_euclidean_dist(data = USStates)

# pivot from wide to long format
USStates_Long <- USStates %>%
  pivot_longer(cols = c("retail_recreation", "grocery_pharmacy", "parks",	
                        "transit_stations",	"workplaces",	"residential"),
               names_to = "type") %>%
  group_by(date, type) %>%
  mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA)),
         high_low = case_when(
           !is.na(outlier) & outlier > mean(value) ~ "High",
           !is.na(outlier) & outlier < mean(value) ~ "Low",
           TRUE ~ as.character(NA)),
         mean = mean(value))
