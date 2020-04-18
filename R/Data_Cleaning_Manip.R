# load libraries
library(hrbrthemes)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(janitor)

# set seed for repeatability
set.seed(222)

# load helper functions
source("R/Helpers.R")

# Data Cleaning/Manipulation ----------------------------------------------

# read in state mobility trends
USStates <- read_csv("data/Social Distancing - States.csv")

# read in state governmental actions and basic covid-19 data
USStateActions <- read_csv("data/Social Distancing - State Actions.csv") %>%
  janitor::clean_names()

# read in confirmed cases
ConfirmedCases <- read_csv("data/Social Distancing - State Cases.csv") %>%
  pivot_longer(cols = c(2:4), names_to = "date", values_to = c("confirmed_cases"), names_prefix = "cases_through") %>%
  pivot_longer(cols = c(2:4), names_to = "date2", values_to = c("rate"), names_prefix = "rate") %>%
  mutate(date = mdy(date),
         date2 = mdy(date2)) %>%
  filter(date == date2) %>%
  select(-date2)

# join dataframes and remove unnecessary data
USStates <- left_join(USStates, USStateActions, by = "state") %>%
  mutate_at(vars(starts_with("date"), state_mandated_school_closures, emergency_declaration), mdy) %>%
  left_join(ConfirmedCases, by = c("state", "date"))

# read in data for state areas
USStates_Areas <- read_csv("data/Social Distancing - Area.csv")

USStates <- left_join(USStates, USStates_Areas, by = "state") %>%
  mutate(pop_density = population/area) %>%
  filter(state != "District of Columbia")
rm(USStates_Areas, USStateActions, ConfirmedCases)

# find days between 1st case and 1st death
USStates <- USStates %>%
  mutate(days_from_case_to_death = as.numeric(date_of_1st_death - date_of_1st_case),
         response_stay_home = as.numeric(date_of_stay_at_home_order - date_of_1st_case),
         response_school = as.numeric(state_mandated_school_closures - date_of_1st_case),
         response_emergency = as.numeric(emergency_declaration - date_of_1st_case),
         cases_per_capita = confirmed_cases/population)

# find most unique states in terms of mobility trends
USStates <- find_euclidean_dist(data = USStates, dates = 3)

USStates <- USStates %>%
  left_join(USStates %>%
              filter(date == "2020-03-29") %>%
              select(state, confirmed_cases) %>%
              rename(cases_3_29 = confirmed_cases),
            by = "state")

# find clusters based on population density and initial cases (on 3/29)
# standardize mobility data
US_standardized <- standardize_data(USStates, mobility = F)

distance <- US_standardized %>%
  dplyr::select(pop_density, cases_3_29) %>%
  dist()

k_means <- kmeans(distance, 4)

USStates <- USStates %>%
  dplyr::mutate(cluster_pop = k_means$cluster)

# find clusters within population density and initial cases clusters of social mobility
USStates <- find_clusters_within(USStates)

# # convert cluster number to factor with nice labels
# USStates <- USStates %>%
#   mutate(cluster_k_means = case_when(
#     cluster_k_means == 1 ~ "Cluster 1",
#     cluster_k_means == 2 ~ "Cluster 2",
#     cluster_k_means == 3 ~ "Cluster 3"))

US_standardized <- standardize_data(USStates, mobility = T)

# calculate social distancing scores and join to dataset
scores <- US_standardized %>%
  mutate(social_dist_score = -1 * (retail_recreation + grocery_pharmacy + parks + transit_stations + workplaces) + residential) %>%
  select(state, date, social_dist_score)

USStates <- left_join(USStates, scores, by = c("state", "date"))

# remove unneccessary data
rm(k_means, distance, scores)

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

# pivot from long to wide format
USStates_Wide <- USStates %>%
  pivot_wider(names_from = date, values_from = c(retail_recreation:residential, confirmed_cases, cases_per_capita, rate, euclidean_dist_avg, cluster_k_means, social_dist_score)) %>%
  mutate(avg_response_time = (response_emergency + response_school + response_stay_home)/3,
         avg_dist_score = (`social_dist_score_2020-03-29` + `social_dist_score_2020-04-05` + `social_dist_score_2020-04-11`)/3)

# find change in social distancing score across time
USStates <- USStates %>%
  left_join(USStates_Wide %>%
              mutate(diff_score = `social_dist_score_2020-04-11` - `social_dist_score_2020-03-29`,
                     diff_cases_capita = `cases_per_capita_2020-04-11` - `cases_per_capita_2020-03-29`,
                     diff_rate = `rate_2020-04-11` - `rate_2020-03-29`) %>%
              select(state, diff_score, diff_cases_capita, diff_rate), by = "state")

# rearrange clusters and make clusters factors
USStates <- USStates %>%
  mutate(cluster_k_means = case_when(
    cluster_pop == 1 & cluster_k_means == 1 ~ 2,
    cluster_pop == 1 & cluster_k_means == 2 ~ 3,
    cluster_pop == 1 & cluster_k_means == 3 ~ 1,
    cluster_pop == 2 & cluster_k_means == 1 ~ 3,
    cluster_pop == 2 & cluster_k_means == 2 ~ 1,
    cluster_pop == 2 & cluster_k_means == 3 ~ 2,
    cluster_pop == 4 & cluster_k_means == 1 ~ 2,
    cluster_pop == 4 & cluster_k_means == 2 ~ 1,
    TRUE ~ as.numeric(cluster_k_means)),
    cluster_pop = factor(cluster_pop),
    cluster_k_means = factor(cluster_k_means))

# standardize mobility data
US_standardized <- standardize_data(USStates)

# write .csv file of datasets
write_csv(USStates, path = "data/USStates.csv")
write_csv(USStates_Wide, path = "data/USStates_Wide.csv")
