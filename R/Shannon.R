library(tidyverse)
library(broom)
library(dplyr)
library(ggrepel)
library(hrbrthemes)

source("R/Helpers.R")

source("R/Data_Cleaning_Manip.R")
#read in data 
state_data_set <- read_csv("data/USStates.csv")

# Exploratory Data Analysis -----------------------------------------------

# Linear Model Exploration ------------------------------------------------
lm_total <- lm(confirmed_cases_through_date ~ retail_recreation + date +
                    grocery_pharmacy + parks + transit_stations + workplaces + 
                    residential + population, data = state_data_set)
lm_step <- step(lm_total, direction = "backward")

lm_efficient <- lm(confirmed_cases_through_date ~ grocery_pharmacy + workplaces
                   + parks + retail_recreation + population, 
                   data = state_data_set)
tidy(lm_efficient)

glance(lm_efficient) %>% 
  pull(adj.r.squared)
#note adjusted r squared is only .2578

lm_factors <- lm(confirmed_cases_through_date ~ population + date_of_1st_case + 
     date_of_1st_death + date_of_stay_at_home_order + 
     state_mandated_school_closures + emergency_declaration, 
     data = state_data_set )

lm_step_2 <- step(lm_factors, direction = "backward")

lm_efficient_2 <- lm(confirmed_cases_through_date ~ population + date +
                       date_of_stay_at_home_order, data = state_data_set)

tidy(lm_efficient_2)
glance(lm_efficient_2) %>% 
  pull(adj.r.squared)
#Note adjusted r squared sucks = .1723

# Looking at Response Time -------------------------------------------------
# Create response time variables
state_data_set <- state_data_set %>% 
  mutate(response_stay_home = date_of_stay_at_home_order - date_of_1st_case,
         response_school = state_mandated_school_closures - date_of_1st_case,
         response_emergency = emergency_declaration - date_of_1st_case
         ) 
# Note that 6/51 states/DC do not have stay at home orders
state_data_set %>% 
  filter(response_stay_home != is.na(response_stay_home)) %>% 
  select(response_stay_home)

# Attempt to plot
# pivot from wide to long format
state_data_set_long <- state_data_set %>%
  pivot_longer(cols = c("response_stay_home", "response_school", 
                        "response_emergency")
               names_to = "response_type") %>%
  group_by(date, response_type) %>%
  mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA)),
         high_low = case_when(
           !is.na(outlier) & outlier > mean(value) ~ "High",
           !is.na(outlier) & outlier < mean(value) ~ "Low",
           TRUE ~ as.character(NA)),
         mean = mean(value))

# Exploratory Data Analysis -----------------------------------------------

# wide to long conversion
state_data_set_long <- state_data_set %>%
  filter(date == "2020-03-29") %>%
  pivot_longer(cols = c(response_stay_home, response_school, response_emergency), 
               names_prefix = "response_",
               names_to = "type_response_time",
               values_to = "response_time") %>%
  group_by(type_response_time) %>%
  mutate(outlier = ifelse(is_outlier(response_time), response_time, as.numeric(NA)),
         high_low = case_when(
           !is.na(outlier) & outlier > mean(response_time, na.rm = T) ~ "High",
           !is.na(outlier) & outlier < mean(response_time, na.rm = T) ~ "Low",
           TRUE ~ as.character(NA)),
         mean = mean(response_time, na.rm = T))

# Attempt to plot
state_data_set_long %>%
  ggplot() +
  geom_boxplot(aes(x = type_response_time, y = response_time)) +
  geom_text_repel(aes(x = type_response_time, y = response_time,
                      label = ifelse(!is.na(outlier), paste0(state, ": ", response_time, " days"), ""),
                      color = high_low), size = 3, fontface = "bold", family = hrbrthemes::font_an) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_discrete(labels = c("State of Emergency", "School Cancellations", "Stay at Home Order")) +
  guides(color = F) +
  theme_ipsum() +
  labs(title = "US Response Times",
       x = NULL,
       y = "Response Time From Date of 1st Case")

# Closer look at NA stay at home states

state_data_set %>% 
  filter(is.na(response_stay_home)) %>% 
  group_by(date) %>% 
  summarise(avg_per_capita = mean(cases_per_capita),
            avg_cases = mean(confirmed_cases_through_date),
            avg_pop = mean(population))

state_data_set %>% 
  filter(response_stay_home != is.na(response_stay_home)) %>% 
  group_by(date) %>% 
  summarise(avg_per_capita = mean(cases_per_capita),
            avg_cases = mean(confirmed_cases_through_date),
            avg_pop = mean(population))
  
  .000352/.000123
  .000871/.000288
  6644679/1639835
  7391/454
  3116/197
# States who have declared stay at home orders have approximately
# three times more corona virus per capita than those that did not
# and 4 times the population accounting for 16 times more corona cases

#Map of stay at home orders shows midwest is not responsive, and that
#Washington state failed to respond first despite being firse case
plot_usmap(data = state_data_set, values = "date_of_stay_at_home_order")
  
plot_usmap(data = state_data_set, values = "date_of_1st_case")

first_day <- state_data_set %>% 
  filter(date == "2020-03-29") %>% 
  filter(state != "New York") %>% 
  filter(state != "New Jersey")
plot_usmap(data = first_day, values = "confirmed_cases_through_date")

# Read in Data 

state_area <- read_csv("data/Social Distancing - Area.csv")

state_data_set <- left_join(state_data_set, state_area, by = "state")

state_data_set <- state_data_set %>% 
  mutate(pop_density = population/area)

bad_response <- state_data_set %>% 
  filter(response_stay_home > 47) %>% 
  plyr::colwise(mean)(.) 

state_data_set %>% 
  filter(response_stay_home < 47) %>% 
  plyr::colwise(mean)(.) %>% 
  view()
#States with the worst reponse times were those hit with corona first. 
#They also seem to have better social distancing practices. They are very 
#populated but also fairly large. The average dates of the responses are the
# same for both groups, suggesting that those hit first did not know how to 
# handle the novel virus at the time but have since adopted strong social 
# distancing stances

state_data_set %>% 
  arrange(date_of_1st_case) %>% 
  view()
#West Virginia was last state to get covid, best response

state_data_set %>% 
  arrange(desc(pop_density)) %>% 
  view()

state_data_set %>% 
  filter(state != "District of Columbia") %>% 
  ggplot(aes(x = pop_density, y = confirmed_cases_through_date))+
  geom_point()

state_data_set %>% 
  filter(state != "District of Columbia") %>% 
  group_by(state, pop_density) %>% 
  summarise(spread = confirmed_cases_through_date[2]-confirmed_cases_through_date[1]) %>% 
  ggplot(aes(x = pop_density, y = spread))+
  geom_point()
#spread vs. pop-density

state_data_set %>% 
  filter(state != "District of Columbia") %>%
  ggplot(aes(x = pop_density, y = cases_per_capita))+
  geom_point()
#pop density vs. cases per capita

#Mapping clusters on map
plot_usmap(data = state_data_set, values = "cluster_k_means")

#Cases vs. Social Mobility
state_data_set %>%
  filter(date == "2020-03-29") %>% 
  ggplot() +
  geom_smooth(aes(x = confirmed_cases_through_date, y = social_dist_score, 
                  color = as.factor(cluster_k_means)), method = "lm", se = T) +
  geom_point(aes(x = confirmed_cases_through_date, y = social_dist_score, color = as.factor(cluster_k_means))) +
  geom_text_repel(aes(x = confirmed_cases_through_date, y = social_dist_score, label = ifelse(social_dist_score > 8 | confirmed_cases_through_date > 10000, state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_k_means, scales = "free") +
  guides(color = F) +
  theme_ipsum() +
  labs(title = "How do changes in cases lead to changes in mobility",
       subtitle = "Based on mobility data from 3/29",
       y = "Change in Standardized Social Mobility",
       x = "Cases on 3/29")

cluster_1_means <- state_data_set %>% 
  filter(cluster_k_means == "Cluster 1") %>% 
  plyr::colwise(mean)(.)

cluster_2_means <- state_data_set %>% 
  filter(cluster_k_means == "Cluster 2") %>% 
  plyr::colwise(mean)(.)

cluster_3_means <- state_data_set %>% 
  filter(cluster_k_means == "Cluster 3") %>% 
  plyr::colwise(mean)(.)

#Cluster 1 has the most cases, most cases/capita, best social distancing score,
# and worst response times
