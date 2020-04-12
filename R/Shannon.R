library(tidyverse)
library(broom)
library(dplyr)

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

state_data_set <- state_data_set %>% 
  mutate(response_stay_home = date_of_stay_at_home_order - date_of_1st_case,
         response_school = state_mandated_school_closures - date_of_1st_case,
         response_emergency = emergency_declaration - date_of_1st_case
         ) 
# Create response time variables
state_data_set %>% 
  mutate(resp_sch = mean(response_school),
         resp_emer = mean(response_emergency),
         resp_home = mean(response_stay_home)) %>% 
  select(resp_sch, resp_home, resp_emer)

# Attempt to plot
ggplot(data = state_data_set, aes(y = response_emergency))+
  geom_boxplot()
  