library(tidyverse)
library(broom)
library(dplyr)

source("Helpers.R")

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
<<<<<<< HEAD
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

# basic EDA boxplot for mobility trends
USStates_Long %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(response_type, -value), y = value/100)) +
  geom_text_repel(aes(x = reorder(type, -value), y = value/100,
                      label = ifelse(!is.na(outlier), paste0(state, ": ", outlier, "%"), ""),
                      color = high_low), size = 3, fontface = "bold", family = hrbrthemes::font_an) +
  scale_x_discrete(labels = c("Residential", "Parks", "Grocery/Pharmacy", "Workplaces", "Retail/Recreation", "Transit Stations")) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("red", "blue")) +
  facet_wrap(.~ date) +
  guides(color = F) +
  theme_ipsum(axis = "xy") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(title = "United States Mobility Trends",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       caption = "Data courtesy of Google")
  
=======
         response_emergency = emergency_declaration - date_of_1st_case) 

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

>>>>>>> 26107613c45f267576dfd4f8e9c3e4d66e099b8b
