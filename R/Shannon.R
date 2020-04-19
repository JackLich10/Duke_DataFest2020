# load libraries
library(tidyverse)
library(broom)
library(dplyr)
library(ggrepel)
library(hrbrthemes)
library(usmap)

# source data and helper functions (this will populate environment with data)
source("R/Data_Cleaning_Manip.R")

USStates <- read_csv("data/USStates.csv")
USStates_wide <- read_csv("data/USStates_Wide.csv")

# Linear Model Exploration ------------------------------------------------
lm_total <- lm(confirmed_cases ~ retail_recreation + date +
                    grocery_pharmacy + parks + transit_stations + workplaces + 
                    residential + population, data = USStates)
lm_step <- step(lm_total, direction = "backward")

lm_efficient <- lm(confirmed_cases ~ grocery_pharmacy + workplaces
                   + parks + retail_recreation + population, 
                   data = USStates)
tidy(lm_efficient)

glance(lm_efficient) %>% 
  pull(adj.r.squared)
#note adjusted r squared is .2578

lm_factors <- lm(confirmed_cases ~ population + date_of_1st_case + 
     date_of_1st_death + date_of_stay_at_home_order + 
     state_mandated_school_closures + emergency_declaration, 
     data = USStates )

lm_step_2 <- step(lm_factors, direction = "backward")

lm_efficient_2 <- lm(confirmed_cases ~ population + date +
                       date_of_stay_at_home_order, data = USStates)

tidy(lm_efficient_2)
glance(lm_efficient_2) %>% 
  pull(adj.r.squared)
#Note adjusted r squared = .1847

# Looking at Response Time -------------------------------------------------

# Note that 6/51 states/DC do not have stay at home orders
USStates_Wide %>%
  filter(is.na(response_stay_home)) %>%
  nrow()

# Exploratory Data Analysis -----------------------------------------------
# wide to long conversion
state_data_set_long <- USStates %>%
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
  geom_boxplot(aes(x = type_response_time, y = response_time, fill = governor)) +
  geom_text_repel(aes(x = type_response_time, y = response_time, fill = governor,
                      label = ifelse(!is.na(outlier), paste0(state, ": ", response_time, " days"), ""),
                      color = high_low), size = 3, fontface = "bold", family = hrbrthemes::font_an) +
  scale_fill_manual(values = c("#1a4ba9", "#a90010")) +
  scale_x_discrete(labels = c("State of Emergency", "School Cancellations", "Stay at Home Order")) +
  guides(color = F) +
  theme_ipsum() +
  labs(title = "US Response Times",
       x = NULL,
       y = "Response Time From Date of 1st Case")

# Closer look at NA stay at home states
USStates %>% 
  filter(is.na(response_stay_home)) %>% 
  group_by(date) %>% 
  summarise(avg_per_capita = mean(cases_per_capita),
            avg_cases = mean(confirmed_cases),
            avg_pop = mean(population))
  
  .000352/.000123
  .000871/.000288
  6644679/1639835
  7391/454
  3116/197
# States who have declared stay at home orders have approximately
# three times more corona virus per capita than those that did not
# and 4 times the population accounting for 16 times more corona cases

# Map of stay at home orders shows midwest is not responsive, and that
# Washington state failed to respond first despite being first case
plot_usmap(data = USStates, values = "response_stay_home")
  
plot_usmap(data = USStates, values = "date_of_1st_case")

first_day <- USStates %>% 
  filter(date == "2020-03-29") %>% 
  filter(state != "New York") %>% 
  filter(state != "New Jersey")
plot_usmap(data = first_day, values = "confirmed_cases")

# response times
bad_response <- USStates %>% 
  filter(response_stay_home > 47) %>% 
  plyr::colwise(mean)(.) 

USStates %>% 
  filter(response_stay_home < 47) %>% 
  plyr::colwise(mean)(.) %>% 
  view()
#States with the worst reponse times were those hit with corona first. 
#They also seem to have better social distancing practices. They are very 
#populated but also fairly large. The average dates of the responses are the
# same for both groups, suggesting that those hit first did not know how to 
# handle the novel virus at the time but have since adopted strong social 
# distancing stances

USStates %>%
  arrange(date_of_1st_case) %>% 
  view()
# West Virginia was last state to get covid, best response

USStates %>%
  arrange(desc(pop_density)) %>% 
  view()

USStates %>% 
  filter(state != "District of Columbia") %>% 
  ggplot(aes(x = pop_density, y = confirmed_cases))+
  geom_point()

USStates %>% 
  filter(state != "District of Columbia") %>% 
  group_by(state, pop_density) %>% 
  summarise(spread = confirmed_cases[2]-confirmed_cases[1]) %>% 
  ggplot(aes(x = pop_density, y = spread))+
  geom_point()
#spread vs. pop-density

USStates %>% 
  filter(state != "District of Columbia") %>%
  ggplot(aes(x = pop_density, y = cases_per_capita))+
  geom_point()
#pop density vs. cases per capita

#Mapping clusters on map
plot_usmap(data = USStates, values = "cluster_k_means")

#Cases vs. Social Mobility
USStates %>%
  filter(date == "2020-03-29") %>% 
  ggplot() +
  geom_smooth(aes(x = confirmed_cases, y = social_dist_score, 
                  color = cluster_k_means), method = "lm", se = T) +
  geom_point(aes(x = confirmed_cases, y = social_dist_score, color = cluster_k_means)) +
  geom_text_repel(aes(x = confirmed_cases, y = social_dist_score, label = ifelse(social_dist_score > 8 | confirmed_cases > 10000, state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_k_means, scales = "free") +
  guides(color = F) +
  theme_ipsum() +
  labs(title = "How do changes in cases lead to changes in mobility",
       subtitle = "Based on mobility data from 3/29",
       y = "Change in Standardized Social Mobility",
       x = "Cases on 3/29")

cluster_1_means <- USStates %>% 
  filter(cluster_k_means == "Cluster 1") %>% 
  plyr::colwise(mean)(.)

cluster_2_means <- USStates %>% 
  filter(cluster_k_means == "Cluster 2") %>% 
  plyr::colwise(mean)(.)

cluster_3_means <- USStates %>% 
  filter(cluster_k_means == "Cluster 3") %>% 
  plyr::colwise(mean)(.)

#Cluster 1 has the most cases, most cases/capita, best social distancing score,
# and worst response times

plot_usmap(data = USStates, values = "social_dist_score", labels = FALSE) +
  scale_fill_steps2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0) +
  theme(legend.position = "bottom") +
  labs(fill = "Social Distancing Score")

# lm to predict avg_dist_score for each state
to_model <- USStates_Wide %>%
  select(c(avg_dist_score, governor, days_from_case_to_death:response_emergency, population, `confirmed_cases_2020-03-29`, `confirmed_cases_2020-04-05`))

lm_everythang <- lm(avg_dist_score ~ ., data = to_model)

summary(lm_everythang)

lm_step_everythang <- step(lm_everythang, direction = "backward")

summary(lm_step_everythang)

# Cleaning Case Data

state_confirmed <- read_csv("data/covid_confirmed_usafacts.csv")

state_confirmed %>% 
  group_by(State) %>% 
  summarise("4/11/20" = sum(`4/11/2020`)) %>% 
  view()

## Exploring political relationships

USStates_wide %>%
  mutate(score_bin = if_else(avg_dist_score >= 0, "good", "bad")) %>% 
  group_by(governor, score_bin) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

USStates_wide <- USStates_wide %>% 
  mutate(score_bin = if_else(avg_dist_score >= 0, "good", "bad"))

USStates_wide %>% 
  ggplot(aes(fill = governor, x = score_bin, y = avg_response_time)) + 
  geom_bar(position="fill", stat="identity")

# Reactive states
USStates_wide %>% 
  group_by(avg_dist_score) %>% 
  arrange(desc(avg_dist_score)) %>% 
  select(state, avg_dist_score) %>% 
  view()

USStates_wide %>% 
ggplot(aes(x = state, y = state_mandated_school_closures))+
  geom_point()
         