# load libraries
library(hrbrthemes)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(janitor)

# Data Cleaning/Manipulation ----------------------------------------------

# read in state mobility trends
USStates <- read_csv("data/Social Distancing - States.csv")

# read in state governmental actions and basic covid-19 data
USStateActions <- read_csv("data/Social Distancing - State Actions.csv") %>%
  janitor::clean_names()

# join dataframes and remove unnecessary data
USStates <- left_join(USStates, USStateActions, by = "state") %>%
  mutate_at(vars(starts_with("date"), state_mandated_school_closures, emergency_declaration), mdy)
rm(USStateActions)

# find days between 1st case and 1st death
USStates <- USStates %>%
  mutate(days_from_case_to_death = date_of_1st_death - date_of_1st_case)

# normalization of mobility trends (subtracting mean, dividing by standard deviation) (must be quantitative data)
means <- apply(USStates[, 3:8], 2, mean)
stdevs <- apply(USStates[, 3:8], 2, sd)
standardized <- as.data.frame(scale(USStates[, 3:8], means, stdevs))

# find euclidean distance from avg mobility trends for all states
avg_mobility_US <- standardized %>%
  summarise(across(retail_recreation:residential, mean))

euclid_dist <- tibble(state = USStates$state,
                      euclidean_dist = as.numeric(rep(NA, nrow(USStates))))

for (i in 1:nrow(USStates)) {
  euclid_dist[i, 2] <- as.numeric(dist(rbind(avg_mobility_US, standardized[i,1:6])))
}

# join euclidean distance by state and remove unnecessary dataframes
USStates <- left_join(USStates, euclid_dist, by = "state")
rm(avg_mobility_US, euclid_dist, i, means, stdevs, standardized)

# function to detect outliers
is_outlier <- function(x) {
  return(x <= quantile(x, 0.25) - 1.5 * IQR(x) | x >= quantile(x, 0.75) + 1.5 * IQR(x))
}

# pivot from wide to long format
USStates_Long <- USStates %>%
  pivot_longer(cols = c("retail_recreation", "grocery_pharmacy", "parks",	
                        "transit_stations",	"workplaces",	"residential"),
               names_to = "type") %>%
  group_by(type) %>%
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
  geom_boxplot(aes(x = reorder(type, -value), y = value/100)) +
  geom_text_repel(aes(x = reorder(type, -value), y = value/100,
                      label = ifelse(!is.na(outlier), paste0(state, ": ", outlier, "%"), ""),
                      color = high_low), size = 3, fontface = "bold", family = hrbrthemes::font_an) +
  scale_x_discrete(labels = c("Residential", "Parks", "Grocery/Pharmacy", "Workplaces", "Retail/Recreation", "Transit Stations")) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("red", "blue")) +
  theme_ipsum() +
  guides(color = F) +
  labs(title = "United States Mobility Trends",
       subtitle = paste0("as of ", USStates$date),
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       caption = "Data courtesy of Google")

USStates %>%
  arrange(desc(euclidean_dist)) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(y = reorder(state, euclidean_dist), x = euclidean_dist, fill = euclidean_dist)) +
  theme_ipsum(grid = "X") +
  guides(fill = F) +
  labs(title = "20 Most Unique Mobility Trends",
       subtitle = "among US states",
       x = "Standardized Euclidean Distance",
       y = NULL)

USStates_Long %>%
  mutate(type = factor(type, levels = c("transit_stations", "retail_recreation", "workplaces", "grocery_pharmacy", "parks", "residential"))) %>%
  arrange(desc(euclidean_dist)) %>%
  head(60) %>%
  ggplot(aes(order = mean)) +
  geom_segment(aes(x = reorder(type, mean), xend = type, y = value/100, yend = mean/100)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey") +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = type)) +
  facet_wrap(.~ state) +
  scale_color_discrete(labels = c("Transit Stations", "Retail/Recreation", "Workplaces", "Grocery/Pharmacy", "Parks", "Residential")) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis = "y") +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(0.25, "lines"),
        legend.position = c(0.9, 0.175)) +
  labs(title = "US Covid-19 Mobility Trends",
       subtitle = paste0("as of ", USStates$date, " (grey points correspond to average among all US states)"),
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       color = NULL,
       caption = "Data courtesy of Google")
