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

# function to find Euclidean distance
find_euclidean_dist <- function(data) {
  # make a copy of data
  data_copy <- data
  
  for (i in c(nrow(data)/2, nrow(data))) {
    # normalization of mobility trends (subtracting mean, dividing by standard deviation) (must be quantitative data)
    means <- apply(data[(i-50):i, 3:8], 2, mean)
    stdevs <- apply(data[(i-50):i, 3:8], 2, sd)
    data_copy[(i-50):i, 3:8] <- as.data.frame(scale(data[(i-50):i, 3:8], means, stdevs))
  }
  
  # find euclidean distance from avg mobility trends for all states
  avg_mobility_US <- data_copy %>%
    group_by(date) %>%
    summarise(across(retail_recreation:residential, mean))
  
  euclid_dist <- tibble(state = data$state,
                        date = data$date,
                        euclidean_dist = as.numeric(rep(NA, nrow(data))))
  
  for (i in 1:nrow(data)) {
    euclid_dist[i, 3] <- as.numeric(dist(rbind(avg_mobility_US[ceiling(2*i/nrow(data)),2:7], data_copy[i,3:8])))
  }
  
  # join euclidean distance by state and remove unnecessary dataframes
  data <- left_join(data, euclid_dist, by = c("state", "date"))
  return(data)
}

USStates %>%
  group_by(state)
  
# function to detect outliers
is_outlier <- function(x) {
  return(x <= quantile(x, 0.25) - 1.5 * IQR(x) | x >= quantile(x, 0.75) + 1.5 * IQR(x))
}

USStates <- find_euclidean_dist(data = USStates)

# pivot from wide to long format
USStates_Long <- USStates %>%
  pivot_longer(cols = c("retail_recreation", "grocery_pharmacy", "parks",	
                        "transit_stations",	"workplaces",	"residential"),
               names_to = "type") %>%
  group_by(date , type) %>%
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
  facet_wrap(.~ date) +
  guides(color = F) +
  theme_ipsum(axis = "xy") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(title = "United States Mobility Trends",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       caption = "Data courtesy of Google")

# Most unique mobility states
pd <- USStates %>%
  group_by(date) %>%
  top_n(10, euclidean_dist) %>% 
  ungroup() %>%
  arrange(date, euclidean_dist) %>%
  mutate(order = row_number()) %>%
  group_by(state) %>%
  mutate(count = n()) %>%
  ungroup()

pd %>%
  ggplot() +
  geom_col(aes(y = order, x = euclidean_dist, fill = count)) +
  facet_wrap(.~ date, scales = "free") +
  scale_y_continuous(breaks = pd$order, labels = pd$state, expand = c(0, 0)) +
  guides(fill = F) +
  theme_ipsum(grid = "X") +
  theme(panel.spacing = unit(0.25, "lines")) +
  labs(title = "10 Most Unique Mobility Trends",
       subtitle = "among US states",
       x = "Standardized Euclidean Distance",
       y = NULL,
       caption = "Data courtesy of Google")

# Changes in uniqueness of mobility
USStates %>%
  group_by(state) %>%
  mutate(diff_euclid = case_when(
    date == "2020-03-29" ~ euclidean_dist - lead(euclidean_dist, order_by = date, default = 0),
    date == "2020-04-05" ~ euclidean_dist - lag(euclidean_dist, order_by = date, default = 0)),
    color = case_when(
      date == "2020-03-29" & diff_euclid > 0.95 ~ "More Average",
      date == "2020-03-29" & diff_euclid < -0.95 ~ "More Unique",
      date == "2020-04-05" & diff_euclid > 0.95 ~ "More Unique",
      date == "2020-04-05" & diff_euclid < -0.95 ~ "More Average",
      TRUE ~ "About the Same"
    )) %>%
  ggplot() +
  geom_line(aes(x = date, y = euclidean_dist, group = state, color = color, alpha = abs(diff_euclid))) +
  geom_point(aes(x = date, y = euclidean_dist, alpha = abs(diff_euclid))) +
  geom_text_repel(aes(x = date, y = euclidean_dist, label = ifelse(abs(diff_euclid) > .95, state, "")), 
                  size = 3, family = hrbrthemes::font_an) +
  scale_color_manual(values = c("grey", "blue", "red")) +
  guides(alpha = F) +
  theme_ipsum() +
  theme(legend.position = c(0.2, 0.9)) +
  labs(title = "US Changes in Mobility Trends",
       x = NULL,
       y = "Standardized Euclidean Distance",
       color = NULL)

# top 10 mobility trends for 3/29
USStates_Long %>%
  filter(date == "2020-03-29") %>%
  mutate(type = factor(type, levels = c("transit_stations", "retail_recreation", "workplaces", "grocery_pharmacy", "parks", "residential")),
         state = factor(state, levels = state[order(-euclidean_dist)])) %>%
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
       subtitle = "as of 3/29/20 (grey points correspond to average among all US states)",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       color = NULL,
       caption = "Data courtesy of Google")

# top 10 mobility trends for 4/5
USStates_Long %>%
  filter(date == "2020-04-05") %>%
  mutate(type = factor(type, levels = c("transit_stations", "retail_recreation", "workplaces", "grocery_pharmacy", "parks", "residential")),
         state = factor(state, levels = state[order(-euclidean_dist)])) %>%
  arrange(desc(euclidean_dist)) %>%
  head(60) %>%
  ggplot(aes(order = mean)) +
  geom_segment(aes(x = reorder(type, mean), xend = type, y = value/100, yend = mean/100)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey") +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = type)) +
  facet_wrap(.~ state) +
  scale_color_discrete(labels = c("Transit Stations", "Retail/Recreation", "Workplaces", "Grocery/Pharmacy", "Residential", "Parks")) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis = "y") +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(0.25, "lines"),
        legend.position = c(0.9, 0.175)) +
  labs(title = "US Covid-19 Mobility Trends",
       subtitle = "as of 4/5/20 (grey points correspond to average among all US states)",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       color = NULL,
       caption = "Data courtesy of Google")


USStates %>%
  ggplot() +
  geom_col(aes(y = reorder(state, euclidean_dist), x = euclidean_dist)) +
  theme_ipsum(grid = "x") +
  labs(title = "Most Unique Mobility Trends",
       subtitle = paste0("as of ", USStates$date),
       x = "Euclidean Distance",
       y = NULL)
  

  geom_point(aes(x = days_from_case_to_death, y = euclidean_dist))

