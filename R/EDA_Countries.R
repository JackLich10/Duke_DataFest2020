# EDA script
library(hrbrthemes)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(janitor)

Countries <- read_csv("data/Social Distancing - Countries.csv") %>%
  mutate(date = mdy(date))

Countries_Long <- Countries %>%
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

Countries_Long %>%
  mutate(type = factor(type, levels = c("retail_recreation", "transit_stations", "grocery_pharmacy", "parks", "workplaces", "residential"))) %>%
  ggplot() +
  geom_segment(aes(x = reorder(type, mean), xend = type, y = value/100, yend = mean/100)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey") +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = type)) +
  facet_wrap(.~ country) +
  scale_color_discrete(labels = c("Retail/Recreation", "Transit Stations", "Grocery/Pharmacy", "Parks",  "Workplaces", "Residential")) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis = "y") +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(0.25, "lines"),
        legend.position = c(0.9, 0.175)) +
  labs(title = "International Covid-19 Mobility Trends",
       subtitle = paste0("as of ", Countries$date, " (grey points correspond to average among 10 countries)"),
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       color = NULL,
       caption = "Data courtesy of Google")

Countries_Long %>%
  ggplot() +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = country)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey", size = 3) +
  scale_x_discrete(labels = c("Retail/Recreation", "Transit Stations", "Grocery/Pharmacy", "Parks", "Workplaces", "Residential")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Covid-19 Mobility Trends",
       subtitle = paste0("as of ", Countries$date),
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       caption = "Data courtesy of Google")

  
