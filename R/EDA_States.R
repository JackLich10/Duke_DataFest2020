# EDA script for states
library(hrbrthemes)
library(tidyverse)
library(ggrepel)
library(janitor)

# read in state mobility trends
USStates <- read_csv("data/Social Distancing - States.csv") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

# read in state governmental actions and basic covid-19 data
USStateActions <- read_csv("data/Social Distancing - State Actions.csv") %>%
  set_names(tolower(make.names(names(.))))

# join dataframes and remove unnecessary data
USStates <- left_join(USStates, USStateActions, by = "state")
rm(USStateActions)

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
           TRUE ~ as.character(NA)))

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

