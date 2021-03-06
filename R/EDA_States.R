# source data cleaning
source("R/Data_Cleaning_Manip.R")

# Exploratory Data Analysis -----------------------------------------------
# basic EDA boxplot for mobility trends
USStates_Long %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(type, -value), y = value/100)) +
  geom_text_repel(aes(x = reorder(type, -value), y = value/100,
                      label = ifelse(type != "residential" & !is.na(outlier), paste0(state, ": ", outlier, "%"), ""),
                      color = high_low), size = 3, fontface = "bold", family = hrbrthemes::font_an) +
  scale_x_discrete(labels = c("Residential", "Parks", "Grocery/Pharmacy", "Workplaces", "Retail/Recreation", "Transit Stations")) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#a90010", "#1a4ba9")) +
  facet_wrap(.~ date) +
  guides(color = F) +
  theme_ipsum(axis = "xy") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(title = "United States Mobility Trends",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       caption = "Data from Google Mobility Reports")

# Changes in mobility
USStates %>%
  mutate(color = case_when(
      diff_score > 1.5 ~ "More Distancing",
      diff_score < -1.5 ~ "Less Distancing",
      TRUE ~ "About the Same")) %>%
  ggplot() +
  geom_smooth(aes(x = date, y = social_dist_score),
              method = "lm") +
  geom_line(aes(x = date, y = social_dist_score, group = state, color = color, alpha = 0.7)) +
  geom_point(aes(x = date, y = social_dist_score, alpha = 0.8)) +
  geom_text_repel(aes(x = date, y = social_dist_score, label = ifelse(date == "2020-03-29", state, "")), 
                  size = 3, family = hrbrthemes::font_an) +
  scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") +
  scale_color_manual(values = c("grey", "red", "blue")) +
  facet_wrap(. ~ cluster_pop) +
  guides(alpha = F) +
  theme_ipsum() +
  theme(legend.position = "top") +
  labs(title = "Changes in Social Distancing Behaviors",
       subtitle = "among US states from 3/29 through 4/11",
       x = "Date",
       y = "Social Distancing Score",
       color = NULL,
       caption = "Data from Google Mobility Reports")
              
# top and bottom 5 states in social distancing
USStates_Avg <- USStates %>%
  group_by(state) %>%
  mutate(avg_dist_score = mean(social_dist_score),
         avg_retail_rec = mean(retail_recreation),
         avg_grocery_phar = mean(grocery_pharmacy),
         avg_parks = mean(parks),
         avg_transit = mean(transit_stations),
         avg_workplaces = mean(workplaces),
         avg_residential = mean(residential)) %>%
  pivot_longer(cols = c("avg_retail_rec", "avg_grocery_phar", "avg_parks",	
                        "avg_transit",	"avg_workplaces",	"avg_residential"),
               names_to = "type") %>%
  group_by(type) %>%
  mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA)),
         high_low = case_when(
           !is.na(outlier) & outlier > mean(value) ~ "High",
           !is.na(outlier) & outlier < mean(value) ~ "Low",
           TRUE ~ as.character(NA)),
         mean = mean(value)) %>%
  filter(date == "2020-03-29")

USStates_Avg %>%
  mutate(type = factor(type, levels = c("avg_transit", "avg_retail_rec", "avg_workplaces", "avg_grocery_phar", "avg_parks", "avg_residential")),
         state = factor(state, levels = state[order(-avg_dist_score)])) %>%
  arrange(desc(avg_dist_score)) %>%
  head(30) %>%
  ggplot(aes(order = mean)) +
  geom_segment(aes(x = reorder(type, mean), xend = type, y = value/100, yend = mean/100)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey", size = 2) +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = type), size = 4) +
  geom_text(aes(x = 1.5, y = ifelse(state == "Hawaii", 0.05, 0), label = ifelse(state == "Hawaii", paste("Distancing Score: ", round(avg_dist_score, 2), sep = "\n"), round(avg_dist_score, 2))), 
            family = hrbrthemes::font_an, size = 3.5, fontface = "bold") +
  facet_wrap(.~ state) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(labels = c("Transit Stations", "Retail/Recreation", "Workplaces", "Grocery/Pharmacy", "Parks", "Residential"),
                     values = c("black", "#a90010", "#1a4ba9", "#a9551a", "#a9a100", "#4400a9")) +
  theme_ipsum() +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.position = "none",
        legend.text = element_text(size = 12)) +
  labs(title = "Leading 5 States in Social Distancing",
       subtitle = "(grey points correspond to average among all US states)",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       color = NULL)

USStates_Avg %>%
  mutate(type = factor(type, levels = c("avg_transit", "avg_retail_rec", "avg_workplaces", "avg_grocery_phar", "avg_parks", "avg_residential")),
         state = factor(state, levels = state[order(avg_dist_score)])) %>%
  arrange(avg_dist_score) %>%
  head(30) %>%
  ggplot(aes(order = mean)) +
  geom_segment(aes(x = reorder(type, mean), xend = type, y = value/100, yend = mean/100)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey", size = 2) +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = type), size = 4) +
  geom_text(aes(x = 1.5, y = ifelse(state == "Nebraska", 0.65, 0.6), label = ifelse(state == "Nebraska", paste("Distancing Score: ", round(avg_dist_score, 2), sep = "\n"), round(avg_dist_score, 2))), 
            family = hrbrthemes::font_an, size = 3.5, fontface = "bold") +
  facet_wrap(.~ state) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(labels = c("Transit Stations", "Retail/Recreation", "Workplaces", "Grocery/Pharmacy", "Parks", "Residential"),
                     values = c("black", "#a90010", "#1a4ba9", "#a9551a", "#a9a100", "#4400a9")) +
  theme_ipsum() +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.position = c(0.9, 0.175),
        legend.text = element_text(size = 12)) +
  labs(title = "Bottom 5 States in Social Distancing",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       color = NULL,
       caption = "Data from Google Mobility Reports")

# closer look at clusters
USStates %>%
  pivot_longer(cols = c(retail_recreation:residential), names_to = "type") %>%
  ggplot() +
  geom_boxplot(aes(x = cluster_pop, y = value/100)) +
  facet_wrap(.~ type, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Breakdown of Clusters",
       x = "Cluster",
       y = "%Change in Mobility Compared to Baseline")

USStates_Wide %>%
  mutate(avg_case_capita = (`cases_per_capita_2020-03-29` + `cases_per_capita_2020-04-05` + `cases_per_capita_2020-04-11`)/3,
         avg_rate = (`rate_2020-03-29` + `rate_2020-04-05` + `rate_2020-04-11`)/3) %>%
  pivot_longer(cols = c(avg_case_capita, avg_rate, days_from_case_to_death), names_to = "type") %>%
  mutate(type = case_when(
    type == "avg_case_capita" ~ "Avg. Cases/Capita",
    type == "avg_rate" ~ "Avg. Rate of Spread",
    type == "days_from_case_to_death" ~ "Days from 1st Case to 1st Death")) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(cluster_pop), y = value, fill = as.factor(cluster_pop))) +
  facet_wrap(.~ type, scales = "free") +
  scale_fill_manual(values = c("white", "#a90010", "grey", "#1a4ba9")) +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Breakdown of Clusters",
       x = "Cluster",
       y = NULL)

USStates_Wide %>%
  pivot_longer(cols = c(response_stay_home, response_school, response_emergency), names_to = "type") %>%
  mutate(type = case_when(
    type == "response_stay_home" ~ "Stay Home Order",
    type == "response_school" ~ "School Cancellations",
    type == "response_emergency" ~ "State of Emergency")) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(cluster_pop), y = value, fill = as.factor(cluster_pop))) +
  facet_wrap(.~ type, scales = "free") +
  scale_fill_manual(values = c("white", "#a90010", "grey", "#1a4ba9")) +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Breakdown of Clusters",
       x = "Cluster",
       y = "Days From 1st Confirmed Case to Declaration")

