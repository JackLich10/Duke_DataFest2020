# source data cleaning
source("R/Data_Cleaning_Manip.R")

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
  top_n(10, euclidean_dist_avg) %>% 
  ungroup() %>%
  arrange(date, euclidean_dist_avg) %>%
  mutate(order = row_number()) %>%
  group_by(state) %>%
  mutate(count = n()) %>%
  ungroup()

pd %>%
  ggplot() +
  geom_col(aes(y = order, x = euclidean_dist_avg, fill = count)) +
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
    date == "2020-03-29" ~ euclidean_dist_avg - lead(euclidean_dist_avg, order_by = date, default = 0),
    date == "2020-04-05" ~ euclidean_dist_avg - lag(euclidean_dist_avg, order_by = date, default = 0)),
    color = case_when(
      date == "2020-03-29" & diff_euclid > 0.95 ~ "More Average",
      date == "2020-03-29" & diff_euclid < -0.95 ~ "More Unique",
      date == "2020-04-05" & diff_euclid > 0.95 ~ "More Unique",
      date == "2020-04-05" & diff_euclid < -0.95 ~ "More Average",
      TRUE ~ "About the Same")) %>%
  ggplot() +
  geom_line(aes(x = date, y = euclidean_dist_avg, group = state, color = color, alpha = abs(diff_euclid))) +
  geom_point(aes(x = date, y = euclidean_dist_avg, alpha = abs(diff_euclid))) +
  geom_text_repel(aes(x = date, y = euclidean_dist_avg, label = ifelse(abs(diff_euclid) > .95, state, "")), 
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
         state = factor(state, levels = state[order(-euclidean_dist_avg)])) %>%
  arrange(desc(euclidean_dist_avg)) %>%
  head(60) %>%
  ggplot(aes(order = mean)) +
  geom_segment(aes(x = reorder(type, mean), xend = type, y = value/100, yend = mean/100)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey", size = 2.5) +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = type), size = 3) +
  facet_wrap(.~ state) +
  scale_color_discrete(labels = c("Transit Stations", "Retail/Recreation", "Workplaces", "Grocery/Pharmacy", "Parks", "Residential")) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis = "y") +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),
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
         state = factor(state, levels = state[order(-euclidean_dist_avg)])) %>%
  arrange(desc(euclidean_dist_avg)) %>%
  head(60) %>%
  ggplot(aes(order = mean)) +
  geom_segment(aes(x = reorder(type, mean), xend = type, y = value/100, yend = mean/100)) +
  geom_point(aes(x = reorder(type, mean), y = mean/100), color = "grey", size = 2.5) +
  geom_point(aes(x = reorder(type, mean), y = value/100, color = type), size = 3) +
  facet_wrap(.~ state) +
  scale_color_discrete(labels = c("Transit Stations", "Retail/Recreation", "Workplaces", "Grocery/Pharmacy", "Residential", "Parks")) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum(axis = "y") +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.position = c(0.9, 0.175)) +
  labs(title = "US Covid-19 Mobility Trends",
       subtitle = "as of 4/5/20 (grey points correspond to average among all US states)",
       x = NULL,
       y = "%Change in Mobility Compared to Baseline",
       color = NULL,
       caption = "Data courtesy of Google")

USStates %>%
  group_by(state) %>%
  mutate(change_cases_per_capita = cases_per_capita - lag(cases_per_capita, order_by = date, default = 0)) %>%
  filter(date == "2020-03-29") %>%
  ggplot() +
  geom_smooth(aes(x = change_cases_per_capita, y = euclidean_dist_change, color = as.factor(cluster_k_means)),
              method = "lm", se = T) +
  geom_point(aes(x = change_cases_per_capita, y = euclidean_dist_change, color = as.factor(cluster_k_means))) +
  geom_text_repel(aes(x = change_cases_per_capita, y = euclidean_dist_change, label = ifelse(euclidean_dist_change > 2.5 | change_cases_per_capita > 0.002, state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_k_means, scales = "free") +
  guides(color = F) +
  theme_ipsum() +
  labs(title = "How do changes in cases lead to changes in mobility",
       subtitle = "Changes from 3/29 to 4/5",
       y = "Change in Standardized Euclidean Distance",
       x = "Change in Cases/Capita")

USStates %>%
  pivot_longer(cols = c(retail_recreation:residential), names_to = "type") %>%
  ggplot() +
  geom_boxplot(aes(x = cluster_k_means, y = value/100, fill = as.factor(cluster_k_means))) +
  facet_wrap(.~ type, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Breakdown of Clusters",
       x = "Cluster",
       y = "%Change in Mobility Compared to Baseline")

USStates <- USStates %>%
  mutate(days_from_case_to_death = as.numeric(days_from_case_to_death))

USStates %>%
  pivot_longer(cols = c(cases_per_capita, days_from_case_to_death), names_to = "type") %>%
  mutate(type = case_when(
    type == "cases_per_capita" ~ "Cases/Capita",
    type == "days_from_case_to_death" ~ "# of Days from 1st Case to 1st Death")) %>%
  ggplot() +
  geom_boxplot(aes(x = cluster_k_means, y = value, fill = as.factor(cluster_k_means))) +
  facet_wrap(.~ type, scales = "free") +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Breakdown of Clusters",
       x = "Cluster",
       y = "%Change in Mobility Compared to Baseline")

