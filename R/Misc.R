# source data cleaning
source("R/Data_Cleaning_Manip.R")

USStates %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(date), y = social_dist_score, color = as.factor(cluster_k_means))) +
  facet_wrap(.~ cluster_k_means)

USStates %>%
  select(state, date, cases_per_capita, social_dist_score, cluster_k_means) %>%
  group_by(state) %>%
  mutate(change_cases_per_capita = cases_per_capita - lag(cases_per_capita, order_by = date, default = 0),
         change_social_dist_score = social_dist_score - lag(social_dist_score, order_by = date, default = 0)) %>%
  filter(date == "2020-04-05") %>%
  ggplot() +
  geom_smooth(aes(x = change_cases_per_capita, y = change_social_dist_score),
              method = "lm", se = T) +
  geom_point(aes(x = change_cases_per_capita, y = change_social_dist_score, color = as.factor(cluster_k_means))) +
  geom_text_repel(aes(x = change_cases_per_capita, y = change_social_dist_score, label = ifelse(is_outlier(change_cases_per_capita) | is_outlier(change_social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(title = "How do changes in cases lead to changes in social distancing",
       subtitle = "Changes from 3/29 to 4/5",
       y = "Change in Social Distancing Score",
       x = "Change in Cases/Capita",
       color = "Cluster")

USStates %>%
  select(state, date, cases_per_capita, social_dist_score, cluster_k_means) %>%
  group_by(state) %>%
  mutate(change_cases_per_capita = cases_per_capita - lag(cases_per_capita, order_by = date, default = 0),
         change_social_dist_score = social_dist_score - lag(social_dist_score, order_by = date, default = 0)) %>%
  filter(date == "2020-04-05") %>%
  ggplot() +
  geom_smooth(aes(x = change_cases_per_capita, y = social_dist_score),
              method = "lm", se = T) +
  geom_point(aes(x = change_cases_per_capita, y = social_dist_score, color = as.factor(cluster_k_means))) +
  geom_text_repel(aes(x = change_cases_per_capita, y = social_dist_score, label = ifelse(is_outlier(change_cases_per_capita) | is_outlier(social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_k_means, scales = "free_x") +
  guides(color = F) +
  theme_ipsum() +
  labs(title = "How do changes in cases lead to social distancing",
       y = "Social Distancing Score",
       x = "Change in Cases/Capita from 3/29 to 4/5")

USStates %>%
  mutate(cluster_k_means = case_when(
    cluster_k_means == 1 ~ "Cluster 1",
    cluster_k_means == 2 ~ "Cluster 2",
    cluster_k_means == 3 ~ "Cluster 3")) %>%
  ggplot() +
  geom_smooth(aes(x = cases_per_capita, y = social_dist_score),
              method = "lm", se = T) +
  geom_point(aes(x = cases_per_capita, y = social_dist_score, color = as.factor(cluster_k_means))) +
  geom_text_repel(aes(x = cases_per_capita, y = social_dist_score, label = ifelse(is_outlier(cases_per_capita) | is_outlier(social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(as.factor(date) ~ as.factor(cluster_k_means), scales = "free") +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(title = "States with more cases are distancing more",
       y = "Social Distancing Score",
       x = "Cases/Capita",
       color = "Cluster")




