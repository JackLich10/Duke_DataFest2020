# source data cleaning
source("R/Data_Cleaning_Manip.R")

# Democratic governed states distance more
library(ggridges)
USStates %>%
  mutate(cluster_pop = case_when(
    cluster_pop == 1 ~ "Cluster 1: GA, HI, IN, KY, LA, MI, NH, NC, SC, TX, TN, VA, WA",
    cluster_pop == 2 ~ "Cluster 2: CA, DE, FL, IL, MD, OH, PA",
    cluster_pop == 3 ~ "Cluster 3: AL, AK, AR, CO, ID, IA, KS, ME, MN, MS, MO, MT, NE, NV, NM, ND, OK, OR, SD, UT, VT, WV, WI, WY",
    cluster_pop == 4 ~ "Cluster 4: CT, MA, NJ, NY, RI",
    TRUE ~ as.character(cluster_pop))) %>%
  ggplot() +
  stat_density_ridges(aes(x = social_dist_score, y = governor, fill = governor), 
                      alpha = 0.75, quantile_lines = T, quantiles = 2, scale = 3, color = "white") +
  facet_wrap(.~ cluster_pop) +
  scale_fill_manual(values = c("#1a4ba9", "#a90010")) +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Democratic Governed States are Distancing More",
       subtitle = "(white line shows median social distance score)",
       x = "Social Distance Score",
       y = "Governor Affiliation",
       caption = "Data courtesy of Google")

USStates %>%
  mutate(cluster_pop = case_when(
    cluster_pop == 1 ~ "Cluster 1: GA, HI, IN, KY, LA, MI, NH, NC, SC, TX, TN, VA, WA",
    cluster_pop == 2 ~ "Cluster 2: CA, DE, FL, IL, MD, OH, PA",
    cluster_pop == 3 ~ "Cluster 3: AL, AK, AR, CO, ID, IA, KS, ME, MN, MS, MO, MT, NE, NV, NM, ND, OK, OR, SD, UT, VT, WV, WI, WY",
    cluster_pop == 4 ~ "Cluster 4: CT, MA, NJ, NY, RI",
    TRUE ~ as.character(cluster_pop))) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(date), y = social_dist_score, color = governor)) +
  facet_wrap(.~ cluster_pop, scales = "free_x") +
  theme_ipsum() +
  labs(x = "Date",
       y = "Social Distancing Score",
       color = "Governor")

USStates %>%
  mutate(cluster_pop = case_when(
    cluster_pop == 1 ~ "Cluster 1: GA, HI, IN, KY, LA, MI, NH, NC, SC, TX, TN, VA, WA",
    cluster_pop == 2 ~ "Cluster 2: CA, DE, FL, IL, MD, OH, PA",
    cluster_pop == 3 ~ "Cluster 3: AL, AK, AR, CO, ID, IA, KS, ME, MN, MS, MO, MT, NE, NV, NM, ND, OK, OR, SD, UT, VT, WV, WI, WY",
    cluster_pop == 4 ~ "Cluster 4: CT, MA, NJ, NY, RI",
    TRUE ~ as.character(cluster_pop))) %>%
  ggplot() +
  geom_smooth(aes(x = date, y = social_dist_score, color = governor),
              method = "lm", se = F) +
  geom_point(aes(x = date, y = social_dist_score, color = governor)) +
  facet_wrap(.~ cluster_pop, scales = "free_x") +
  theme_ipsum() +
  labs(x = "Date",
       y = "Social Distancing Score",
       color = "Governor")

USStates %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(date), y = social_dist_score, fill = cluster_pop)) +
  facet_wrap(.~cluster_pop) +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Social Distancing by Cluster",
       x = "Date",
       y = "Social Distancing Score")

USStates %>%
  select(state, date, cases_per_capita, social_dist_score, cluster_pop) %>%
  group_by(state) %>%
  mutate(change_cases_per_capita = cases_per_capita - lag(cases_per_capita, order_by = date, default = 0),
         change_social_dist_score = social_dist_score - lag(social_dist_score, order_by = date, default = 0)) %>%
  filter(date == "2020-04-05") %>%
  ggplot() +
  geom_smooth(aes(x = change_cases_per_capita, y = change_social_dist_score),
              method = "lm", se = F) +
  geom_point(aes(x = change_cases_per_capita, y = change_social_dist_score, color = as.factor(cluster_pop))) +
  geom_text_repel(aes(x = change_cases_per_capita, y = change_social_dist_score, label = ifelse(is_outlier(change_cases_per_capita) | is_outlier(change_social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_pop, scales = "free") +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(title = "How do changes in cases lead to changes in social distancing",
       subtitle = "Changes from 3/29 to 4/5",
       y = "Change in Social Distancing Score",
       x = "Change in Cases/Capita",
       color = NULL)

USStates %>%
  select(state, date, cases_per_capita, social_dist_score, cluster_pop) %>%
  group_by(state) %>%
  mutate(change_cases_per_capita = cases_per_capita - lag(cases_per_capita, n = 2, order_by = date, default = 0),
         change_social_dist_score = social_dist_score - lag(social_dist_score, n = 2, order_by = date, default = 0)) %>%
  filter(date == "2020-04-05") %>%
  ggplot() +
  geom_smooth(aes(x = change_cases_per_capita, y = social_dist_score),
              method = "lm", se = T) +
  geom_point(aes(x = change_cases_per_capita, y = social_dist_score, color = as.factor(cluster_pop))) +
  geom_text_repel(aes(x = change_cases_per_capita, y = social_dist_score, label = ifelse(is_outlier(change_cases_per_capita) | is_outlier(social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_pop, scales = "free_x") +
  scale_color_manual(values = c("black", "#a90010", "grey", "#1a4ba9")) +
  guides(color = F) +
  theme_ipsum() +
  labs(title = "How do changes in cases lead to social distancing",
       y = "Social Distancing Score",
       x = "Change in Cases/Capita from 3/29 to 4/5")

# Changes in cases/capita affecting social distancing
USStates_Wide %>%
  mutate(diff_cases_capita = (`cases_per_capita_2020-04-11` - `cases_per_capita_2020-03-29`)/`cases_per_capita_2020-03-29`) %>%
  ggplot() +
  geom_smooth(aes(x = diff_cases_capita, y = avg_dist_score, color = governor),
              method = "lm", se = F) +
  geom_point(aes(x = diff_cases_capita, y = avg_dist_score, color = governor)) +
  geom_text_repel(aes(x = diff_cases_capita, y = avg_dist_score, label = ifelse(is_outlier(diff_cases_capita) | is_outlier(avg_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_pop) +
  scale_x_continuous(labels = scales::percent) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "States with Democratic governors are distancing more",
       y = "Average Social Distancing Score",
       x = "%Change in Cases/Capita from 3/29 to 4/11",
       color = NULL)

USStates %>%
  mutate(cluster_pop = case_when(
    cluster_pop == 1 ~ "Cluster 1: GA, HI, IN, KY, LA, MI, NH, NC, SC, TX, TN, VA, WA",
    cluster_pop == 2 ~ "Cluster 2: CA, DE, FL, IL, MD, OH, PA",
    cluster_pop == 3 ~ "Cluster 3: AL, AK, AR, CO, ID, IA, KS, ME, MN, MS, MO, MT, NE, NV, NM, ND, OK, OR, SD, UT, VT, WV, WI, WY",
    cluster_pop == 4 ~ "Cluster 4: CT, MA, NJ, NY, RI",
    TRUE ~ as.character(cluster_pop))) %>%
  ggplot() +
  geom_smooth(aes(x = cases_per_capita, y = social_dist_score, color = as.factor(date)),
              method = "lm", se = F) +
  geom_point(aes(x = cases_per_capita, y = social_dist_score, color = as.factor(date))) +
  geom_text_repel(aes(x = cases_per_capita, y = social_dist_score, label = ifelse(is_outlier(cases_per_capita) | is_outlier(social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(~ cluster_pop, scales = "free_x") +
  scale_color_manual(values = c("#a90010", "black", "#1a4ba9")) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "States with more cases distance more, but everybody is distancing less",
       y = "Social Distancing Score",
       x = "Cases/Capita",
       color = "Date")

# Changes in rate of spread affecting social distancing
USStates_Wide %>%
  mutate(cluster_pop = case_when(
    cluster_pop == 1 ~ "Cluster 1: GA, HI, IN, KY, LA, MI, NH, NC, SC, TX, TN, VA, WA",
    cluster_pop == 2 ~ "Cluster 2: CA, DE, FL, IL, MD, OH, PA",
    cluster_pop == 3 ~ "Cluster 3: AL, AK, AR, CO, ID, IA, KS, ME, MN, MS, MO, MT, NE, NV, NM, ND, OK, OR, SD, UT, VT, WV, WI, WY",
    cluster_pop == 4 ~ "Cluster 4: CT, MA, NJ, NY, RI",
    TRUE ~ as.character(cluster_pop))) %>%
  mutate(diff_rate = (`rate_2020-04-11` - `rate_2020-03-29`)/`rate_2020-03-29`,
         change_dist_score = `social_dist_score_2020-04-11` - `social_dist_score_2020-03-29`) %>%
  ggplot() +
  geom_smooth(aes(x = `social_dist_score_2020-03-29`, y = diff_rate, color = cluster_pop),
              method = "lm", se = F) +
  geom_point(aes(x = `social_dist_score_2020-03-29`, y = diff_rate, color = cluster_pop)) +
  geom_text_repel(aes(x = `social_dist_score_2020-03-29`, y = diff_rate, label = ifelse(state %in% c("New York", "Rhode Island"), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_pop, scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("black", "#a90010", "grey", "#1a4ba9")) +
  theme_ipsum() +
  theme(legend.position = "none") +
  labs(title = "States who Distanced More Initially See Greatest Reduction in Rate of Spread",
       x = "Social Distancing Score on 3/29",
       y = "%Change in Rate of Spread",
       color = NULL)
  
USStates %>%
  ggplot() +
  geom_smooth(aes(x = rate, y = social_dist_score, color = governor),
              method = "lm", se = F) +
  geom_point(aes(x = rate, y = social_dist_score, color = governor)) +
  geom_text_repel(aes(x = rate, y = social_dist_score, label = ifelse(is_outlier(rate) | is_outlier(social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ cluster_pop, scales = "free_x") +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "States with more cases are distancing more",
       y = "Social Distancing Score",
       x = "Rate of Covid-19 Spread",
       color = NULL)

# Look at how rate of week prior changes distancing of next week
USStates %>%
  mutate(cluster_pop = case_when(
    cluster_pop == 1 ~ "Cluster 1: GA, HI, IN, KY, LA, MI, NH, NC, SC, TX, TN, VA, WA",
    cluster_pop == 2 ~ "Cluster 2: CA, DE, FL, IL, MD, OH, PA",
    cluster_pop == 3 ~ "Cluster 3: AL, AK, AR, CO, ID, IA, KS, ME, MN, MS, MO, MT, NE, NV, NM, ND, OK, OR, SD, UT, VT, WV, WI, WY",
    cluster_pop == 4 ~ "Cluster 4: CT, MA, NJ, NY, RI",
    TRUE ~ as.character(cluster_pop))) %>%
  group_by(state) %>%
  mutate(distance_next_week = lead(social_dist_score, order_by = date)) %>%
  select(state, date, rate, cases_per_capita, social_dist_score, distance_next_week, cluster_pop) %>%
  filter(date != "2020-04-11") %>%
  ggplot() +
  geom_smooth(aes(x = cases_per_capita, y = distance_next_week, color = as.factor(date)),
              method = "lm") +
  geom_point(aes(x = cases_per_capita, y = distance_next_week, color = as.factor(date))) +
  facet_wrap(.~ cluster_pop, scales = "free_x") +
  scale_color_manual(values = c("#a90010", "#1a4ba9")) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "Cases/Capita Week Prior Motivates Future Social Distancing",
       x = "Cases/Capita Week Prior",
       y = "Social Distancing Score Next Week",
       color = "Date")



USStates_Wide %>%
  ggplot() +
  geom_point(aes(x = avg_response_time, y = avg_dist_score, color = governor)) +
  geom_smooth(aes(x = avg_response_time, y = avg_dist_score, color = governor),
              method = "lm", se = T)

USStates_Wide %>%
  ggplot() +
  geom_smooth(aes(x = avg_response_time, y = avg_dist_score, color = governor),
              method = "lm") +
  geom_point(aes(x = avg_response_time, y = avg_dist_score, color = governor)) +
  scale_color_manual(values = c("#1a4ba9", "#a90010")) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.9)) +
  labs(title = "Earlier Stay at Home Orders Increase Social Distancing",
       subtitle = "and Democratic governors were more quick to enact orders",
       x = "Date of Stay at Home Order",
       y = "Social Distancing Score 3/29",
       color = "Governor")


