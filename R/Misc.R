# source data cleaning
source("R/Data_Cleaning_Manip.R")

a<- USStates_Wide %>%
  count(cluster_pop, state)

USStates %>%
  mutate(cluster_pop = case_when(
    cluster_pop == 1 ~ "Cluster 1: GA, HI, IN, KY, LA, MI, NH, NC, SC, TX, VA, TN, WA",
    cluster_pop == 2 ~ "Cluster 2: CA, DE, FL, IL, MD, PA, OH",
    cluster_pop == 3 ~ "Cluster 3: AL, AK, AR, CO, ID, IA, KS, ME, MN, MS, MO, MT, NE, NV, NM, ND, OK, OR, SD, UT, VT, WV, WI, WY",
    cluster_pop == 4 ~ "Cluster 4: NY, NJ, RI, MA, CT",
    TRUE ~ as.character(cluster_pop))) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(date), y = social_dist_score, color = governor)) +
  facet_wrap(.~ cluster_pop, scales = "free_x") +
  theme_ipsum() +
  labs(x = "Date",
       y = "Social Distancing Score",
       color = "Governor")

USStates %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(date), y = social_dist_score, fill = cluster_k_means)) +
  facet_wrap(.~ cluster_k_means) +
  guides(fill = F) +
  theme_ipsum() +
  labs(title = "Social Distancing by Cluster",
       x = "Date",
       y = "Social Distancing Score")

USStates %>%
  ggplot() +
  geom_point(aes(x = response_stay_home, y = social_dist_score, color = cluster_k_means)) +
  geom_text_repel(aes(x = response_stay_home, y = social_dist_score, label = ifelse(is_outlier(response_stay_home) | is_outlier(social_dist_score), state, "")),
                family = hrbrthemes::font_an) +
  facet_wrap(.~ date)

pd <- USStates %>%
  group_by(date) %>%
  ungroup() %>%
  arrange(date, social_dist_score) %>%
  mutate(order = row_number())

pd %>%
  ggplot() +
  geom_col(aes(y = order, x = social_dist_score, fill = cluster_k_means)) +
  facet_wrap(.~ date, scales = "free") +
  scale_y_continuous(breaks = pd$order, labels = pd$state, expand = c(0, 0)) +
  guides(fill = F) +
  theme_ipsum() +
  theme(panel.spacing = unit(0.25, "lines")) +
  labs(title = "US Social Distancing",
       x = "Social Distancing Score",
       y = NULL,
       caption = "Data courtesy of Google")

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
       color = NULL)

USStates %>%
  select(state, date, cases_per_capita, social_dist_score, cluster_k_means) %>%
  group_by(state) %>%
  mutate(change_cases_per_capita = cases_per_capita - lag(cases_per_capita, n = 2, order_by = date, default = 0),
         change_social_dist_score = social_dist_score - lag(social_dist_score, n = 2, order_by = date, default = 0)) %>%
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

# Changes in cases/capita affecting social distancing
USStates_Wide %>%
  mutate(diff_cases_capita = (`cases_per_capita_2020-04-11` - `cases_per_capita_2020-03-29`)/`cases_per_capita_2020-03-29`) %>%
  ggplot() +
  geom_smooth(aes(x = diff_cases_capita, y = avg_dist_score, color = governor),
              method = "lm", se = T) +
  geom_point(aes(x = diff_cases_capita, y = avg_dist_score, color = governor)) +
  geom_text_repel(aes(x = diff_cases_capita, y = avg_dist_score, label = ifelse(is_outlier(diff_cases_capita) | is_outlier(avg_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  scale_y_continuous(limits = c(-11, 15)) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "States with Democratic governors are distancing more",
       y = "Average Social Distancing Score",
       x = "Change in Rate of Spread from 3/29 to 4/11",
       color = NULL)

USStates %>%
  ggplot() +
  geom_smooth(aes(x = cases_per_capita, y = social_dist_score, color = governor),
              method = "lm", se = T) +
  geom_point(aes(x = cases_per_capita, y = social_dist_score, color = governor)) +
  geom_text_repel(aes(x = cases_per_capita, y = social_dist_score, label = ifelse(is_outlier(cases_per_capita) | is_outlier(social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ as.factor(date), scales = "free_x") +
  scale_y_continuous(limits = c(-11, 15)) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "States with more cases are distancing more",
       y = "Social Distancing Score",
       x = "Cases/Capita",
       color = NULL)

# Changes in rate of spread affecting social distancing
USStates_Wide %>%
  mutate(diff_rate = `rate_2020-04-11` - `rate_2020-03-29`) %>%
  ggplot() +
  geom_smooth(aes(x = diff_rate, y = avg_dist_score),
              method = "lm", se = T) +
  geom_point(aes(x = diff_rate, y = avg_dist_score)) +
  geom_text_repel(aes(x = diff_rate, y = avg_dist_score, label = ifelse(is_outlier(diff_rate) | is_outlier(avg_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  scale_y_continuous(limits = c(-11, 15)) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "States with more cases are distancing more",
       y = "Average Social Distancing Score",
       x = "Change in Cases/Capita from 3/29 to 4/11",
       color = NULL)
  
USStates %>%
  ggplot() +
  geom_smooth(aes(x = rate, y = social_dist_score),
              method = "lm", se = T) +
  geom_point(aes(x = rate, y = social_dist_score, color = cluster_k_means)) +
  geom_text_repel(aes(x = rate, y = social_dist_score, label = ifelse(is_outlier(rate) | is_outlier(social_dist_score), state, "")),
                  family = hrbrthemes::font_an) +
  facet_wrap(.~ as.factor(date), scales = "free_x") +
  scale_y_continuous(limits = c(-11, 15)) +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "States with more cases are distancing more",
       y = "Social Distancing Score",
       x = "Rate of Covid-19 Spread",
       color = NULL)

USStates_Wide %>%
  ggplot() +
  geom_smooth(aes(x = `rate_2020-03-29`, y = `social_dist_score_2020-04-05`),
              method = "lm", se = T) +
  geom_point(aes(x = `rate_2020-03-29`, y = `social_dist_score_2020-04-05`), color = "blue") +
  geom_smooth(aes(x = `rate_2020-04-05`, y = `social_dist_score_2020-04-11`),
              method = "lm", se = T) +
  geom_point(aes(x = `rate_2020-04-05`, y = `social_dist_score_2020-04-11`), color = "red") +
  theme_ipsum() +
  theme(legend.position = c(0.9, 0.15)) +
  labs(title = "Previous rate of spread influence on social distancing",
       y = "Social Distancing Score 4/05/20",
       x = "Growth Rate of Covid-19 3/29/20",
       color = NULL)

summary(lm(`social_dist_score_2020-04-11` ~ `rate_2020-03-29` + `rate_2020-04-05` + `rate_2020-04-11`, 
           data = USStates_Wide))



a<-USStates %>%
  filter(date != "2020-04-11") %>%
  select(state, date, rate) %>%
  pivot_longer(cols = rate, names_to = "rate", values_to = "rate") %>%
  bind_cols(USStates %>%
              filter(date != "2020-03-29") %>%
              select(state, date, social_dist_score) %>%
              pivot_longer(cols = social_dist_score))


USStates %>%
  ggplot() +
  geom_point(aes(x = rate, y = social_dist_score, color = as.factor(date)))

USStates_Wide %>%
  ggplot() +
  geom_point(aes(x = avg_response_time, y = avg_dist_score, color = governor)) +
  geom_smooth(aes(x = avg_response_time, y = avg_dist_score, color = governor),
              method = "lm", se = T)

USStates_Wide %>%
  ggplot() +
  geom_boxplot(aes(x = ))

