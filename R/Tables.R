source("R/Data_Cleaning_Manip.R")

library(gt)
library(gtsummary)

up_arrow <- "<span style=\"color:#1a4ba9\">&#9650;</span>"
down_arrow <- "<span style=\"color:#a90010\">&#9660;</span>"

up_arrow_red <- "<span style=\"color:#a90010\">&#9650;</span>"
down_arrow_blue <- "<span style=\"color:#1a4ba9\">&#9660;</span>"

USStates %>%
  group_by(date) %>%
  summarise(across(c(retail_recreation, grocery_pharmacy, parks, transit_stations, workplaces, residential), mean)) %>%
  mutate(retail_recreation = retail_recreation/100,
         grocery_pharmacy = grocery_pharmacy/100,
         parks = parks/100,
         transit_stations = transit_stations/100,
         workplaces = workplaces/100,
         residential = residential/100) %>%
  gt() %>%
  fmt_percent(columns = vars(retail_recreation, grocery_pharmacy, parks, transit_stations, workplaces, residential),
              decimals = 2) %>%
  text_transform(locations = cells_body(columns = "retail_recreation",
                                        rows = retail_recreation < lag(retail_recreation, n = 1, order_by = date)),
                 fn = function(x) paste(x, down_arrow_blue)) %>%
  text_transform(locations = cells_body(columns = "retail_recreation",
                                        rows = retail_recreation > lag(retail_recreation, n = 1, order_by = date)),
                 fn = function(x) paste(x, up_arrow_red)) %>%
  text_transform(locations = cells_body(columns = "grocery_pharmacy",
                                        rows = grocery_pharmacy < lag(grocery_pharmacy, n = 1, order_by = date)),
                 fn = function(x) paste(x, down_arrow_blue)) %>%
  text_transform(locations = cells_body(columns = "grocery_pharmacy",
                                        rows = grocery_pharmacy > lag(grocery_pharmacy, n = 1, order_by = date)),
                 fn = function(x) paste(x, up_arrow_red)) %>%
  text_transform(locations = cells_body(columns = "parks",
                                        rows = parks < lag(parks, n = 1, order_by = date)),
                 fn = function(x) paste(x, down_arrow_blue)) %>%
  text_transform(locations = cells_body(columns = "parks",
                                        rows = parks > lag(parks, n = 1, order_by = date)),
                 fn = function(x) paste(x, up_arrow_red)) %>%
  text_transform(locations = cells_body(columns = "transit_stations",
                                        rows = transit_stations < lag(transit_stations, n = 1, order_by = date)),
                 fn = function(x) paste(x, down_arrow_blue)) %>%
  text_transform(locations = cells_body(columns = "transit_stations",
                                        rows = transit_stations > lag(transit_stations, n = 1, order_by = date)),
                 fn = function(x) paste(x, up_arrow_red)) %>%
  text_transform(locations = cells_body(columns = "workplaces",
                                        rows = workplaces < lag(workplaces, n = 1, order_by = date)),
                 fn = function(x) paste(x, down_arrow_blue)) %>%
  text_transform(locations = cells_body(columns = "workplaces",
                                        rows = workplaces > lag(workplaces, n = 1, order_by = date)),
                 fn = function(x) paste(x, up_arrow_red)) %>%
  text_transform(locations = cells_body(columns = "residential",
                                        rows = residential < lag(residential, n = 1, order_by = date)),
                 fn = function(x) paste(x, down_arrow)) %>%
  text_transform(locations = cells_body(columns = "residential",
                                        rows = residential > lag(residential, n = 1, order_by = date)),
                 fn = function(x) paste(x, up_arrow)) %>%
  summary_rows(groups = NULL,
               columns = vars(retail_recreation, grocery_pharmacy, parks, transit_stations, workplaces, residential),
               fns = list(Avg. = ~mean(.)),
               formatter = fmt_percent,
               use_seps = FALSE) %>%
  cols_label(date = "Date", retail_recreation = "Retail/Recreation", grocery_pharmacy = "Grocery/Pharmacy",
             parks = "Parks", transit_stations = "Transit Stations", workplaces = "Workplaces", residential = "Residential")

USStates_Wide %>%
  group_by(cluster_pop) %>%
  mutate(diff_cases_capita = (`cases_per_capita_2020-04-11` - `cases_per_capita_2020-03-29`)/`cases_per_capita_2020-03-29`,
         diff_rate = (`rate_2020-04-11` - `rate_2020-03-29`)/`rate_2020-03-29`) %>%
  summarise(across(c(cases_3_29, `social_dist_score_2020-03-29`, `social_dist_score_2020-04-11`,
                     diff_rate, diff_cases_capita), mean)) %>%
  gt() %>%
  fmt_number(columns = vars(cases_3_29, `social_dist_score_2020-03-29`, `social_dist_score_2020-04-11`, diff_rate, diff_cases_capita),
             suffixing = TRUE) %>%
  fmt_percent(columns = vars(diff_rate, diff_cases_capita),
              decimals = 1) %>%
  text_transform(locations = cells_body(columns = "social_dist_score_2020-04-11",
                                        rows = `social_dist_score_2020-04-11` > `social_dist_score_2020-03-29`),
                 fn = function(x) paste(x, up_arrow)) %>%
  text_transform(locations = cells_body(columns = "social_dist_score_2020-04-11",
                                        rows = `social_dist_score_2020-04-11` < `social_dist_score_2020-03-29`),
                 fn = function(x) paste(x, down_arrow)) %>%
  text_transform(locations = cells_body(columns = "diff_rate",
                                        rows = diff_rate < 0),
                 fn = function(x) paste(x, down_arrow_blue)) %>%
  text_transform(locations = cells_body(columns = "diff_cases_capita",
                                        rows = diff_cases_capita > 0),
                 fn = function(x) paste(x, up_arrow_red)) %>%
  tab_footnote(footnote = "SDS denotes Social Distancing Score",
               locations = cells_column_labels(columns = vars(`social_dist_score_2020-03-29`, `social_dist_score_2020-04-11`))) %>%
  tab_footnote(footnote = "Denotes %change from 3/29 to 4/11",
               locations = cells_column_labels(columns = vars(diff_rate, diff_cases_capita))) %>%
  cols_label(cluster_pop = "Cluster", cases_3_29 = "Initial Cases", `social_dist_score_2020-03-29` = "Initial SDS",
             `social_dist_score_2020-04-11` = "Final SDS", diff_cases_capita = "Cases/Capita", diff_rate = "Rate of Spread") %>%
  tab_source_note(source_note = md("All values are averages within each cluster. Initial/Final refers to data from 3/29 and 4/11, respectively."))


# build simple linear model
to_model <- USStates_Wide %>%
  select(`social_dist_score_2020-03-29`, emergency_declaration, date_of_1st_case, date_of_stay_at_home_order,
         date_of_1st_death, governor, state_mandated_school_closures, cluster_pop) %>%
  mutate_if(is.character, factor) %>%
  mutate(date_of_stay_at_home_order = case_when(
    is.na(date_of_stay_at_home_order) ~ Sys.Date(),
    TRUE ~ date_of_stay_at_home_order))

model1 <- lm(`social_dist_score_2020-03-29` ~., data = to_model)

model_step <- step(model1, direction = "both")
summary(model_step)
  
tbl_regression(model_step, label = list(date_of_1st_case ~ "First Case",
                                        date_of_stay_at_home_order ~ "Stay at Home Order",
                                        governor ~ "Governor",
                                        cluster_pop ~ "Cluster")) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_gt() %>%
  tab_source_note(source_note = md("Linear regression predicting Social Distancing Scores")) %>%
  tab_source_note(source_note = md("Multiple R^2: 0.4766"))
  
  



