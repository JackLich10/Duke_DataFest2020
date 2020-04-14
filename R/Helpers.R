library(tidyverse)

# function to standardize mobility data
standardize_data <- function(data) {
  data_copy <- data
  
  # normalization of mobility trends (subtracting mean, dividing by standard deviation) (must be quantitative data)
  means <- apply(data[, 3:8], 2, mean)
  stdevs <- apply(data[, 3:8], 2, sd)
  data_copy[, 3:8] <- as.data.frame(scale(data[, 3:8], means, stdevs))
  return(data_copy)
}

# function to find Euclidean distance from avg state and between 3/29 and 4/5
find_euclidean_dist <- function(data) {
  # standardize data
  standardized <- standardize_data(data)
  # create tibble for measuring change in mobility by state
  euclid_dist_change <- tibble(state = unique(data$state),
                               euclidean_dist_change = as.numeric(rep(NA, nrow(data)/2)))
  
  # arrange data by state
  standardized_copy <- arrange(standardized, state)
  
  # find euclidean distance as a measure of how each state changed mobility patterns from 3/29 to 4/5
  for (i in seq(from = 1, to = 101, by = 2)) {
    euclid_dist_change[ceiling(i/2), 2] <- as.numeric(dist(rbind(standardized_copy[i, 3:8], standardized_copy[i+1,3:8])))
  }
  
  # find euclidean distance from avg mobility trends for all states
  avg_mobility_US <- standardized %>%
    group_by(date) %>%
    summarise(across(retail_recreation:residential, mean))
  
  euclid_dist <- tibble(state = data$state,
                        date = data$date,
                        euclidean_dist_avg = as.numeric(rep(NA, nrow(data))))
  
  for (i in 1:nrow(data)) {
    euclid_dist[i, 3] <- as.numeric(dist(rbind(avg_mobility_US[ceiling(2*i/nrow(data)), 2:7], standardized[i, 3:8])))
  }
  
  # join euclidean distance by state and remove unnecessary dataframes
  data <- left_join(data, euclid_dist, by = c("state", "date")) %>%
    left_join(euclid_dist_change, by = c("state"))
  return(data)
}

# function to detect outliers
is_outlier <- function(x) {
  return(x <= quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x >= quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}
