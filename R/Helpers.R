library(tidyverse)

# function to find Euclidean distance from avg state and between 3/29 and 4/5
find_euclidean_dist <- function(data) {
  # make a copy of data
  data_copy <- data
  
  for (i in c(nrow(data)/2, nrow(data))) {
    # normalization of mobility trends (subtracting mean, dividing by standard deviation) (must be quantitative data)
    means <- apply(data[(i-50):i, 3:8], 2, mean)
    stdevs <- apply(data[(i-50):i, 3:8], 2, sd)
    data_copy[(i-50):i, 3:8] <- as.data.frame(scale(data[(i-50):i, 3:8], means, stdevs))
  }
  
  euclid_dist_change <- tibble(state = unique(data$state),
                               euclidean_dist_change = as.numeric(rep(NA, nrow(data)/2)))
  
  data_copy2 <- arrange(data_copy, state)
  
  for (i in seq(from = 1, to = 101, by = 2)) {
    euclid_dist_change[ceiling(i/2), 2] <- as.numeric(dist(rbind(data_copy2[i,3:8], data_copy2[i+1,3:8])))
  }
  
  # find euclidean distance from avg mobility trends for all states
  avg_mobility_US <- data_copy %>%
    group_by(date) %>%
    summarise(across(retail_recreation:residential, mean))
  
  euclid_dist <- tibble(state = data$state,
                        date = data$date,
                        euclidean_dist_avg = as.numeric(rep(NA, nrow(data))))
  
  for (i in 1:nrow(data)) {
    euclid_dist[i, 3] <- as.numeric(dist(rbind(avg_mobility_US[ceiling(2*i/nrow(data)),2:7], data_copy[i,3:8])))
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