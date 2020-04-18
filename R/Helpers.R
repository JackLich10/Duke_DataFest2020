library(tidyverse)

# function to standardize mobility data or population data
standardize_data <- function(data, mobility = TRUE) {
  data_copy <- data
  
  if (mobility == TRUE) {
    bounds <- 3:8
  } else {
    bounds <- c(24, 31)
  }
  
  # normalization of mobility trends (subtracting mean, dividing by standard deviation) (must be quantitative data)
  means <- apply(data[, bounds], 2, mean)
  stdevs <- apply(data[, bounds], 2, sd)
  data_copy[, bounds] <- as.data.frame(scale(data[, bounds], means, stdevs))
  return(data_copy)
}

# function to find Euclidean distance from avg state and between 3/29 and 4/5
find_euclidean_dist <- function(data, dates) {
  # standardize data
  standardized <- standardize_data(data)
  # create tibble for measuring change in mobility by state
  euclid_dist_change <- tibble(state = unique(data$state),
                               euclidean_dist_change = as.numeric(rep(NA, nrow(data)/dates)))
  
  # # arrange data by state
  # standardized_copy <- arrange(standardized, state)
  # 
  # # find euclidean distance as a measure of how each state changed mobility patterns from 3/29 to 4/5
  # for (i in seq(from = 1, to = 101, by = 2)) {
  #   euclid_dist_change[ceiling(i/2), 2] <- as.numeric(dist(rbind(standardized_copy[i, 3:8], standardized_copy[i+1,3:8])))
  # }
  
  # find euclidean distance from avg mobility trends for all states
  avg_mobility_US <- standardized %>%
    group_by(date) %>%
    summarise(across(retail_recreation:residential, mean))
  
  euclid_dist <- tibble(state = data$state,
                        date = data$date,
                        euclidean_dist_avg = as.numeric(rep(NA, nrow(data))))
  
  for (i in 1:nrow(data)) {
    euclid_dist[i, 3] <- as.numeric(dist(rbind(avg_mobility_US[ceiling(dates*i/nrow(data)), 2:7], standardized[i, 3:8])))
  }
  # join euclidean distance by state
  data <- left_join(data, euclid_dist, by = c("state", "date"))
  # data <- left_join(data, euclid_dist_change, by = c("state"))
  return(data)
}

# function to find clusters of social distancing within population density clusters
find_clusters_within <- function(data, clusters = 4) {
  # Standardize mobility data
  US_standardized <- standardize_data(data, mobility = T)
  
  for (i in 1:clusters) {
    # Find cluster i
    cluster_i <- US_standardized[which(US_standardized$cluster_pop == i),]
    # Calculate euclidean distance
    distance <- dist(cluster_i[, 3:8])
    # Find 3 clusters
    k_means <- kmeans(distance, 3)
    
    cluster_i <- cluster_i %>%
      mutate(cluster_k_means = k_means$cluster)
    
    if(!exists("total_df")) {
      total_df <- cluster_i
    } else {
      total_df <- rbind(total_df, cluster_i)
    }
  }
  data <- data %>%
    left_join(total_df %>%
                select(state, date, cluster_pop, cluster_k_means),
              by = c("state", "date", "cluster_pop"))
  
  return(data)
}

# function to detect outliers
is_outlier <- function(x) {
  return(x <= quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x >= quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}
