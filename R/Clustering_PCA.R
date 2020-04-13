# source data cleaning
source("R/Data_Cleaning_Manip.R")

# function to standardize mobility data
standardize_data <- function(data) {
  data_copy <- data
  
  for (i in c(nrow(data)/2, nrow(data))) {
    # normalization of mobility trends (subtracting mean, dividing by standard deviation) (must be quantitative data)
    means <- apply(data[(i-50):i, 3:8], 2, mean)
    stdevs <- apply(data[(i-50):i, 3:8], 2, sd)
    data_copy[(i-50):i, 3:8] <- as.data.frame(scale(data[(i-50):i, 3:8], means, stdevs))
  }
  return(data_copy)
}
# standardize mobility data
US_standardized <- standardize_data(USStates)

# calculating Euclidean distance for each state for both dates
distance_march <- US_standardized[which(US_standardized$date == "2020-03-29"),] %>%
  dplyr::select(retail_recreation:residential) %>%
  dist()

distance_april <- US_standardized[which(US_standardized$date == "2020-04-05"),] %>%
  dplyr::select(retail_recreation:residential) %>%
  dist()

# cluster dendrogram with complete linkage
hierarchical_clust_march <- hclust(distance_march)
hierarchical_clust_april <- hclust(distance_april)

clusters_march <- cutree(hierarchical_clust_march, 3)
clusters_april <- cutree(hierarchical_clust_april, 3)

# k-means clustering
k_means_march <- kmeans(distance_march, 3)
k_means_april <- kmeans(distance_april, 3)

# add cluster number to each observation
march <- USStates %>%
  filter(date == "2020-03-29") %>%
  dplyr::mutate(cluster_k_means = k_means_march$cluster,
                cluster_hierarchical = clusters_march)

april <- USStates %>%
  filter(date == "2020-04-05") %>%
  dplyr::mutate(cluster_k_means = k_means_april$cluster,
                cluster_hierarchical = clusters_april)

USStates <- rbind(march, april) %>%
  mutate(cluster_k_means = case_when(
    date == "2020-04-05" & cluster_k_means == 3 ~ as.numeric(1),
    date == "2020-04-05" & cluster_k_means == 1 ~ as.numeric(3),
    TRUE ~ as.numeric(cluster_k_means)))

# write .csv file
write_csv(USStates, path = "data/USStates.csv")

a <- USStates %>%
  rename(`Retail/Recreation` = retail_recreation, 
         `Grocery/Pharmacy` = grocery_pharmacy, 
         `Parks` = parks, 
         `Transit Stations` = transit_stations, 
         `Workplaces` = workplaces, 
         `Residential` = residential)

USStates_PCA <- prcomp(a[which(a$date == "2020-03-29"), 3:8], center = TRUE, scale. = TRUE)

clusters <- as.factor(a[which(USStates$date == "2020-03-29"),]$cluster_k_means)

ggbiplot::ggbiplot(USStates_PCA, ellipse = T, groups = clusters) +
  geom_point(aes(color = clusters)) +
  geom_text_repel(label = unique(USStates$state), size = 2, family = hrbrthemes::font_an) +
  scale_x_continuous(expand = c(0.4, .4)) +
  theme_ipsum() +
  theme(legend.position = c(0.8, 0.9)) +
  labs(title = "PCA of US Mobility Trends",
       subtitle = "3/29/20",
       color = "Cluster")

USStates_PCA <- prcomp(a[which(a$date == "2020-04-05"), 3:8], center = TRUE, scale. = TRUE)

clusters <- as.factor(a[which(USStates$date == "2020-04-05"),]$cluster_k_means)

ggbiplot::ggbiplot(USStates_PCA, ellipse = T, groups = clusters) +
  geom_point(aes(color = clusters)) +
  geom_text_repel(label = unique(USStates$state), size = 2, family = hrbrthemes::font_an) +
  scale_x_continuous(expand = c(0.2, .2)) +
  theme_ipsum() +
  theme(legend.position = c(0.8, 0.9)) +
  labs(title = "PCA of US Mobility Trends",
       subtitle = "4/5/20",
       color = "Cluster")

detach("package:plyr", unload = TRUE)

# # cluster dendrogram with complete linkage
# hierarchical_clust_c <- hclust(distance_march)
# plot(hierarchical_clust_c, hang = 1)
# 
# rect.hclust(hierarchical_clust_c, k = 2, border = "grey")
# rect.hclust(hierarchical_clust_c, k = 3, border = "blue")
# rect.hclust(hierarchical_clust_c, k = 4, border = "red")
# 
# # k-means clustering
# k_means_c <- kmeans(distance_april, 3)
# plot(US_standardized[, 3:8], col = k_means_c$cluster)
# k_means_c
# 
# # hierarchical clustering
# clusters <- cutree(hierarchical_clust_c, 3)
# plot(US_standardized[, 3:8], col = clusters)
  
