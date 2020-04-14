# source data cleaning
source("R/Data_Cleaning_Manip.R")

# plot clusters with PCA
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

rm(a, USStates_PCA, clusters)

