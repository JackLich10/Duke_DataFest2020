source("R/Data_Cleaning_Manip.R")

library(usmap)
library(scales)

USStates_Map <- usmap_transform(USStates_Wide[, c("longitude", "latitude", "cluster_pop", "avg_dist_score")])

# map of US clusters by pop density and cases 3/29
plot_usmap(data = USStates, values = "cluster_pop") +
  geom_point(data = USStates_Map, aes(x = longitude.1, y = latitude.1)) +
  geom_text_repel(data = USStates_Map, aes(x = longitude.1, y = latitude.1, label = cluster_pop),
                  family = hrbrthemes::font_an) +
  scale_fill_manual(values = c("white", "#a90010", "grey", "#1a4ba9")) +
  theme(legend.position = "none",
        title = element_text(family = "Arial Narrow", size = 18, face = "bold", margin = 10),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12, face = "plain")) +
  labs(title = "Clustering US States",
       subtitle = "by population density and cases of Covid-19 on 3/29")

# map of US by social distancing scores
plot_usmap(data = USStates_Wide, values = "avg_dist_score", labels = FALSE) +
  geom_point(data = USStates_Map, aes(x = longitude.1, y = latitude.1)) +
  geom_text_repel(data = USStates_Map, aes(x = longitude.1, y = latitude.1, label = round(avg_dist_score, 2)),
            family = hrbrthemes::font_an) +
  scale_fill_gradient2(low = "#a90010", mid = "white", high = "#1a4ba9", midpoint = 0, limits = c(-10, 10), oob = squish) +
  theme(legend.position = "bottom",
        title = element_text(family = "Arial Narrow", size = 18, face = "bold"),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12, face = "plain"),
        legend.title = element_text(family = "Arial Narrow", size = 12, face = "plain")) +
  labs(title = "US Social Distancing by State",
       subtitle = "from 3/29 to 4/11",
       fill = "Social Distancing Score")

