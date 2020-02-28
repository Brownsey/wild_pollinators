library(tidyverse)


data <- tibble(method = c("Maximum Linkage standardised",
                          "Euclidean Linkage standardised",
                          "Decision Agglomerative standardised",
                          "Decision kmeans standardised",
                          "Maximum Linkage",
                          "Euclidean Linkage",
                          "Decision agglomomerative",
                          "Decision kmeans"), num_clusts = c(2,5,4,3,8,4,7,5))


data %>% 
  ggplot(aes(num_clusts, x = method)) +
  geom_bar(stat="identity", position = "dodge", aes(fill = method)) +
  theme_bw() +
  coord_flip() +
  #Allowing labels to go onto 2 lines if required
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(legend.position = "none") +
  #title ="Median Weekly Face-to-Face Appointment Count by Season",
  labs(y = "Number of Clusters",
       x = "Clustering Method") +
  # theme(axis.text.x = element_text(angle = 90)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 18)) +
  ggtitle("Linkage Function VS Number of Clusters")
