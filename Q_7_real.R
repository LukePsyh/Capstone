library(ggplot2)
library(cluster)
library(dplyr)
set.seed(11313922)

theData_rev <- theData
theData_rev[,92:182] <- apply(theData[,92:182], 2, function(x) 8 - x)

avg_pref <- vector(length = 91)
avg_energy <- vector(length = 91)
clusters <- vector(length = 91)

avg_pref <- colMeans(theData_rev[1:91])
avg_energy <- colMeans(theData_rev[92:182])

df_Q7 <- cbind(avg_energy,avg_pref,clusters)
df_Q7 <- data.frame(df_Q7)
df_Q7 <- df_Q7 %>% slice(-46)

for (k in 2:10) {
  
  kmeans_result <- kmeans(df_Q7, k, nstart=10)
  
  sil_values <- silhouette(kmeans_result$cluster, dist(df_Q7))

  print(paste("k =", k, " average silhouette:", mean(sil_values[,3])))
}

kmeans_result <- kmeans(df_Q7, 4, nstart=10)

clusters <- as.numeric(kmeans_result$cluster)

df_Q7$clusters <- clusters

ggplot(df_Q7, aes(x=avg_pref, y=avg_energy, color=factor(clusters))) +
  geom_point()+
  labs(title = "2D Space of Average Energy Rating vs Average Preference Rating",
       subtitle = "For 90 art pieces",
       x = "Average Preference Rating",
       y = "Average Energy Rating")+
  theme(axis.title = element_text(size = 35),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggsave("Q7_cluster.png")