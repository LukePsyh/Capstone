library(lm.beta)  
library(dplyr)
library(psych)
library(caret)
set.seed(11313922)

theData_Q9 <- theData[,1:91]
theData_Q9 <- cbind(theData_Q9,theData[,183:194])
theData_Q9 <- na.omit(theData_Q9)

dark_ratings <- theData_Q9[,92:103]
art_preferences_Q9 <- theData_Q9[,1:91]
mean_pref_ratings_Q9 <- rowMeans(art_preferences_Q9)

pca_dark <- principal(dark_ratings, nfactors = 3)
first_pca_component_Q9 <- pca_dark$scores[,1]
second_pca_component_Q9 <- pca_dark$scores[,2]
third_pca_component_Q9 <- pca_dark$scores[,3]

View(first_pca_component_Q9)

plot_pca <- prcomp(dark_ratings, scale = TRUE)
barplot(plot_pca$rotation[,1], main = "PC 1 Loading Plot", las = 2, axisnames = FALSE)
barplot(plot_pca$rotation[,2], main = "PC 2 Loading Plot", las = 2, axisnames = FALSE)
barplot(plot_pca$rotation[,3], main = "PC 3 Loading Plot", las = 2, axisnames = FALSE)

data_Q9_for_splitting <- 

data_Q9_for_splitting <- data.frame(cbind(first_pca_component_Q9, second_pca_component_Q9,
                                          third_pca_component_Q9, mean_pref_ratings_Q9))

split_Q9 <- createDataPartition(data_Q9_for_splitting$mean_pref_ratings_Q9, p=0.8, list=FALSE)
train_Q9 <- data_Q9_for_splitting[split_Q9,]
test_Q9 <- data_Q9_for_splitting[-split_Q9,]
View(train_Q9)
str(train_Q9)

model_Q9_train <- lm(mean_pref_ratings_Q9 ~ first_pca_component_Q9 + second_pca_component_Q9 +
                     third_pca_component_Q9, data = train_Q9)

predictions_Q9 <- predict(model_Q9_train, test_Q9)

regression_df_Q9 <- data.frame(prediction=predictions_Q9, actual=test_Q9$mean_pref_ratings_Q9)

mse_Q9 <- mean((test_Q9$mean_pref_ratings - predictions_Q9)^2)
rmse_Q9 <- sqrt(mse_Q9)
print(mse_Q9)
print(rmse_Q9)
summary(model_Q9_train)$r.squared
lm.beta(model_Q9_train)

ggplot(regression_df_Q9, aes(x=prediction, y=actual)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE, col = "red") +
  xlab("Predicted Mean Preference Ratings") +
  ylab("Actual Mean Preference Ratings") +
  ggtitle("Predicted Mean Preference Ratings vs. Actual Mean Preference Ratings",
          subtitle = "Using testing data")+
  theme(axis.title = element_text(size = 35),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
ggsave("Q_9_lm.png")
