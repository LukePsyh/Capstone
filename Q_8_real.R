library(dplyr)
library(psych)
library(caret)
set.seed(11313922)

theData_Q8 <- theData[,1:91]
theData_Q8 <- cbind(theData_Q8,theData[,206:215])
theData_Q8 <- na.omit(theData_Q8)

self_image_ratings <- theData_Q8[,92:101]
art_preferences <- theData_Q8[,1:91]
mean_pref_ratings <- rowMeans(art_preferences)

pca_self_image <- principal(self_image_ratings)
first_pca_component <- pca_self_image$scores[,1]

eigenvalues <- pca_self_image$values
eigenvalues_df <- data.frame(eigenvalue=eigenvalues, component=1:length(eigenvalues))

data_Q8_for_splitting <- data.frame(cbind(first_pca_component, mean_pref_ratings))

split <- createDataPartition(data_Q8_for_splitting$mean_pref_ratings, p=0.8, list=FALSE)
train <- data_Q8_for_splitting[split,]
test <- data_Q8_for_splitting[-split,]

model_Q8_train <- lm(mean_pref_ratings ~ first_pca_component, data = train)

predictions_Q8 <- predict(model_Q8_train, test)

mse_Q8 <- mean((test$mean_pref_ratings - predictions_Q8)^2)
rmse_Q8 <- sqrt(mse_Q8)
print(mse_Q8)
print(rmse_Q8)
summary(model_Q8_train)$r.squared

regression_df <- data.frame(prediction=predictions_Q8, actual=test$mean_pref_ratings)

ggplot(eigenvalues_df, aes(x=component, y=eigenvalue)) + 
  geom_bar(stat="identity") +
  xlab("Component") +
  ylab("Eigenvalue") +
  ggtitle("Eigenvalues for PCA of Self-Image Ratings")
ggsave("Q_8_eigen.png")

ggplot(regression_df, aes(x=prediction, y=actual)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE, col = "red") +
  xlab("Predicted Mean Preference Ratings") +
  ylab("Actual Mean Preference Ratings") +
  ggtitle("Predicted Mean Preference Ratings vs. Actual Mean Preference Ratings")+
  theme(axis.title = element_text(size = 35),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
ggsave("Q_8_lm.png")
