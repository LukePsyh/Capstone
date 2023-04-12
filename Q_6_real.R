library(dplyr)
set.seed(11313922)

theData_1_ <- theData_1_ %>% rename('Political_orientation' = 'Political orientation',
                          'Art_education' = 'Art education',
                          'An_artist_themselves' = 'An artist themselves')

df_Q6_clean <- na.omit(theData_1_, subset = c('Age','Gender','Political_orientation','Art_education',
                                          'General_sophistication','An_artist_themself'))

df_Q6 <- data.frame(matrix(nrow = 276, ncol = 8))

energy_means <- apply(df_Q6_clean[, 92:182], 1, mean)
preference_means <- apply(df_Q6_clean[, 1:91], 1, mean)
df_Q6 <- cbind(df_Q6_clean[216:221], energy_means, preference_means) 
cor(df_Q6, method = 'pearson')

set.seed(11313922)
train_index_Q6 <- createDataPartition(df_Q6$preference_means, p = 0.8, list = FALSE)
train_Q6 <- df_Q6[train_index_Q6,]
test_Q6 <- df_Q6[-train_index_Q6,]

model_Q6 <- lm(preference_means ~ energy_means + Age + Gender + 
                 Political_orientation + Art_education + Sophistication + An_artist_themselves, data = train_Q6)
summary(model_Q6)
r2_Q6 <- summary(model_Q6)$r.squared
print(r2_Q6)

predictions_Q6 <- predict.lm(model_Q6, test_Q6)
predictions_Q6

test_Q6["Predicted"]<-predictions_Q6

mse_Q6 <- mean((test_Q6$preference_means - predictions_Q6)^2)
rmse_Q6 <- sqrt(mse_Q6)
print(mse_Q6)
print(rmse_Q6)

cor(test_Q6$Predicted, test_Q6$preference_means)

ggplot(test_Q6, aes(x = Predicted, y = preference_means)) +
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  labs(title = "Predicted Mean Preference Ratings",
       subtitle = "Actual Mean Preference Ratings vs Predicted Mean Preference Ratings",
       x = "Predicted Mean Preference Ratings",
       y = "Actual Mean Preference Ratings")+
  theme(axis.title = element_text(size = 35),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggsave('Q_6_scatter.png')