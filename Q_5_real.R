library(caret)
library(tidyr)
set.seed(11313922)

df_Preference <- theData[1:91]
df_Energy <- theData[92:182]
df_Preference <- pivot_longer(df_Preference,1:91,names_to = 'Painting_Names', values_to = 'pref_ratings')
df_Energy <- pivot_longer(df_Energy,1:91,names_to = 'Painting_Names', values_to = 'energy_ratings')
df_Q5 <- data.frame(painting_names = df_Energy$Painting_Names, Preference_Ratings = df_Preference$pref_ratings,
                    Energy_Ratings = df_Energy$energy_ratings)

cor_Q5 <- cor(df_Q5$Preference_Ratings, df_Q5$Energy_Ratings)
print(cor_Q5)

train_index <- createDataPartition(df_Q5$Preference_Ratings, p = 0.8, list = FALSE)
train_Q5 <- df_Q5[train_index,]
test_Q5 <- df_Q5[-train_index,]

model_Q5 <- lm(Preference_Ratings ~ Energy_Ratings, data = train_Q5)

r2 <- summary(model_Q5)$r.squared
print(r2)

predictions_Q5 <- predict.lm(model_Q5, test_Q5)

mse_Q5 <- mean((test_Q5$Preference_Ratings - predictions_Q5)^2)
rmse_Q5 <- sqrt(mse_Q5)
print(mse_Q5)
print(rmse_Q5)

ggplot(train_Q5, aes(x = Energy_Ratings, y = Preference_Ratings)) +
  geom_point(alpha = 0.0045)+
  geom_smooth(method = "lm", col = "red")+
  labs(title = "Linear Regression Model",
       subtitle = "Art Preference Ratings vs Art Energy Ratings",
       x = "Energy Ratings",
       y = "Preference Ratings")+
  theme(axis.title = element_text(size = 35),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


ggsave('Q_5_lm.png')