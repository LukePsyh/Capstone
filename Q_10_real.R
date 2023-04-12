library(dplyr)
library(ggplot2)
library(ggsave)
set.seed(11313922)
theData_Q10<-theData %>%
  mutate(left=ifelse(`Political orientation`==1|`Political orientation`==2, 1, 0))
View(theData_Q10)

relevant_data <- theData_Q10[183:222]

relevant_data$`Political orientation`<-NULL
relevant_data<-na.omit(relevant_data)

maxs<-apply(relevant_data, 2, max)
mins<-apply(relevant_data, 2, min)
relevant_data<-data.frame(scale(relevant_data, center=mins, scale=maxs-mins))


split_Q10 <- sample(c(TRUE, FALSE), nrow(relevant_data), replace=TRUE, prob=c(0.8,0.2))
train_Q10  <- relevant_data[split_Q10, ]
test_Q10   <- relevant_data[-split_Q10, ]

model_Q10 <- lm(left~., data=train_Q10)
x<-data.frame(linear_model$coefficients)
View(x)

log_model <- glm(left ~.,family=binomial(link='logit'),data=train_Q10)
predictions_Q10 <- predict(log_model, test_Q10)

mse_Q10 <- mean((test_Q10$left - predictions_Q10)^2)
rmse_Q10 <- sqrt(mse_Q10)
print(mse_Q10)
print(rmse_Q10)

regression_df_Q10 <- data.frame(prediction=predictions_Q10, actual=test_Q10$left)

ggplot(regression_df_Q10, aes(x=prediction, y=actual)) + 
  geom_point() +
  geom_smooth( se=FALSE, col = "red") +
  xlab("Predicted Leanings") +
  ylab("Actual Leanings") +
  ggtitle("Predicted Leanings vs. Actual Mean Leanings",
          subtitle = "Using testing data")+
  theme(axis.title = element_text(size = 35),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
ggsave("Q_10_log.png")
