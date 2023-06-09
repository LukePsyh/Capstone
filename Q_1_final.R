library(tidyr)
library(ggplot2)
library(gginference)
set.seed(11313922)

library(readr)
theData <- read_csv("~/Desktop/theData_real.csv")
View(theData)

types <- pivot_longer(theData,1:35,names_to = 'Painting',values_to = 'Classical_Ratings')
types_2 <- pivot_longer(theData,36:70,names_to = 'Painting',values_to = 'Modern_Ratings')

mean(types$Classical_Ratings)
mean(types_2$Modern_Ratings)

ggplot(types,aes(x=Classical_Ratings))+
  geom_histogram(binwidth = 1, color = "blue", fill = "white")+
  geom_vline(aes(xintercept = mean(Classical_Ratings)),
             color = 'red', linetype = 'dashed', size = 2)+
  annotate("text", x = 6, y = 2700, label = "Mean: 4.74", col = 'red', size = 10)+
  labs(title = 'Frequency Distribution of Classical Art Preference Ratings',
       y = 'Count')+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggplot(types_2,aes(x=Modern_Ratings))+
  geom_histogram(binwidth = 1, color = "blue", fill = "white")+
  geom_vline(aes(xintercept = mean(Modern_Ratings)),
             color = 'red', linetype = 'dashed', size = 2)+
  annotate("text", x = 5.5, y = 2700, label = "Mean: 4.26", col = 'red', size = 10)+
  labs(title = 'Frequency Distribution of Modern Art Preference Ratings',
       y = 'Count')+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggsave("classical_dist.png")
ggsave("modern_dist.png")

classical = types$Classical_Ratings
modern = types_2$Modern_Ratings

mwu <- wilcox.test(classical, modern, 
                     mu = 0, paired = FALSE, exact = NULL, correct = FALSE)
mwu_p <- mwu$p.value

ggplot(df_Q1)

boxplot(df_Q1, xlab = "Style", ylab = "Preference Rating",  col = c("red", "blue"))






