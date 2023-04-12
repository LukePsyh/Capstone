library(tidyr)
library(ggplot2)
set.seed(11313922)
#Filter out non-binary participants and participants who did not disclose their gender
gender_df <- theData[!is.na(theData$Gender) & theData$Gender !=  3,]

#Get the art preference ratings of men and women in their own data frames
Women_Ratings_df <- gender_df[gender_df$Gender == 2, 1:91]
Men_Ratings_df <- gender_df[gender_df$Gender == 1, 1:91]

#Get all observations of art preference ratings into their own variables.
men_df <- data.frame(matrix(nrow = 8645, ncol = 2))
colnames(men_df) <- c('Painting_Names',"Men_Ratings")
men_df <- pivot_longer(Men_Ratings_df,1:91,names_to = 'Painting_Names',values_to = 'Men_Ratings')

women_df <- data.frame(matrix(nrow = 16289, ncol = 2))
colnames(women_df) <- c('Painting_Names',"Women_Ratings")
women_df <- pivot_longer(Women_Ratings_df,1:91,names_to = 'Painting_Names',values_to = 'Women_Ratings')

#Get the variables of art preference ratings of men and women into the R global environment
women_preferences <- women_df$Women_Ratings
men_preferences <- men_df$Men_Ratings

women_mean <- mean(women_preferences)
men_mean <- mean(men_preferences)

ggplot(women_df,aes(x=Women_Ratings))+
  geom_histogram(binwidth = 1, color = "blue", fill = "white")+
  geom_vline(aes(xintercept = women_mean),
             color = 'red', linetype = 'dashed', size = 2)+
  annotate("text", x = 5.5, y = 3500, label = "Mean: 4.23", col = 'red', size = 10)+
  labs(title = 'Frequency Distribution of Womens Preference Ratings on All Pieces of Art',
       y = 'Count')+
  theme(plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggplot(men_df,aes(x=Men_Ratings))+
  geom_histogram(binwidth = 1, color = "blue", fill = "white")+
  geom_vline(aes(xintercept = men_mean),
             color = 'red', linetype = 'dashed', size = 2)+
  annotate("text", x = 5.5, y = 2000, label = "Mean: 4.22", col = 'red', size = 10)+
  labs(title = 'Frequency Distribution of Mens Preference Ratings on All Pieces of Art',
       y = 'Count')+
  theme(plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

mwu_3 <- wilcox.test(women_preferences, men_preferences,
                      mu = 0, paired = FALSE, exact = NULL, correct = FALSE)
mwu_p_3 <- mwu_3$p.value
print(mwu_3)

ggsave('men_ratings.png')
ggsave('women_ratings.png')

boxplot(women_preferences, men_preferences, xlab = "Gender", ylab = "Preference Rating",  col = c("pink", "blue"),
        names = c("women","men"))
