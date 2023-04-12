library(tidyr)
library(ggplot2)
set.seed(11313922)
#Filter out those who had zero or 3 years of art education, as well as those who did not respond and place in one df
some_education_df <- theData[!is.na(theData$`Art education`) & theData$`Art education` !=3 & theData$`Art education` != 0,]
#Filter out those who had any art education and those who did not respond and place the obs. into another df
no_education_df <- theData[!is.na(theData$`Art education`) & theData$`Art education` !=3 & 
                             theData$`Art education` != 1 & theData$`Art education` != 2,]

#Isolate preference ratings from both data frames
some_df <- some_education_df[some_education_df$`Art education` == 1 & 2, 1:91]
none_df <- no_education_df[no_education_df$`Art education` == 0, 1:91]

#Place all observations of art preference ratings into their own variables
some_pref_df <- data.frame(matrix(nrow = 8099, ncol = 2))
colnames(some_pref_df) <- c('Painting_Names', 'Some_Edu_Ratings')
some_pref_df <- pivot_longer(some_df,1:91,names_to = 'Painting_Names',values_to = 'Some_Edu_Ratings')

none_pref_df <- data.frame(matrix(nrow = 8372, ncol = 2))
colnames(none_pref_df) <- c('Painting_Names', 'No_Edu_Ratings')
none_pref_df <- pivot_longer(none_df,1:91,names_to = 'Painting_Names', values_to = 'No_Edu_Ratings')

#Get the variables of art preference ratings of those with some art education and those
# with no art education into the R global environment
no_edu_prefrences <- none_pref_df$No_Edu_Ratings
some_edu_prefrences <- some_pref_df$Some_Edu_Ratings

mean_no_edu <- mean(no_edu_prefrences)
mean_some_edu <- mean(some_edu_prefrences)

ggplot(none_pref_df,aes(x=no_edu_prefrences))+
  geom_histogram(binwidth = 1, color = "blue", fill = "white")+
  geom_vline(aes(xintercept = mean_no_edu),
             color = 'red', linetype = 'dashed', size = 2)+
  annotate("text", x = 2, y = 1500, label = "Mean: 4.31", col = 'red', size = 10)+
  labs(title = 'Distribution of No Art Education Preference Ratings',
       y = 'Count')+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggplot(some_pref_df,aes(x=some_edu_prefrences))+
  geom_histogram(binwidth = 1, color = "blue", fill = "white")+
  geom_vline(aes(xintercept = mean_some_edu),
             color = 'red', linetype = 'dashed', size = 2)+
  annotate("text", x = 5.8, y = 1750, label = "Mean: 4.22", col = 'red', size = 10)+
  labs(title = 'Distribution of Some Art Education Preference Ratings',
       y = 'Count')+
  theme(plot.title = element_text(size = 19),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggsave('no_edu_distribution.png')
ggsave('some_edu_distribution.png')

mwu_4 <- wilcox.test(some_edu_pref, no_edu_pref,
                     alternative = "two.sided", mu = 0, paired = FALSE, exact = NULL, correct = FALSE)
mwu_p_4 <- mwu_4$p.value

boxplot(some_edu_pref, no_edu_pref, xlab = "Art Education Level", ylab = "Preference Rating",  col = c("red", "blue"),
        names = c("Some","None"))


