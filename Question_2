library(tidyr)
library(ggplot2)
set.seed(11313922)

#Load in theData
library(readr)

types_3 <- pivot_longer(theData,71:91,names_to = 'Painting',values_to = 'Non_Human_Ratings')

ggplot(types_3,aes(x=Non_Human_Ratings))+
  geom_histogram(binwidth = 1, color = "blue", fill = "white")

non_human <- types_3$Non_Human_Ratings

mwu_2 <- wilcox.test(modern, non_human,
                   alternative = "two.sided", mu = 0, paired = FALSE, exact = NULL, correct = FALSE)
mwu_p_2 <- mwu_2$p.value

boxplot(modern, non_human, xlab = "Style", ylab = "Preference Rating",  col = c("red", "blue"),
        names = c("modern","non-human"))
