
library(SDSRegressionR)
library(tidyverse)

#Load the Data:
wellbeing <- read_csv("data/WellBeing.csv")

#Subset for Italy
ital <- wellbeing %>% 
  filter(Country == 1)

# Visualize and describe Psychological.Wellbeing
histogram(ital$Psychological.Wellbeing)
fivenum(ital$Psychological.Wellbeing)
mean(ital$Psychological.Wellbeing)
sd(ital$Psychological.Wellbeing)

# Create a correlation matrix 
cor(select(ital, Psychological.Wellbeing, Sat.With.Life, Positive.Aff, Negative.Aff), 
    use="pairwise.complete.obs")

# scatterplots with line of best fit
simpleScatter(ital, Positive.Aff,Psychological.Wellbeing,line = TRUE)
simpleScatter(ital, Negative.Aff,Psychological.Wellbeing,line = TRUE)
simpleScatter(ital, Sat.With.Life,Psychological.Wellbeing,  line = TRUE)

# identify specific case
filter(ital, Psychological.Wellbeing == max(Psychological.Wellbeing, na.rm=TRUE))

#Subset the data
good_ital <- ital %>% 
  filter(SubjectID != "ID215")

# Rerun a correlation matrix 
cor(select(good_ital, Psychological.Wellbeing, Sat.With.Life, Positive.Aff, Negative.Aff), 
    use="pairwise.complete.obs")

# Create correlation matrix with p-values
library(psych)
corr.test(select(good_ital, Psychological.Wellbeing, Sat.With.Life, Positive.Aff, Negative.Aff), 
          use="pairwise.complete.obs")$p
