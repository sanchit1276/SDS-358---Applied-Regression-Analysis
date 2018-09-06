#### Intro to R ####

#Install our custom class package
install.packages("devtools") #if needed...
devtools::install_github("MichaelJMahometa/SDSRegressionR")
library(SDSRegressionR) #"Activate" the package with the library() command

#Install the helpers (if needed)
install.packages("tidyverse")
library(tidyverse)

#A basic calculator
7 + 2

#Saving objects
object <- 7+2
object

#Working with objects
object * 2

#Vectors (single dimention of data)
rooster <- c(7,2)
rooster
rooster * 2
sum(rooster)

#Import data with syntax
surv <- read_csv("data/FirstDaySurveyData.csv") 

# Take a quick peek to confirm...
surv

#Getting a vector from data
surv$Gender
pull(surv, Gender)
pull(surv, 2)

#Select mulitple columns
select(surv, Gender, Political)
#Save the selection
small <- select(surv, UniqueID, Gender, Political)

#Filter (subset)
men <- filter(surv, Gender=="Male")
men_dem <- filter(surv, Gender=="Male" & Political=="Democrat")

sixsix <- filter(surv, height.inches >= 66)
sixsix_fem <- filter(surv, height.inches >= 66 & Gender=="Female")

#Create new variables
surv <- mutate(surv,
               Hap_cat = case_when(Happy < 75 ~ 0,
                                   Happy >= 75 & Happy <= 100 ~ 1))

#Check
table(select(surv, Happy, Hap_cat)) #Needs factoring!

#Truncate a variable
surv <- mutate(surv, Emot = case_when(Emotional.Women == "Strongly Disagree" |
                                Emotional.Women == "Disagree" ~ 0,
                                Emotional.Women == "Strongly Agree" |
                                Emotional.Women == "Agree" ~ 1))
#Check
table(select(surv, Emotional.Women, Emot))

#Factor a categorical variable
surv <- mutate(surv, 
               Emotional.Women = factor(Emotional.Women,
                                        levels=c("Strongly Disagree", "Disagree",
                                                 "Agree", "Strongly Agree")))
#Checking
table(select(surv, Emotional.Women, Emot))

#Creating variables
#Simple
surv <- mutate(surv, par_diff = Father.Born - Mother.Born)

#Using functions (with piping)
surv <- surv %>% 
  rowwise() %>% 
  mutate(par_sum = sum(c(Father.Born, Mother.Born), na.rm=TRUE)) %>% 
  ungroup()

#This is the same, just harder to read...
surv <- mutate(rowwise(surv), par_sum2 = sum(c(Father.Born, Mother.Born), na.rm=TRUE))

#Different functions
surv <- surv %>% 
  rowwise() %>% 
  mutate(par_mean = mean(c(Father.Born, Mother.Born), na.rm=TRUE)) %>% 
  ungroup()

surv <- surv %>% 
  rowwise() %>% 
  mutate(par_max = max(c(Father.Born, Mother.Born), na.rm=TRUE)) %>% 
  ungroup()

#Summarizing (Count data)
#Using dplyr notation
t <- table(select(surv, Gender))
t

#Get more information
addmargins(t)
prop.table(t)
barplot(t)
barplot(prop.table(t))

#Two way table
t2 <- table(select(surv, Gender, Emotional.Women))
t2
addmargins(t2)
prop.table(t2) #Table proportions
prop.table(t2, 1) #Row proportions
prop.table(t2, 2) #Column proportions
barplot(t2, beside=TRUE, legend=TRUE)

#Simple functions on quantitative data
#Using base vector notation:
mean(surv$Happy, na.rm = TRUE)
sd(surv$Happy, na.rm = TRUE)
fivenum(surv$Happy)
hist(surv$Happy)

#Rolling into dplyr
surv %>% 
  group_by(Gender) %>% 
  summarise(mn = mean(Happy, na.rm=TRUE))

#Using skimr (very helpful)
#install.packages("skimr")
library(skimr)
surv %>% 
  group_by(Gender) %>% 
  skim(Happy) 

#Plotting....
s <- surv %>% 
  group_by(Gender) %>% 
  summarise(mn = mean(Happy, na.rm=TRUE))

barplot(s$mn, names.arg=s$Gender)

#Or with ggplot
library(ggplot2)
ggplot(s, aes(x=Gender, y=mn)) +
  geom_bar(stat="identity", fill="grey") +
  coord_cartesian(ylim=c(60,80))

#A very nice graph...
s2 <- surv %>% 
  group_by(Gender) %>% 
  summarise(mn = mean(Happy, na.rm=TRUE),
            sd = sd(Happy, na.rm=TRUE))
ggplot(s2, aes(x=Gender, y=mn)) +
  geom_bar(stat="identity", fill="grey") +
  geom_errorbar(aes(ymin=mn-sd, ymax=mn+sd), width=.2) 
