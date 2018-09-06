#Import data with syntax
surv <- read_csv("data/FirstDaySurveyData.csv") 

#Descriptives and Visual
median(surv$Happy, na.rm = TRUE)
sd(surv$Happy, na.rm = TRUE)
fivenum(surv$Happy)
hist(surv$Happy)

#Categorizing a new variable
surv <- mutate(surv,
               Hap = case_when(Happy < 70 ~ 0,
                               Happy >= 70 & Happy <= 100 ~ 1))


#Table of hap
hap_table <- table(select(surv, Hap))
hap_table
prop.table(hap_table)


#Creating a Contingency Table for two categorical variables:
t2 <- table(select(surv, Political, Hap))
t2
prop.table(t2, 1)
prop.table(t2, 2)