
#Lab9: Mediation Models

library(SDSRegressionR)

#Load Data:
athlete <- read_csv("data/athleteBurnout.csv")
names(athlete)

#Full model to look for "baddness"
full <- lm(burnout ~ generalstress + noconfidence + performancedemand + academicrequirements, data=athlete)
residFitted(full)
cooksPlot(full, key.variable="StudID", print.obs=TRUE, sort.obs=TRUE)
threeOuts(full, key.variable="StudID")

#Take out the Cook's D folks:
g_athlete <- athlete %>% 
  filter(StudID %not in% c(277))

#Run the FULL model first:
#Path B and Cprime
cprime <- lm(burnout ~ generalstress + noconfidence + performancedemand + academicrequirements, data=g_athlete)
summary(cprime)
lmBeta(cprime)

#Keep ALL the model data...
all_data <- modelData(cprime)

#Total effect model (no mediator(s))
pathc <- lm(burnout ~ generalstress + performancedemand + academicrequirements, data=all_data)
summary(pathc)
lmBeta(pathc)

#Path A
patha <- lm(noconfidence ~ generalstress + performancedemand + academicrequirements, data=all_data)
summary(patha)
lmBeta(patha)

#Indirect Effect (Multiply paths a and b - your choice how)
ind <- summary(patha)$coef["generalstress", "Estimate"] * 
  summary(cprime)$coef["noconfidence", "Estimate"]

#Sobel test
se <- sqrt((summary(cprime)$coef["noconfidence", "Estimate"]^2 * 
              summary(patha)$coef["generalstress", "Std. Error"]^2) + 
             (summary(patha)$coef["generalstress", "Estimate"]^2 * 
                summary(cprime)$coef["noconfidence", "Std. Error"]^2))
z <- ind/se
z
(1 - pnorm(z)) * 2

#Double Check Work (with Bootstrapped CIs)
library(psych)
#NOTICE: use of our "good dataset"
burnout <- psych::mediate(burnout ~ generalstress + (noconfidence) - performancedemand - academicrequirements, data=g_athlete, n.iter = 1000, std=FALSE)
print(burnout, short=FALSE)
burnout$a
burnout$b
burnout$c
burnout$cprime
burnout$ab

burnout_std <- psych::mediate(burnout ~ generalstress + (noconfidence) - performancedemand - academicrequirements, data=g_athlete, n.iter = 1000, std=TRUE)
print(burnout_std, short=FALSE)
burnout_std$a
burnout_std$b
burnout_std$c
burnout_std$cprime
burnout_std$ab



