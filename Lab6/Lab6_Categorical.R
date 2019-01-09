
#Lab6: Categorical Variables

library(SDSRegressionR)

#Import data...
dent <- read_csv("data/dentalAnxiety.csv")
names(dent)

#Examine the categorical variables:
table(dent$Sex, useNA = "always")
table(dent$Marriage, useNA = "always")
table(dent$Education, useNA = "always")

#Run the model with FACTOR VARIABLE (1 as reference)
dent <- dent %>% 
  mutate(Education_f = factor(Education, levels=c(1,2,3,4)))

dent_f <- lm(DFS ~ Age + Sex + BDI + Education_f, data=dent)
summary(dent_f)

#Check the model...
library(car)
vif(dent_f)
residFitted(dent_f)
cooksPlot(dent_f, key.variable = "UniqueID", print.obs=TRUE, sort.obs = TRUE, save.cutoff = TRUE)
cooksCutOff * 2

#Drop the outliers
g_dent <- dent %>% 
  filter(UniqueID %not in% c("SUB178", "SUB103", "SUB284"))

#Rerun
dent2_f <- lm(DFS ~ Age + Sex + BDI + Education_f, data=g_dent)
summary(dent2_f)

#Overall ANOVA
library(car)
Anova(dent2_f, type="III")

#Predictor Unique R^2
pCorr(dent2_f)

#Education R^2
lmSingleR2(dent2_f, "Education_f")

#Post-hoc exploration
library(emmeans)
dent2_f_mn <- emmeans(dent2_f, "Education_f")
dent2_f_mn
pairs(dent2_f_mn, adjust="holm") #Just right
