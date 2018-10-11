#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab4: Multiple Linear Regression
library(SDSRegressionR)
library(tidyverse)

#import data...
vit <- read_csv("data/VitaminD.csv")
names(vit)

#Inital correlationss
vars <- c("D25","ageyears", "D125", "weight", "bmi", "pth")
library(psych)
corr.test(select(vit, one_of(vars)))

#First model
q_mod <- lm(D25 ~ ageyears + D125 + weight + bmi + pth, data=vit)
summary(q_mod)
library(car)
vif(q_mod)
1-1/vif(q_mod)

#Remove the problem independent:
q_mod2 <- lm(D25 ~ ageyears + D125 + bmi + pth, data=vit)
summary(q_mod2)
vif(q_mod2)

#Good model: Check assumptions
residFitted(q_mod2)

#Find the outliers...
cooksPlot(q_mod2, key.variable = "PartID", print.obs = TRUE, sort.obs = TRUE)
threeOuts(q_mod2, key.variable = "PartID")

#Remove the outlier(s)
good_vit <- vit %>% 
  filter(PartID %not in% c("ID178"))

#Final Model
q_mod_f <- lm(D25 ~ ageyears + D125 + bmi + pth, data=good_vit)
summary(q_mod_f)
residFitted(q_mod_f) #Just checking
confint(q_mod_f) #Confidence intervals for the slopes (for reporting)
lmBeta(q_mod_f) #Standardized Betas for our final model
pCorr(q_mod_f) #Partial and Part correlation coefficients

#Predict
library(emmeans)
ref_grid(q_mod_f)
emmeans(q_mod_f, "pth", at=list(pth=80))