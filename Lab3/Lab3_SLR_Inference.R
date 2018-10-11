#### Here is the R script you will use:  (remember that # indicates a comment) ####
##Lab 3: Simple Regression, Inference
library(SDSRegressionR)
library(tidyverse)

res <- read_csv("data/Resilience.csv")

#Subset the data
basic <- res %>% 
  filter(Group == "Basic Sciences")

-----------

#Take an intial look at the variables 
library(psych)
corr.test(select(basic, MS.QoL,DREEM.A.SP),use="pairwise.complete.obs")$r

#Check linearity
simpleScatter(basic, DREEM.A.SP,MS.QoL,line=TRUE)

#Run the intial model
mod <- lm(MS.QoL ~ DREEM.A.SP, data=basic)
summary(mod)

#Look for outliers
threeOuts(mod, key.variable = "IDR")
cooksPlot(mod, key.variable = "IDR", print.obs = TRUE)

#Remove the offending outlier
basic_noout <- basic %>% 
  filter(IDR %not in% c("IDR1058"))

#Re-run the model
mod2 <- lm(MS.QoL ~ DREEM.A.SP, data=basic_noout)
summary(mod2)

#Grab confidence intervals
confint(mod2)

#additional questions

library(car)

nw_sp <- data.frame(DREEM.A.SP=10)
predict(mod2, nw_sp, interval="prediction")

linearHypothesis(mod2, "DREEM.A.SP=0.175")
