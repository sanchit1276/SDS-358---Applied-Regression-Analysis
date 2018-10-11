
#Lab5: Sequential Regression

library(SDSRegressionR)

#import data...
int <- read_csv("data/introverts.csv")
names(int)

#Determine and run the full model to check for assumptions
full <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=int)

#Look for any issues:
library(car)
vif(full)
residFitted(full)
cooksPlot(full, key.variable = "SubjectID", print.obs = TRUE, 
          sort.obs=TRUE, save.cutoff = TRUE)
cooksCutOff * 3
threeOuts(full, key.variable = "SubjectID")

#Clean up
good_int <- int %>% 
  filter(SubjectID %not in% c("ID320", "ID198", "ID252", "ID316"))

#Re-run the final model
fullg <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=good_int)
summary (fullg)

#Get the "model data" for the nesting
good_int_m2 <- modelData(fullg)

#Now for the Sequential Regression:
#Model 1:
m1_seq <- lm(Happiness ~ Age + ERA + QSR, data=good_int_m2)
summary(m1_seq)
summary(m1_seq)$r.squared*100
lmBeta(m1_seq)
pCorr(m1_seq)

#Model 2:
m2_seq <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=good_int_m2)
summary(m2_seq)
summary(m2_seq)$r.squared*100
lmBeta(m2_seq)
pCorr(m2_seq)

(summary(m2_seq)$r.squared - summary(m1_seq)$r.squared)*100
#Now the Sequential Results
anova(m1_seq, m2_seq)

simpleAnova(m1_seq)
simpleAnova(m2_seq)
