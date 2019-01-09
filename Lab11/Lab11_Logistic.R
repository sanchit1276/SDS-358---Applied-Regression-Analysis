
#Lab11: Multiple Logistic Regression

library(SDSRegressionR)
#Bring in data
prostate <- read.csv("data/prostateData.csv", stringsAsFactors = FALSE)
names(prostate)

#Intially:
table(prostate$CAPSULE)

#Intital Model
b_mod <- glm(CAPSULE ~ AGE + PSA + VOL + GLEASON, data=prostate, family="binomial")
summary(b_mod)

library(car)
vif(b_mod)
cooksPlot(b_mod, key.variable = "PatientID", print.obs = TRUE, sort.obs = TRUE, save.cutoff=TRUE)
cooksCutOff * 3
threeOuts(b_mod, key.variable = "PatientID")

#Get good data...
g_prostate <- prostate %>% 
  filter(PatientID %not in% c('ID_76', 'ID_89','ID_8'))

#Stats
library(rms)
b_mod2.2 <- lrm(CAPSULE ~ AGE + PSA + VOL + GLEASON, g_prostate)
b_mod2.2

#Re-run
b_mod2 <- glm(CAPSULE ~ AGE + PSA + VOL + GLEASON, data=g_prostate, family="binomial")
summary(b_mod2)

#Odds-ratios
exp(b_mod2$coef)
exp(confint.default(b_mod2))


#Examine the variables of interest graphically...
#Look at ranges...
library(skimr)
g_prostate %>% 
  skim(PSA)

g_prostate %>% 
  skim(GLEASON)

library(emmeans)

prostate_mns <- summary(emmeans(b_mod2, "PSA",
                                at=list(PSA = seq(0, 160, 0.001)), type="response"))

g <- simpleScatter(g_prostate, PSA, CAPSULE, title="CAPSULE",
                   xlab="PSA", ylab="CAPSULE probability")

mark_prob <- 0.5

ci_marks <- prostate_mns %>% 
  filter(abs(asymp.LCL - mark_prob) == min(abs(asymp.LCL - mark_prob)) |
           abs(asymp.UCL - mark_prob) == min(abs(asymp.UCL - mark_prob))) %>% 
  pull(PSA)

g + 
  geom_line(data=prostate_mns, aes(x=PSA, y=prob), color="red") + 
  geom_line(data=prostate_mns, aes(x=PSA, y=asymp.LCL), linetype="dashed") +
  geom_line(data=prostate_mns, aes(x=PSA, y=asymp.UCL), linetype="dashed") +
  geom_vline(xintercept = ci_marks)


prostate_mns_gleason <- summary(emmeans(b_mod2, "GLEASON",
                                        at=list(GLEASON = seq(0, 10, .001)), type="response"))

g_gleason <- simpleScatter(g_prostate, GLEASON, CAPSULE, title="CAPSULE",
                   xlab="GLEASON", ylab="CAPSULE probability")

ci_marks_gleason <- prostate_mns_gleason %>% 
  filter(abs(asymp.LCL - mark_prob) == min(abs(asymp.LCL - mark_prob)) |
           abs(asymp.UCL - mark_prob) == min(abs(asymp.UCL - mark_prob))) %>% 
  pull(GLEASON)

g_gleason + 
  geom_line(data=prostate_mns_gleason, aes(x=GLEASON, y=prob), color="red") + 
  geom_line(data=prostate_mns_gleason, aes(x=GLEASON, y=asymp.LCL), linetype="dashed") +
  geom_line(data=prostate_mns_gleason, aes(x=GLEASON, y=asymp.UCL), linetype="dashed") +
  geom_vline(xintercept = ci_marks_gleason)

