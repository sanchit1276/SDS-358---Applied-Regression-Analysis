
#Lab8: Quantitative Interaction

library(SDSRegressionR)

#Load Data
conn <- read_csv("data/connected.csv")
names(conn)


#Run first model for diagnostics
#First run
mod1 <- lm(Problem.Beh ~ age + female + School.Connectedness + Family.Connectedness + Depressive.Sympt+School.Connectedness*Depressive.Sympt, data=conn)
summary(mod1)

#Diagnostics
library(car)
vif(mod1)
residFitted(mod1)
cooksPlot(mod1, key.variable = "STUD_ID", print.obs=TRUE, sort.obs=TRUE, save.cutoff=TRUE)
cooksCutOff * 2

#Remove bad outlier(s)
g_conn <- conn %>% 
  filter(STUD_ID %not in% c("SID192","SID134"))

#Re-run
mod2 <- lm(Problem.Beh ~ age + female + School.Connectedness + Family.Connectedness + Depressive.Sympt+School.Connectedness*Depressive.Sympt, data=g_conn)
summary(mod2)

#Find the Simple Slope locations (Pick-a-Point)
m_data <- modelData(mod2)
mean(m_data$School.Connectedness, na.rm=TRUE)
sd(m_data$School.Connectedness, na.rm=TRUE)
mean(m_data$School.Connectedness, na.rm=TRUE) - sd(m_data$School.Connectedness, na.rm=TRUE)
mean(m_data$School.Connectedness, na.rm=TRUE) + sd(m_data$School.Connectedness, na.rm=TRUE)

#lsmeans for simple slopes
library(emmeans)
simple_mns <- emmeans(mod2, "Depressive.Sympt", 
                      at=list(Depressive.Sympt=c(0,1), School.Connectedness=c(2.68678,4.409509, 6.132239)), 
                      by="School.Connectedness")
simple_mns
pairs(simple_mns, reverse=TRUE)

# Regions of Significance
library(jtools)
johnson_neyman(mod2, pred = "Depressive.Sympt", modx = "School.Connectedness")


#Double-check work
sim_slopes(mod2, pred = "Depressive.Sympt", modx = "School.Connectedness")
ss <- sim_slopes(mod2, pred = "Depressive.Sympt", modx = "School.Connectedness")
ss$slopes
interact_plot(mod2, pred = "Depressive.Sympt", modx = "School.Connectedness", plot.points = TRUE)
