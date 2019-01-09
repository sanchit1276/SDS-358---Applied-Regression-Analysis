
#Lab7: Categorical Interaction

library(SDSRegressionR)

#Import data...
state <- read_csv("data/stateAnx.csv")

#Examine the categorical variable(s):
#Remember, it's good to do this for ALL categorical variables
table(state$Group)

#Run the Factoring for categorical variable:
# (Basic Sciences as Reference)
state <- state %>% 
  mutate(Group = factor(Group, levels=c("Basic Sciences", "Clinical Sciences")))


levels(state$Group)

#Initial Model:
#Run the model (Basic Sciences as Reference)
mod1 <- lm(State.Anxiety ~ Age + BDI + Group + WHOQOL.PSY + 
             Group*WHOQOL.PSY, data=state)
summary(mod1)

#Check the diagnostics/outliers
residFitted(mod1)
library(car)
vif(mod1)
c <- cooksPlot(mod1, key.variable="IDR", print.obs=TRUE, sort.obs=TRUE, save.cutoff=TRUE)
threeOuts(mod1, key.variable="IDR")
cooksCutOff * 3
c

#Get good data
g_state <- state %>% 
  filter(IDR %not in% c("IDR932", "IDR154", "IDR1289", "IDR639", "IDR686"))

#Re-run the model:
mod2 <- lm(State.Anxiety ~ Age + BDI + Group + WHOQOL.PSY + 
             Group*WHOQOL.PSY, data=g_state)
summary(mod2)

#Check the overall interaction significance
library(car)
Anova(mod2, type="III")

#Simple Slopes
library(emmeans)
ref_grid(mod2)
means <- emmeans(mod2, "WHOQOL.PSY", at=list(WHOQOL.PSY = c(0,1)), by="Group")
means

#Test of Simple Slopes
slopes <- pairs(means, reverse=TRUE)
slopes

# (difference of differences -- Interaction terms)
pairs(update(slopes, by=NULL), reverse=TRUE, adjust="none")

#CI Plot (for fun)
library(emmeans)
mns <- summary(emmeans(mod2, "WHOQOL.PSY", at=list(WHOQOL.PSY = seq(0,160,1)), by="Group"))
simpleScatter(g_state, x=WHOQOL.PSY, y=State.Anxiety, ptalpha = 0,
              title="WHOQOL.PSY and State.Anxiety",
              subtitle = "by Group") +
  geom_line(data=mns, aes(x=WHOQOL.PSY, y=emmean, color=Group)) +
  geom_ribbon(data=mns, aes(y=emmean, ymin=lower.CL, ymax=upper.CL, group=Group), 
              alpha=0.3) + 
  #Change to your group names and number of groups
  scale_colour_manual(name = "Groups", 
                      values =c("blue", "red"),
                      #IMPORTANT: Same order below as the factor()
                      labels = c("Basic", "Clinical")) 

#Or a straight ggplot...but this falls a little short...
ggplot(g_state,aes(x=WHOQOL.PSY,y=State.Anxiety,color=Group)) +
  stat_smooth(method=lm, se = FALSE, fullrange=TRUE) +
  geom_point() + 
  labs(title="Group Interaction", x="WHOQOL.PSY", y="State.Anxiety") +
  theme_bw()

#Mean value of Quality Life
ref_grid(mod2)
pairs(emmeans(mod2, "Group", at=list(WHOQOL.PSY = 62.625), by="WHOQOL.PSY"), reverse=TRUE)


