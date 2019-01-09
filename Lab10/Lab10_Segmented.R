
#Lab10: Segmented Regression

library(SDSRegressionR)
#Bring in data
gss <- read_csv("data/GSSData.csv")
names(gss)

#Establish cut-off
cutoff <- 12

#Code the data for the cut-off
gss2 <- gss %>% 
  mutate_at(vars(educ), as.numeric) %>% #Initial catch all for numeric...
  mutate(educ1 = educ, #Simple replication
         educ2 = educ - cutoff, #Start second segment counting...
         educ2 = case_when(educ1 <= cutoff ~ 0, #Make sure to start at zero BEFORE segment
                          TRUE ~ educ2),
         jump = case_when(educ < cutoff ~ 0, #Define the segment status...
                          educ >= cutoff ~ 1))
#Check
plyr::count(gss2, c("educ", "educ1", "educ2", "jump"))

#Inital model run and diagnostics
full <- lm(realrinc ~ educ1 + jump + educ2, data=gss2)
residFitted(full)
cooksPlot(full, key.variable="UID", print.obs=TRUE, sort.obs = TRUE)
threeOuts(full, key.variable="UID")

#Get good data...
g_gss <- gss2 %>% 
  filter(UID %not in% c('ID_27289','ID_26725','ID_11432','ID_11075'))

#Initial look
simpleScatter(g_gss, educ, realrinc, title="Raw Data")

#Look with means
mns_gss <- g_gss %>%
  group_by(educ) %>%
  summarise(mean = mean(realrinc, na.rm=TRUE))

g_mns <- simpleScatter(mns_gss, educ, mean, title="Means Plot")
g_mns

#Run the model
seg <- lm(realrinc ~ educ1 + jump + educ2, data=g_gss)
summary(seg)

#Come up with prediction lines
library(skimr)
g_gss %>%
  skim(educ)

library(emmeans)
p1 <- summary(emmeans(seg, "educ1", at=list(educ1=c(0, cutoff), educ2=0, jump=0)))
p2 <- summary(emmeans(seg, "educ1", at=list(educ1=c(cutoff, 20), educ2=c(0, (20-cutoff)), jump=1),
                      by="educ2"))
p2 <- p2 %>% #Just the first and last row
  slice(c(1,4))

#Graph it!
g <- simpleScatter(g_gss, educ, realrinc, title="Raw Data")
g + 
  labs(subtitle="Segmented Regression") +
  geom_vline(xintercept = 12, linetype="dashed", color="green") +
  geom_line(data=p1, aes(x=educ1, y=emmean), color="red") + 
  geom_line(data=p2, aes(x=educ1, y=emmean), color="red")

#Graph it! MEANS
g_mns + 
  labs(subtitle="Segmented Regression") +
  geom_vline(xintercept = 12, linetype="dashed", color="green") +
  geom_line(data=p1, aes(x=educ1, y=emmean), color="red") + 
  geom_line(data=p2, aes(x=educ1, y=emmean), color="red")

#Code for slope of zero
g_gss <- g_gss %>% 
  mutate(educ1_is = case_when(educ >= cutoff ~ cutoff,
                             TRUE ~ educ))

#Re-run model
seg_is <- lm(realrinc ~ educ1_is + jump + educ2, data=g_gss)
summary(seg_is)
