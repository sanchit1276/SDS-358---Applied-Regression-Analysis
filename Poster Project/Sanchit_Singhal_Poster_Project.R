
# Poster Project by Sanchit

library(SDSRegressionR)

##Data Pre-processing

#import data...
vid <- read_csv("USvideos.csv")
names(vid)

#Examine the categorical variables:
table(vid$category_id, useNA = "always")

#Factor
vid <- vid %>% 
  mutate(category_id = factor(category_id))

#Determine and run the full model to check for assumptions
full <- lm(comment_count ~ category_id + views + likes + dislikes, data=vid)

#Look for any issues:
library(car)
vif(full)
residFitted(full)

cooks <- cooksPlot(full, key.variable = "video_id", print.obs = TRUE, 
          sort.obs=TRUE, save.cutoff = TRUE)

#Clean up
g_vid <- vid %>% 
  filter(video_id %not in% cooks$video_id)

#Re-run the model
fullg <- lm(comment_count ~ category_id + views + likes + dislikes, data=g_vid)

#Re-check assumptions
vif(fullg)
residFitted(fullg)
cooks <- cooksPlot(fullg, key.variable = "video_id", print.obs = TRUE, 
                   sort.obs=TRUE, save.cutoff = TRUE)

#Clean up again

g_vid2 <- g_vid %>% 
  filter(video_id %not in% c('p8npDG2ulKQ', 'GnGPAYvve1A'))


#Re-run the model
fullg <- lm(comment_count ~ category_id + views + likes + dislikes, data=g_vid2)

#Re-check assumptions
vif(fullg)
residFitted(fullg)
cooksPlot(fullg, key.variable = "video_id", print.obs = FALSE, 
                   sort.obs=TRUE, save.cutoff = TRUE)

#Get the "model data" for the nesting
good_vid_m2 <- modelData(fullg)

##Descriptives of data

#Descriptives table
library(skimr)
good_vid_m2 %>% 
  skim(comment_count, likes, dislikes, views)

#Examine the categorical variable:
table(good_vid_m2$category_id)

#Graphs of Raw Data
simpleScatter(good_vid_m2, likes, comment_count, title="Likes vs Comments")
simpleScatter(good_vid_m2, dislikes, comment_count, title="Dislikes vs Comments")

# 3D Scatterplot
library(scatterplot3d) 
attach(mtcars) 
s3d <-scatterplot3d(good_vid_m2$likes,good_vid_m2$dislikes,good_vid_m2$comment_count, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot of Variables of Interest", xlab='Number of Likes', zlab='Number of Comments', ylab = 'Number of Dislikes')

##Now for the Sequential Regression Model runs:

#Model 1 - only cat and views:
m1_seq <- lm(comment_count ~ category_id + views, data=good_vid_m2)
summary(m1_seq)
summary(m1_seq)$r.squared*100
lmBeta(m1_seq)
pCorr(m1_seq)

#Model 2 - both likes and dislikes:
m2_seq <- lm(comment_count ~ category_id + views + likes + dislikes, data=good_vid_m2)
summary(m2_seq)
summary(m2_seq)$r.squared*100
lmBeta(m2_seq)
pCorr(m2_seq)

#Model 3 - only likes:
m3_seq <- lm(comment_count ~ category_id + views + likes, data=good_vid_m2)
summary(m3_seq)
summary(m3_seq)$r.squared*100
lmBeta(m3_seq)
pCorr(m3_seq)

#Model 4 - only dislikes:
m4_seq <- lm(comment_count ~ category_id + views + dislikes, data=good_vid_m2)
summary(m4_seq)
summary(m4_seq)$r.squared*100
lmBeta(m4_seq)
pCorr(m4_seq)

##Now the Sequential Results

#both likes and dislikes
(summary(m2_seq)$r.squared - summary(m1_seq)$r.squared)*100
anova(m1_seq, m2_seq)

#only likes
(summary(m3_seq)$r.squared - summary(m1_seq)$r.squared)*100
anova(m1_seq, m3_seq)

#only dislikes
(summary(m4_seq)$r.squared - summary(m1_seq)$r.squared)*100
anova(m1_seq, m4_seq)


simpleAnova(m1_seq)
simpleAnova(m2_seq)
simpleAnova(m3_seq)
simpleAnova(m4_seq)

# Diagrams to help explain results

# Pie Chart with Percentages
slices <- c(34.25, 43.68, 22.07) 
lbls <- c("Likes+Dislikes", "Cat+Views", "Unexplained")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Variance in Number of Comments")

# 3D Exploded Pie Chart
library(plotrix)
slices <- c(34.25, 43.68, 22.07) 
lbls <- c("Likes+Dislikes", "Cat+Views", "Unexplained")
pie3D(slices,labels=lbls,explode=0.1,
      main="Variance in Number of Comments")


