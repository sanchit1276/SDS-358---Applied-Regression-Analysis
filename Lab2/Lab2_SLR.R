
##Lab 2: Simple Regression
library(SDSRegressionR)
library(tidyverse)

#Read in the data 
edss <- read_csv("data/ms_edss.csv")

#Run a correlation
library(psych)
round(cor(select(edss,delta_edss, delta_TotalGrayVol), 
    use="pairwise.complete.obs"),4)

corr.test(select(edss,delta_edss, delta_TotalGrayVol), 
          use="pairwise.complete.obs")$t

corr.test(select(edss,delta_edss, delta_TotalGrayVol), 
          use="pairwise.complete.obs")$p

#Check for linearity
simpleScatter(edss, delta_edss, delta_TotalGrayVol, line=TRUE)

#r2
r2 = 0.2142256*0.2142256
round(r2*100,2)

#Run a Simple Linear Regression model
c_mod <- lm(delta_edss ~ delta_TotalGrayVol, edss)
c_mod

#Check for Homoscedastity
residFitted(c_mod)

#Check for outliers
studResidPlot(c_mod, key.variable = "PatID", print.obs = TRUE, sort.obs = TRUE)
levPlot(c_mod, key.variable = "PatID", print.obs = TRUE, sort.obs = TRUE)
cooksPlot(c_mod, key.variable = "PatID", print.obs = TRUE, sort.obs = TRUE)
threeOuts(c_mod, key.variable = "PatID")

#Remove "outliers"
"%not in%" <-Negate("%in%")
edss_nout <- edss %>% 
  filter(PatID %not in% c(42, 90, 120))

#Re-run the correlations to see a change
round(cor(select(edss_nout,delta_edss, delta_TotalGrayVol), 
          use="pairwise.complete.obs"),4)

r <- cor(select(edss_nout,delta_edss, delta_TotalGrayVol), 
         use="pairwise.complete.obs")

r2 = r*r
r2

corr.test(select(edss_nout,delta_edss, delta_TotalGrayVol), 
          use="pairwise.complete.obs")$t

corr.test(select(edss_nout,delta_edss, delta_TotalGrayVol), 
          use="pairwise.complete.obs")$p

#Re-run the Simple Linear Regression Model
simpleScatter(edss_nout, delta_edss, delta_TotalGrayVol, line=TRUE)
c_mod2 <- lm(delta_edss ~ delta_TotalGrayVol, edss_nout)
c_mod2

# test delta_SubCortGrayVol

round(cor(select(edss_nout,delta_edss, delta_SubCortGrayVol), 
          use="pairwise.complete.obs"),4)

corr.test(select(edss_nout,delta_edss, delta_SubCortGrayVol), 
          use="pairwise.complete.obs")$t

corr.test(select(edss_nout,delta_edss, delta_SubCortGrayVol), 
          use="pairwise.complete.obs")$p

simpleScatter(edss_nout, delta_edss, delta_TotalGrayVol, line=TRUE)
c_mod3 <- lm(delta_edss ~ delta_TotalGrayVol, edss_nout)
c_mod3
