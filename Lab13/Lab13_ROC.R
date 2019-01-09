#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab13: ROC and AUC
library(SDSRegressionR)

#Read Data
loans <- read_csv("data/historicLoans.csv")

#Factor where needed:
table(loans$loanIndicator)
table(loans$purpose)
loans <- loans %>% 
  mutate(good = factor(loanIndicator, levels=c(0,1), labels=c("Bad Borrower", "Good Borrower")),
         purpose_f = factor(purpose))
table(loans$good)

#First model:
mod1 <- glm(good ~ fico + dti+ loan_amnt + purpose_f, data = loans, family = "binomial")
summary(mod1)

#IF we wanted to remove outliers...
cooksPlot(mod1, key.variable = "LoanID", print.obs = TRUE)

#Best Predictor
library(car)
Anova(mod1, type="III")

#ROC for Fico Score
fico_mod <- glm(good ~ fico, data = loans, family = "binomial")
summary(fico_mod)
exp(coef(fico_mod))

library(emmeans)
emmeans(fico_mod, "fico", at=list(fico = seq(500, 850, 25)), type = "response")


#ROC
library(pROC)
#get the data from the model.
fico_d <- modelData(fico_mod)
levels(fico_d$good)
r_fico <- roc(fico_d$good, fico_d$fico, ci = TRUE, levels=c("Bad Borrower", "Good Borrower"))
r_fico

#Plot with Youden
plot.roc(r_fico, print.thres="best", print.thres.best.method="youden")

#Let's use a TESTING data set
loan_test <- read_csv("data/historicLoans_Testing.csv")

#Original Factoring
loan_test <- loan_test %>% 
  mutate(good = factor(loanIndicator, levels=c(0,1), labels=c("Bad Borrower", "Good Borrower")),
         purpose_f = factor(purpose))

#Predict a Good Borrower
loan_test <- loan_test %>% 
  mutate(pred = predict(fico_mod, loan_test, type="response")) %>% 
  mutate(good_pred = case_when(fico > 709.5 ~ "Good Borrower",
                               TRUE ~ "Bad Borrower"))

#Used for Specificity and Sensitivity
t <- table(select(loan_test, good_pred, good))[2:1, 2:1]
t
accuracy <- (2158 + 452) / sum(t)
