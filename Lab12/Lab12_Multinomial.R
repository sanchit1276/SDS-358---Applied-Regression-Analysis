
#Lab12: Ordinal/Multinomial Logistic Regression

library(SDSRegressionR)

#Data
emp <- read_csv("data/employment.csv")

#Examine
table(emp$age)
table(emp$mhs)
table(emp$fhs)
table(emp$adjinc)
table(emp$wtest)
table(emp$empl)

#Factor
emp <- emp %>% 
  mutate(mhs = factor(mhs, levels=c(0,1)),
         fhs = factor(fhs, levels=c(0,1)),
         empl = factor(empl, levels=c(0,1,2)))

#Model
library(nnet)
m_emp <- multinom(empl ~ age + mhs + fhs + adjinc + wtest, data = emp)
summary(m_emp)

#Overall
deviance(m_emp)
deviance(multinom(empl~1, data=emp))
x2 <- deviance(multinom(empl~1, data=emp)) - deviance(m_emp)
x2
pchisq(x2, 6, lower.tail=FALSE)

#Odds-ratios
exp(coef(m_emp))

#Individual paramerters
z <- summary(m_emp)$coefficients/summary(m_emp)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
round(p, 6)

#Change the baseline:
emp <- emp %>% 
  mutate(empl_employed = factor(empl, levels=c(1,0,2)))

#Model_employed
m_emp_e <- multinom(empl_employed ~ age + mhs + fhs + adjinc + wtest, data = emp)
summary(m_emp_e)

#Individual paramerters_Leave
z <- summary(m_emp_e)$coefficients/summary(m_emp_e)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
round(p, 6)

#Odds-ratios_Leave
exp(coef(m_emp_e))

#Graphing
library(emmeans)
wtest_mns <- summary(emmeans(m_emp, c("empl", "wtest"), 
                           at=list(wtest=seq(-3,3,.1))))

ggplot(wtest_mns, aes(y=prob, x=wtest, color=empl)) +
  geom_line() +
  labs(title="Wtest impact") +
  theme_bw()

#predictions
emmeans(m_emp, c("empl", "wtest"),at=list(wtest=1))

