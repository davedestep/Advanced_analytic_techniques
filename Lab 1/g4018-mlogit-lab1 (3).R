#gss = read.csv(file.choose()) ## choose GSS.2006.csv

setwd("C:\\Users\\daved\\Documents\\Advanced analytic techniques")
gss<-read.csv("GSS.2006.csv")
install.packages("mlogit")
library(mlogit)
library(knitr)
library(tidyverse)
library(stargazer)


table(gss$natchld)
summary(gss$age)

gss %>% ggplot(aes(x=age)) + geom_histogram(binwidth=3, color="grey") + ggtitle("Histogram of age of GSS respondents")


gss$rnatchld = 4-gss$natchld # Reversing the scale
gss2 = mlogit.data(gss, varying=NULL, choice="rnatchld", shape="wide")

## testing the grey peril hypothesis ##
ml1 = mlogit(rnatchld ~ 1 | age + educ + sex + prestg80 + as.factor(region), data=gss2, reflevel="2")
summary(ml1)
stargazer(ml1, type="text", report = "vc*")

test = ((0.01872522-0.00445479)^2)/(0.00472158^2 + 0.00259763^2)
test
pchisq(test, df = 1, lower.tail = FALSE)

## add in children ##
ml2 = mlogit(rnatchld ~ 1 | age + childs + educ + sex  + prestg80 + as.factor(region), data=gss2, reflevel="2")
summary(ml2)
stargazer(ml2, type="text", report = "vc*")

test2 = (( 0.01692479-0.00632173)^2)/(0.00507468^2 + 0.00281551^2)
test2
pchisq(test2, df = 1, lower.tail = FALSE)

## interact children with age ##
ml3 = mlogit(rnatchld ~ 1 | age + childs + age*childs + educ + sex  + prestg80 + as.factor(region), data=gss2, reflevel="2")
summary(ml3)
stargazer(ml3, type="text", report = "vc*")


mlundadjusted = mlogit(rnatchld ~ 1 | age +as.factor(wrkstat), data=gss2, reflevel="2")
stargazer(mlundadjusted, type="text", report = "vc*")






install.packages("nnet")
library(nnet)

gss$ncok <- relevel(as.factor(gss$rnatchld), ref = 2)

## same as above ##
mult1 = multinom(ncok ~ age + educ + sex + prestg80 + as.factor(region), data=gss)
summary(mult1)

z1 <- summary(mult1)$coefficients/summary(mult1)$standard.errors
z1

options(scipen=999) ## if you want to revert back, use options(scipen=0) 
p1 <- (1 - pnorm(abs(z1), 0, 1))*2
p1

pred1 <- predict(mult1, type = "probs")
head(pred1) ## overall probabilities

# data frame of values to use for predictions
data.child <- expand.grid(
  age = 20:95, # let age vary from 20 to 95 
  region = 3,  # set region to east north central 
  sex = 1, # fix sex as male
  prestg80 = mean(gss$prestg80,na.rm=T), # fix prestige at its mean
  educ = mean(gss$educ,na.rm=T)) # fix educ at its mean


# combine age and predicted probabilities in a data frame
preds.child <- data.frame( 
  age = data.child$age, # age
  predict(mult1, newdata = data.child, type = "probs", se = TRUE)) # predicted probabilities

install.packages("plyr")
library(plyr)

# avg predicted probabilities for each level of age
ddply(preds.child, "age", colMeans) 

table(gss$rnatchld) ## sanity check, small-medium-large in precentages

