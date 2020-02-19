##install.packages("data.table")
setwd("C:\\Users\\daved\\Documents\\Advanced analytic techniques")
library(data.table)

vars <- c("memnum", "educ", "year", "sex", "relig", "age")

d <- data.table::fread(
  file.choose(), ## pick GSS-Cum.zip on Coursework under Data, unzip it and use the .csv##
  sep = ",",
  select = vars,
  data.table = FALSE) 

head(d)

d$norelig = ifelse(d$relig==4,1,0)
d$seventies = ifelse(d$year<1980,1,0)
d$eighties = ifelse(d$year>1979 & d$year<1990,1,0)
d$nineties = ifelse(d$year>1989 & d$year<2000,1,0)
d$aughts = ifelse(d$year>=2000,1,0)
d$female = ifelse(d$sex==2,1,0)


ols1 = lm(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d)
summary(ols1)


gls1 = glm(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d, family="gaussian")
summary(gls1)

p1 = glm(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d, family="poisson")
summary(p1)

##install.packages("MASS")
library(MASS)

nb1 = glm.nb(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d)
summary(nb1)

nb2 = glm.nb(memnum ~ female + eighties + nineties + aughts + educ + norelig + offset(log(age)), d)
summary(nb2) 

##install.packages("pscl")
library(pscl)

znb1 <- zeroinfl(memnum ~ female + eighties + nineties + aughts + educ + norelig | age,
                 data = d, dist = "negbin", EM = TRUE)
summary(znb1)

install.packages("AICcmodavg")
library(AICcmodavg)
AICc(znb1, return.K = FALSE, second.ord = FALSE)

## predicted probabilities to come, or look on Slide #3, p. 18, for code -- you might need mlogit code too ##