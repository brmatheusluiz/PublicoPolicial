library("haven")
library("dplyr")
library("tidyr")
library("ggplot2")
library("broom")


mydata <- read_dta("http://dss.princeton.edu/training/Panel101.dta") 
summary(mydata)

logit=glm(y_bin~x1+x2+x3, data=mydata, family = binomial(link="logit"))
summary(logit)




