rm(list=ls())
library(checkpoint)
checkpoint ("2016-02-11", use.knitr = T)
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
library(arm)
library (broom)
library(blmeco) #bayesiand data analysis book
library(rmarkdown)
library(knitr)
#load the data
data=read.csv ("resubmision/prolicovani_ruzne_substraty_2.csv", header=TRUE, sep=";") 

#explore the data
head(data)
str(data)

#put together some model
mod <- glm(cbind(succ, fail) ~ treat, data=data,family=binomial)
mod
summary(mod)
anova(mod, test="Ch")

#see what is going on with residuals
X11()
par(mfrow=c(2,2))
plot(mod)
rd=residuals(mod)
plot(rd)
qqnorm(residuals(mod, type="deviance"))
abline(a=0,b=1)

#back-transformation of coeficients for each treatment to mean survival probability
str(mod)
plogis (mod$coefficients[1]) #intercept (clay)
plogis (mod$coefficients[1]+mod$coefficients[2]) #control
plogis (mod$coefficients[1]+mod$coefficients[3]) #mud
plogis (mod$coefficients[1]+mod$coefficients[4]) #peat
plogis (mod$coefficients[1]+mod$coefficients[5]) #sand
plogis (mod$coefficients[1]+mod$coefficients[6]) #sand_clay
plogis (mod$coefficients[1]+mod$coefficients[7]) #sand_mud
plogis (mod$coefficients[1]+mod$coefficients[8]) #sand_peat
plogis (mod$coefficients[1]+mod$coefficients[9]) #topsoil

