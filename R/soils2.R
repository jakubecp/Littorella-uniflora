rm(list=ls())
library(checkpoint)
# checkpoint ("2016-02-11", use.knitr = T)
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
library (broom)
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

#summary for ploting
sumary.dev = summarySE (data, 
  measurevar="succ", groupvars="treat")

#ploting in ggplot2
# tiff (filename="outputs/substrates_germination_barplots.tiff", 
#   width=5000, height=3500, 
#   compression="lzw", res= 800)
p = ggplot (sumary.dev, aes (y=succ, x=treat))
p2=p + stat_summary(fun.y=mean, geom="bar", position=position_dodge())+
  xlab("Germination substrate")+
  ylab("Mean nuber of germinated seeds")+
  geom_errorbar(aes(ymin=succ-se, ymax=succ+se),
    width=.2,                    # Width of the error bars
    position=position_dodge(.9))
p3=p2+ coord_flip()
p3+scale_x_discrete(labels=c("Control", "Sand+Clay","Sand+Peat", "Sand+Pond mud", "Pond mud", "Sand", "Clay", "Peat", "Topsoil"))
# dev.off()