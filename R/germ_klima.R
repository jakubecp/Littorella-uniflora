rm(list=ls())
# reshaping the data to be in tidy format
library(dplyr) # manipulation with original data
library (lazyeval) # manipulation with original data
library(tidyr) # manipulation with original data
library(reshape2) # manipulation with original data
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
library (broom)
data=read.csv ("resubmision/kliceni_klima.csv", header=TRUE, sep=",") 
head(data)

# #treat will be left out and not melted, but rest will be
# substr <- melt (data, c("loc", "dish")) 
# 
# head (substr) #see how it looks like
# names (substr) <- c("loc", "dish", "date", "germ") #meaningful names of variables
# #data exploration
# summary (substr)
# mean (substr$germ [substr$loc == "ka"])
# str (substr)
# plot (substr$loc, substr$germ)
# 
# #results will go into this data frame and will be used for further analysis
# levels (substr$loc)
# treat <- c("ka", "kr", "la", "mr", "no", "os", "ry", "st")
# germ <- c(sum (substr$germ [substr$loc == "ka"]),
#           sum (substr$germ [substr$loc == "kr"]),
#           sum (substr$germ [substr$loc == "la"]),
#           sum (substr$germ [substr$loc == "mr"]),
#           sum (substr$germ [substr$loc == "no"]),
#           sum (substr$germ [substr$loc == "os"]),
#           sum (substr$germ [substr$loc == "ry"]),
#           sum (substr$germ [substr$loc == "st"]))
# total <- c(rep (75, 8))
# p=germ/total #probability of succesfull germination
# negative <- total-germ #how many did not germinate
# 
# data_klima_germ<- data.frame (treat, germ, total, negative, p) # dataframe of resulting values
# y<-cbind(germ, total - germ) # this vector should be feedid into binomial model

#binomial model and its p-values, "a" is control so every results is compared with control by summary function
mod <- glm(cbind(succ, fail) ~ loc, data=data,family=binomial)
mod
summary(mod)
anova(mod, test="Ch")
X11()
par(mfrow=c(2,2))
plot(mod)
rd=residuals(mod)
plot(rd)
qqnorm(residuals(mod, type="deviance"))
abline(a=0,b=1)

#plogis - "ka", "kr", "la", "mr", "no", "os", "ry", "st"
plogis (mod$coefficients[1]) #intercept (ka)
plogis (mod$coefficients[1]+mod$coefficients[2]) #kr
plogis (mod$coefficients[1]+mod$coefficients[3]) #la
plogis (mod$coefficients[1]+mod$coefficients[4]) #mr
plogis (mod$coefficients[1]+mod$coefficients[5]) #no
plogis (mod$coefficients[1]+mod$coefficients[6]) #os
plogis (mod$coefficients[1]+mod$coefficients[7]) #ry
plogis (mod$coefficients[1]+mod$coefficients[8]) #st

#barplot of mean or median germination success across different substrates.#summarySE is function, which is preparing data to be ploted with SE or confidence intervals...
sumary.dev = summarySE (data, measurevar="succ", groupvars="loc")

tiff (filename="outputs/germ_clima_pond2.tiff", 
  width=5000, height=3500, 
  compression="lzw", res= 800)
p = ggplot (sumary.dev, aes (y=succ, x=loc))
p + stat_summary(fun.y=mean, geom="bar", position=position_dodge())+
  xlab("Locality")+
  ylab("Mean number of germinated seeds")+
  geom_errorbar(aes(ymin=succ-se, ymax=succ+se),
    width=.2,                    # Width of the error bars
    position=position_dodge(.9))
dev.off()
