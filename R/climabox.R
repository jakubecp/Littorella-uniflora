rm(list=ls())
#packages used for data analysis
# install.packages(c("reshape2", "ggplot2", "Rmisc"))
# library(dplyr) # manipulation with original data
# library (lazyeval) # manipulation with original data
# library(tidyr) # manipulation with original data
library(reshape2) # manipulation with original data
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
library (MASS) # dose.p function for estimation of limitting depth of soil layer
library (broom)
#loading data into R
substr=read.csv ("resubmision/proklicovani_klima.csv", header=TRUE, sep=",") 
head(substr)
substr$dish <- as.factor (substr$dish)
# reshaping the data to be in tidy format
#treat will be left out and not melted, but rest will be


head (substr) #see how it looks like
summary (substr)
mean (substr$germ [substr$depth == "b"])
str (substr)
X11()
plot (substr$depth, substr$germ)

#binomial model and its p-values, "a" is control so every results is compared with control by summary function
mod <- glm(cbind(succ, fail) ~ depth, data=substr,family=binomial)
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
#ploting original data (not model)
ggplot(substr, aes(x=depth, y=succ))

#plot of probability of germintion on depth of soil layer
ggplot (data_klima, aes (x=treat, y=p))+
  geom_point(size=3)

#Germination probability model coeficients
a<-plogis (mod$coefficients[1]) #intercept (control)
b<-plogis (mod$coefficients[1]+mod$coefficients[2]) #depth 1cm
x<-c(-10:10)
y <- a+b*x
data=data.frame (y,x)
ggplot (data, aes (y=y, x=x))+
  stat_summary(fun.y=mean, geom="line", position=position_dodge())
#germination decrease with increasing depth of soil layer
summary (m1)
dose.p (m1, cf=c(1:2), p=seq(0.05,0.9,0.05))

#barplot of mean or median germination success across different substrates.#summarySE is function, which is preparing data to be ploted with SE or confidence intervals...
sumary.dev = summarySE (substr, measurevar="succ", groupvars="depth")

tiff (filename="outputs/climabox_barplots_depth2.tiff", 
  width=5000, height=3500, 
  compression="lzw", res= 800)
p = ggplot (sumary.dev, aes (y=succ, x=depth))
p + stat_summary(fun.y=mean, geom="bar", position=position_dodge())+
  xlab("Depth of substrate (cm)")+
  ylab("Mean number of germinated seeds")+
  geom_errorbar(aes(ymin=succ-se, ymax=succ+se),
    width=.2,                    # Width of the error bars
    position=position_dodge(.9))+
  scale_x_continuous(breaks=c(0,1,3,5))
dev.off()

# head(substr)
# #control mean percentage of germinated seeds and its SD
# control <- c(sum (substr$germ [substr$dish == "1"]),
#              sum (substr$germ [substr$dish == "2"]),
#              sum (substr$germ [substr$dish == "3"]))
# m0=mean(control) #mean germination
# p0=(m0/25)*100                                 #mean germination in %
# p0
# 
# s0=sd(control)
# sd0=(s0/25)*100
# sd0
# 
# # depth 1 mean percentage of germinated seeds and its SD
# depth1 <- c(sum (substr$germ [substr$dish == "4"]),
#             sum (substr$germ [substr$dish == "5"]),
#             sum (substr$germ [substr$dish == "6"]))
# m0=mean(depth1) #mean germination
# p0=(m0/25)*100                                 #mean germination in %
# p0
# 
# s0=sd(depth1)
# sd0=(s0/25)*100
# sd0
# # depth 2 mean percentage of germinated seeds and its SD
# depth2 <- c(sum (substr$germ [substr$dish == "7"]),
#             sum (substr$germ [substr$dish == "8"]),
#             sum (substr$germ [substr$dish == "9"]))
# m0=mean(depth2) #mean germination
# p0=(m0/25)*100                                 #mean germination in %
# p0
# 
# s0=sd(depth2)
# sd0=(s0/25)*100
# sd0
# 
# # depth 3 mean percentage of germinated seeds and its SD
# depth3 <- c(sum (substr$germ [substr$dish == "10"]),
#             sum (substr$germ [substr$dish == "11"]),
#             sum (substr$germ [substr$dish == "12"]))
# m0=mean(depth3) #mean germination
# p0=(m0/25)*100                                 #mean germination in %
# p0
# 
# s0=sd(depth3)
# sd0=(s0/25)*100
# sd0