rm(list=ls())
library(checkpoint)
checkpoint("2016-02-11")
# reshaping the data to be in tidy format
library(dplyr) # manipulation with original data
library (lazyeval) # manipulation with original data
library(tidyr) # manipulation with original data
library(reshape2) # manipulation with original data
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
library (broom)
data=read.csv ("resubmision/prolicovani_ruzne_substraty.csv", header=TRUE, sep=";") 

#treat will be left out and not melted, but rest will be
substr <- melt (data, "treat") 

head (substr) #see how it looks like
names (substr) <- c("treat", "date", "count") #meaningful names of variables
#data exploration
summary (substr)
mean (substr$count [substr$treat == "pi"])
str (substr)
plot (substr$treat, substr$count)

#results will go into this data frame and will be used for further analysis
levels (substr$treat)
treat <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")
germ <- c(sum (substr$count [substr$treat == "a"]),
          sum (substr$count [substr$treat == "b"]),
          sum (substr$count [substr$treat == "c"]),
          sum (substr$count [substr$treat == "d"]),
          sum (substr$count [substr$treat == "e"]),
          sum (substr$count [substr$treat == "f"]),
          sum (substr$count [substr$treat == "g"]),
          sum (substr$count [substr$treat == "h"]),
          sum (substr$count [substr$treat == "i"]))
total <- c(rep (100, 9))
p=germ/total #probability of succesfull germination
negative <- total-germ #how many did not germinate

data_soil<- data.frame (treat, germ, total, negative, p) # dataframe of resulting values
y<-cbind(germ, total - germ) # this vector should be feedid into binomial model

#binomial model and its p-values, "a" is control so every results is compared with control by summary function
m1=glm(y~treat, family=binomial)
anova(m1, test="Ch")
summary (m1)
tidy (m1)

par(mfrow=c(1,1))
rd=residuals(m1)
plot(rd)
qqnorm(residuals(m1, type="deviance"))
abline(a=0,b=1)

#barplot of mean or median germination success across different substrates.#summarySE is function, which is preparing data to be ploted with SE or confidence intervals...
sumary.dev = summarySE (substr, 
  measurevar="count", groupvars="treat")

# tiff (filename="outputs/substrates_germination_barplots.tiff", 
#   width=5000, height=3500, 
#   compression="lzw", res= 800)
p = ggplot (sumary.dev, aes (y=count, x=treat))
p2=p + stat_summary(fun.y=mean, geom="bar", position=position_dodge())+
  xlab("Germination substrate")+
  ylab("Mean nuber of germinated seeds / inspection")+
    geom_errorbar(aes(ymin=count-se, ymax=count+se),
    width=.2,                    # Width of the error bars
    position=position_dodge(.9))
p3=p2+ coord_flip()
p3+scale_x_discrete(labels=c("Control", "Sand+Clay","Sand+Peat", "Sand+Pond mud", "Pond mud", "Sand", "Clay", "Peat", "Topsoil"))
# dev.off()
#substrates_germination_barplots

#control mean percentage of germinated seeds and its SD
control <- c(sum (substr$count [substr$dish == "1"]),
  sum (substr$germ [substr$dish == "2"]),
  sum (substr$germ [substr$dish == "3"]))
m0=mean(control) #mean germination
p0=(m0/25)*100                                 #mean germination in %
p0

s0=sd(control)
sd0=(s0/25)*100
sd0

# depth 1 mean percentage of germinated seeds and its SD
depth1 <- c(sum (substr$germ [substr$dish == "4"]),
  sum (substr$germ [substr$dish == "5"]),
  sum (substr$germ [substr$dish == "6"]))
m0=mean(depth1) #mean germination
p0=(m0/25)*100                                 #mean germination in %
p0

s0=sd(depth1)
sd0=(s0/25)*100
sd0
# depth 2 mean percentage of germinated seeds and its SD
depth2 <- c(sum (substr$germ [substr$dish == "7"]),
  sum (substr$germ [substr$dish == "8"]),
  sum (substr$germ [substr$dish == "9"]))
m0=mean(depth2) #mean germination
p0=(m0/25)*100                                 #mean germination in %
p0

s0=sd(depth2)
sd0=(s0/25)*100
sd0

# depth 3 mean percentage of germinated seeds and its SD
depth3 <- c(sum (substr$germ [substr$dish == "10"]),
  sum (substr$germ [substr$dish == "11"]),
  sum (substr$germ [substr$dish == "12"]))
m0=mean(depth3) #mean germination
p0=(m0/25)*100                                 #mean germination in %
p0

s0=sd(depth3)
sd0=(s0/25)*100
sd0