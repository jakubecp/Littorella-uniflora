rm(list=ls())
# reshaping the data to be in tidy format
library(dplyr) # manipulation with original data
library (lazyeval) # manipulation with original data
library(tidyr) # manipulation with original data
library(reshape2) # manipulation with original data
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
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

#barplot of mean or median germination success across different substrates.#summarySE is function, which is preparing data to be ploted with SE or confidence intervals...
sumary.dev = summarySE (substr, 
  measurevar="count", groupvars="treat")

tiff (filename="outputs/substrates_germination_barplots.tiff", 
  width=5000, height=3500, 
  compression="lzw", res= 800)
p = ggplot (sumary.dev, aes (y=count, x=treat))
p2=p + stat_summary(fun.y=mean, geom="bar", position=position_dodge())+
  xlab("Germination substrate")+
  ylab("Mean nmber of germinated seeds")+
    geom_errorbar(aes(ymin=count-se, ymax=count+se),
    width=.2,                    # Width of the error bars
    position=position_dodge(.9))
p3=p2+ coord_flip()
p3+scale_x_discrete(labels=c("Control", "Sand+Clay","Sand+Peat", "Sand+Pond mud", "Pond mud", "Sand", "Clay", "Peat", "Topsoil"))
dev.off()
#substrates_germination_barplots