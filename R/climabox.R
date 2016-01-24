rm(list=ls())
# reshaping the data to be in tidy format
library(dplyr) # manipulation with original data
library (lazyeval) # manipulation with original data
library(tidyr) # manipulation with original data
library(reshape2) # manipulation with original data
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
library (MASS) # dose.p function for estimation of limitting depth of soil layer

data=read.csv ("resubmision/proklicovani_klima.csv", header=TRUE, sep=";") 

#treat will be left out and not melted, but rest will be
substr <- melt (data, c("depth", "dish")) 

head (substr) #see how it looks like
names (substr) <- c("depth", "dish", "date", "germ") #meaningful names of variables
#data exploration
summary (substr)
mean (substr$germ [substr$depth == "b"])
str (substr)
plot (substr$depth, substr$germ)

#results will go into this data frame and will be used for further analysis
levels (substr$depth)
treat <- c("a", "b", "c", "d")
germ <- c(sum (substr$germ [substr$depth == "a"]),
          sum (substr$germ [substr$depth == "b"]),
          sum (substr$germ [substr$depth == "c"]),
          sum (substr$germ [substr$depth == "d"]))
total <- c(rep (75, 4))
p=germ/total #probability of succesfull germination
negative <- total-germ #how many did not germinate

data_klima<- data.frame (treat, germ, total, negative, p) # dataframe of resulting values
y<-cbind(germ, total - germ) # this vector should be feedid into binomial model

#binomial model and its p-values, "a" is control so every results is compared with control by summary function
m1=glm(y~treat, family=binomial)
anova(m1, test="Ch")
summary (m1)

#ploting original data (not model)
ggplot(substr, aes(x=depth, y=germ))+
  geom_boxplot ()

#plot of probability of germintion on depth of soil layer
ggplot (data_klima, aes (x=treat, y=p))+
  geom_point(size=3)

#germination decrease with increasing depth of soil layer
summary (m1)
dose.p (m1, cf=c(1:2), p=seq(0.01,0.9,0.05))

#barplot of mean or median germination success across different substrates.#summarySE is function, which is preparing data to be ploted with SE or confidence intervals...
sumary.dev = summarySE (substr, measurevar="germ", groupvars="depth")

tiff (filename="outputs/climabox_barplots_depth.tiff", 
  width=5000, height=3500, 
  compression="lzw", res= 800)
p = ggplot (sumary.dev, aes (y=germ, x=depth))
p + stat_summary(fun.y=mean, geom="bar", position=position_dodge())+
  xlab("Depth of substrate")+
  ylab("Mean number of germinated seeds")+
  geom_errorbar(aes(ymin=germ-se, ymax=germ+se),
    width=.2,                    # Width of the error bars
    position=position_dodge(.9))
dev.off()