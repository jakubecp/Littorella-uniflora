rm(list=ls())
# reshaping the data to be in tidy format
library(ggplot2) # plotting graphs
library(Rmisc) # summarySE function for SE and CI calcul. and ploting
library (MASS) # dose.p function for estimation of limitting depth of soil layer
library (broom)
data=read.csv ("resubmision/proklicovani_sklenik.csv", header=TRUE, sep=";") 
head(data)

#results will go into this data frame and will be used for further analysis
levels (substr$depth)
treat <- c(0,0,0,1,1,1,3,3,3,5,5,5)
germ <- apply(data[,3:18],1, sum)
length(germ)
total <- c(rep (25, 12))
p=germ/total #probability of succesfull germination
negative <- total-germ #how many did not germinate

data_glass<- data.frame (treat, germ, total, negative, p) # dataframe of resulting values
y<-cbind(germ, total - germ) # this vector should be feedid into binomial model

#binomial model and its p-values, "a" is control so every results is compared with control by summary function
m1=glm(y~treat, family=binomial)
m1
anova(m1, test="Ch")
summary (m1)
tidy (m1)
X11()
par(mfrow=c(2,2))
plot(m1)

rd=residuals(m1,type = c("deviance"))
par(mfrow=c(2,1))
rd=residuals(m1)
plot(rd)
qqnorm(residuals(m1, type="deviance"))
abline(a=0,b=1)

#ploting original data (not model)
ggplot(substr, aes(x=depth, y=germ))+
  geom_boxplot ()

#plot of probability of germintion on depth of soil layer
ggplot (data_glass, aes (x=treat, y=p))+
  geom_point(size=3)

#germination decrease with increasing depth of soil layer
summary (m1)
dose.p (m1, cf=c(1:2), p=seq(0.05,0.9,0.05))

#barplot of mean or median germination success across different substrates.
#summarySE is function, which is preparing data to be ploted with SE or 
#confidence intervals...
sumary.dev = summarySE (data_glass, measurevar="germ", groupvars="treat")

tiff (filename="outputs/glass_barplots_depth2.tiff", 
  width=5000, height=3500, 
  compression="lzw", res= 800)
p = ggplot (sumary.dev, aes (y=germ, x=treat))
p + stat_summary(fun.y=mean, geom="bar", position=position_dodge())+
  xlab("Depth of substrate")+
  ylab("Mean number of germinated seeds / inspection")+
  geom_errorbar(aes(ymin=germ-se, ymax=germ+se),
    width=.2,                    # Width of the error bars
    position=position_dodge(.9))+
  scale_x_discrete(labels=c("Control", "1","3", "5"))
dev.off()

#mean germination and SEs or SDs
head(substr)
#control mean percentage of germinated seeds and its SD
control <- c(sum (substr$germ [substr$dish == "1"]),
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