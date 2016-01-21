rm(list=ls())
# reshaping the data to be in tidy format
library(dplyr)
library (lazyeval)
library(tidyr)
library(reshape2)

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
