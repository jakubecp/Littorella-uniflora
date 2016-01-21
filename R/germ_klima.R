rm(list=ls())
# reshaping the data to be in tidy format
library(dplyr)
library (lazyeval)
library(tidyr)
library(reshape2)

data=read.csv ("resubmision/kliceni_klima.csv", header=TRUE, sep=";") 

#treat will be left out and not melted, but rest will be
substr <- melt (data, c("loc", "dish")) 

head (substr) #see how it looks like
names (substr) <- c("loc", "dish", "date", "germ") #meaningful names of variables
#data exploration
summary (substr)
mean (substr$germ [substr$loc == "b"])
str (substr)
plot (substr$loc, substr$germ)

#results will go into this data frame and will be used for further analysis
levels (substr$loc)
treat <- c("ka", "kr", "la", "mr", "no", "os", "ry", "st")
germ <- c(sum (substr$germ [substr$loc == "ka"]),
          sum (substr$germ [substr$loc == "kr"]),
          sum (substr$germ [substr$loc == "la"]),
          sum (substr$germ [substr$loc == "mr"]),
          sum (substr$germ [substr$loc == "no"]),
          sum (substr$germ [substr$loc == "os"]),
          sum (substr$germ [substr$loc == "ry"]),
          sum (substr$germ [substr$loc == "st"]))
total <- c(rep (75, 8))
p=germ/total #probability of succesfull germination
negative <- total-germ #how many did not germinate

data_klima_germ<- data.frame (treat, germ, total, negative, p) # dataframe of resulting values
y<-cbind(germ, total - germ) # this vector should be feedid into binomial model

#binomial model and its p-values, "a" is control so every results is compared with control by summary function
m1=glm(y~treat, family=binomial)
anova(m1, test="Ch")
summary (m1)
