rm(list=ls())
# reshaping the data to be in tidy format
library(dplyr)
library (lazyeval)
library(tidyr)
library(reshape2)

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


