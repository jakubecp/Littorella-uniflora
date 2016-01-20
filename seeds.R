rm(list=ls())
data=read.csv ("data2.csv", header=TRUE, sep=";") 
names(data)
str(data)
data_lab =data[data$exp == "lab",]
data_ext =data[data$exp == "ext",]

#probability of success is counted and stored as "p"
p=data$germ/data$n
p_lab=data_lab$germ/data_lab$n
p_ext=data_ext$germ/data_ext$n

install.packages("reshape2")

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("hadley/lazyeval")
devtools::install_github("hadley/dplyr")
devtools::install_github("hadley/tidyr")

# reshaping the data to be in tidy format
library(dplyr)
library (lazyeval)
library(tidyr)
library(reshape2)
data=read.csv ("resubmision/prolicovani_ruzne_substraty.csv", header=TRUE, sep=";") 
substr <- melt (data, "treat") #treat will be left out and not melted, but rest will be
head (substr) #see how it looks like
names (substr) <- c("treat", "date", "count") #meaningful names of variables
#data exploration
summary (substr)
mean (substr$count [substr$treat == "pi"])
str (substr)
plot (substr$treat, substr$count)
