rm(list=ls())
# reshaping the data to be in tidy format
library(dplyr)
library (lazyeval)
library(tidyr)
library(reshape2)

data=read.csv ("resubmision/kliceni_klima.csv", header=TRUE, sep=";") 
