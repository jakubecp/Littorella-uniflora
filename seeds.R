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

install.packages("lazyeval")

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("hadley/lazyeval")
devtools::install_github("hadley/dplyr")


# have to restructuralized the data ...
library(dplyr)
library (lazyeval)
