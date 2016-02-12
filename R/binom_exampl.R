rm(list=ls())
# install.packages("blmeco")
library(blmeco)
data (resprouts)
dat <- resprouts
summary (dat)
fail <- dat$pre - dat$post
succ <- dat$pre - fail
dat <-cbind (dat, fail, succ)
mod <- glm(cbind(succ, fail) ~ treatment, data=dat,family=binomial)
plogis(-1.24+1.16)
