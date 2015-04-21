#1. zpracovat proklicovani - binomical analysis, kontrola + 3 treatmenty - hypoth- je rozdil mezi threatmenty
# zkusit jestli je zdrzeni - otakzka jestli to ma smysl... zdrzeni i u kliceni...
#2. proklicovani - puvodni venkovni experiment - stejne zpracovani, bacha kontrola je z jineho data,
# takze spis jen pro nastaveni nejake hranice klicivosti

#EXAMPLE!!!!!!!!!!!!!!!
#test for feeding choice experiment (8 ladybirds ate the egg out of 24)


# test for difference between number of parasitized ladybirds which ate the boiled eggs and which did not (p = 0.38).
binom.test(7, 10, 0.38)
#test for difference between number of parasitized ladybirds between wageningen and radwijk
local = rbind (c(60,10), c(22,15))
local
?fisher.test (local)


#LITORELA

#test if true probability of germination of seeds covered by 1cm of soil is the same as for the control
binom.test(7, 68, 0.1333333333) #LABORKA
binom.test (26,49, 0.53333333333333) #VENKU
binom.test (33,117,0.33333333333333) #LABORKA + VENKU


#test if true probability of germination of seeds covered by 3 and 5 cm of soil is the same as for the control
binom.test(0, 75, 0.1333333333)
binom.test(0, 75, 0.53333333333333)
binom.test(0, 150, 0.33333333333333)

#I have doubts about these test because i am doing multiple comparisons
#, which is not statisticaly correct. I should try something else...



#some bulshit - delete it
local = cbind (c(10,7,0,0), c(65,68,75,75))
fisher.test (local)
#Binomial model
list()
rm(list=ls())
setwd ("C:/Users/pavel/Downloads/Dropbox/Litorela uniflora/") #notas
setwd ("C:/Users/jakubecp/Dropbox/Litorela uniflora") #skola
setwd ("/home/pavel/Dropbox/Litorela uniflora")

data=read.csv ("data.csv", header=TRUE, sep=";") #data with only final values are used
names(data)
str(data)
data_lab =data[data$exp == "lab",]
data_ext =data[data$exp == "ext",]

#probability of success is counted and stored as "p"
p=data$germ/data$n
p_lab=data_lab$germ/data_lab$n
p_ext=data_ext$germ/data_ext$n
#check for interactions
interaction.plot(data$treatment, data$exp, p, 
                 xlab="Soil depth", ylab="Mean germination", main="Mean germination of L. uniflora") #no interaction was found

y=cbind(data$germ, data$n - data$germ)
y_lab=cbind(data_lab$germ, data_lab$n - data_lab$germ)
y_ext=cbind(data_ext$germ, data_ext$n - data_ext$germ)
#model with an interaction
m1=glm(y~data$exp*data$treatment, family=binomial)
anova(m1, test="F")
summary (m1)
m2=glm(y~data$exp+data$treatment, family=binomial)
m3=glm(y~data$treatment, family=binomial)
AIC (m1,m2,m3)
summary (m1)
summary (m2)
summary (m3)



m4=glm(y_lab~data_lab$treatment, family=binomial)
anova(m2, test="Chi")
m3=glm(y_ext~data_ext$treatment, family=binomial)
anova(m3, test="Chi")
summary (m2)
summary (m3)
#model without an interaction
m2=update(m1, ~. -data$exp:data$treatment)
anova(m2, test="Chisq")
summary(m2)
#check if one of the models is significantly better
AIC(m1,m2)
#plot the selected model to check the fit
plot (m2, which=1)
#quasibinomial distribution of errors because residual deviance is twice of residual degrees of freedom
m3=update(m2, family=quasibinomial)
summary(m3)
anova(m3, test="F")
# estimate of relative germination
#venku
1/(1+exp(-0.3018))
#v laborce
1/(1+exp(-0.3018+1.8989))
x=seq(0,5,by=0.1)
plot (data$treatment, p)

lines (x, predict (m2, data.frame(data$treatment=x)type="response"))

#estimate of relative germination
#
plot(data$temp[data$Herkomstcode==1], data$dvrTotalcombi[data$Herkomstcode==1], type='p', pch=16, col=c("red"))
points(data$temp[data$Herkomstcode==0], data$dvrTotalcombi[data$Herkomstcode==0], type='p', pch=16, col=c("blue"))
abline(a=nll.allG$par[1],b=nll.allG$par[3], col='blue', lty=2)
abline(a=nll.allG$par[2],b=nll.allG$par[3], col='red', lty=2)

#FINAL MODEL
rm(list=ls())
setwd ("C:/Users/pavel/Downloads/Dropbox/Litorela uniflora/") #notas
setwd ("C:/Users/jakubecp/Dropbox/Litorela uniflora") #skola
setwd ("/home/pavel/Dropbox/Litorela uniflora")
data=read.csv ("data.csv", header=TRUE, sep=";")
head(data)
attach(data)
p=germ/n
y=cbind(germ, n - germ)
m1=glm(y~exp+treatment, family=binomial)
pr=resid(m1,type="pearson")
plot (treatment, pr)
x=seq(0,6,0.1)
plot (treatment,p, xlab="Soil depth", ylab="Germination", main = "")
lines (x, predict (m1, list(treatment=x, exp=factor(rep("lab", length(x)), levels=levels(exp))), type="response"))
lines (x, predict (m1, list(treatment=x, exp=factor(rep("ext", length(x)), levels=levels(exp))), type="response"))

library (MASS)
m2 = glm (y~exp+treatment-1, family=binomial)
summary (m2)
dose.p (m2, cf=c(1,3), p=0.01)
dose.p (m2, cf=c(2,3), p=0.01)
