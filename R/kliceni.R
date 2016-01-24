#1. zpracovat proklicovani - binomical analysis, kontrola + 3 treatmenty - hypoth- je rozdil mezi threatmenty
# zkusit jestli je zdrzeni - otakzka jestli to ma smysl... zdrzeni i u kliceni...
#2. proklicovani - puvodni venkovni experiment - stejne zpracovani, bacha kontrola je z jineho data,
# takze spis jen pro nastaveni nejake hranice klicivosti

#LITORELA

#test if true probability of germination of seeds covered by 1cm of soil is the same as for the control
binom.test(7, 68, 0.1333333333) #LABORKA
binom.test (26,49, 0.53333333333333) #VENKU
binom.test (33,117,0.33333333333333) #LABORKA + VENKU


#test if true probability of germination of seeds covered by 3 and 5 cm of soil is the same as for the control
binom.test(0, 75, 0.1333333333)
binom.test(0, 75, 0.53333333333333)
binom.test(0, 150, 0.33333333333333)
install.packages ("ggplot2")
library (ggplot2)
#I have doubts about these test because i am doing multiple comparisons
#, which is not statisticaly correct. I should try something else...



#some bulshit - delete it
local = cbind (c(10,7,0,0), c(65,68,75,75))
fisher.test (local)
#Binomial model

rm(list=ls())
# setwd ("C:/Users/pavel/Downloads/Dropbox/Litorela uniflora/") #notas
# setwd ("C:/Users/jakubecp/Dropbox/Litorela uniflora") #skola
# setwd ("/home/pavel/Dropbox/Litorela uniflora")

data=read.csv ("data/data2.csv", header=TRUE, sep=";") #data with only final values are used
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
anova(m1, test="Ch")
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
library(ggplot2)
# setwd ("C:/Users/pavel/Downloads/Dropbox/Litorela uniflora/") #notas
# setwd ("C:/Users/jakubecp/Dropbox/Litorela uniflora") #skola
# setwd ("/home/pavel/Dropbox/Litorela uniflora") #comp
data=read.csv ("data/data2.csv", header=TRUE, sep=";")
head(data)
attach(data)
data_lab =data[data$exp == "lab",]
data_ext =data[data$exp == "ext",]
p=germ/n
y=cbind(germ, n - germ)
m1=glm(y~exp+treatment, family=binomial)
anova(m1, test="Chi")
summary (m1)
pr=resid(m1,type="pearson")
#qqplot2 pokus o plot s SE
ggplot(data, aes(x=treatment, y=p, colour=factor (exp))) + 
  geom_point(shape=1) + 
  stat_smooth(method="glm", se=T, size=1)
  
  


grid <- with(data, expand.grid(
  exp = levels(factor(exp)),
  treatment = seq(min(treatment), max(treatment), length = 12 )))
grid$y <- stats::predict(m1, newdata=grid)
qplot(treatment, y, data=data, colour=factor (exp)) + geom_line(data=grid)
x=seq(0,6,0.1)
X11()
plot (data_lab$treatment,p[exp=="lab"], xlab="Soil depth (cm)", ylab="Germination probability", ylim=c(0,0.65), main = "", pch=16, col="blue")
points (data_ext$treatment,p[exp=="ext"], pch=16, col="red")
lines (x, predict (m1, list(treatment=x, exp=factor(rep("ext", length(x)), levels=levels(exp))), type="response"), lty=2, col="red")
lines (x, predict (m1, list(treatment=x, exp=factor(rep("lab", length(x)), levels=levels(exp))), type="response"), col="blue")
legend (3.6,0.65, c("Laboratory", "Glasshouse"), lty=1:2, col=c("blue", "red"))
dev.print(tiff, "image.tiff", compression = "lzw", res=600, height=12, width=12, units="in")
tiff ("image.tiff", compression = "lzw", res=600, height=8, width=8, units="in")
##plot()
dev.off()
#germination decrease with increasing depth of soil layer
library (MASS)
m2 = glm (y~exp+treatment-1, family=binomial)
summary (m2)
dose.p (m2, cf=c(1,3), p=0.05)
dose.p (m2, cf=c(2,3), p=0.05)


# estimate of relative germination
#venku
1/(1+exp(-0.2024))
#v laborce
1/(1+exp(-0.2024-0.3148))

m0=mean (data_lab$germ [data_lab$treatment ==0])
p0=(m0/25)*100
s0= sd (data_lab$germ [data_lab$treatment ==0])
sd0= (s0/25)*100
m0=mean (data_ext$germ [data_ext$treatment ==0])
p0=(m0/25)*100
s0=sd (data_ext$germ [data_ext$treatment ==0])
sd0= (s0/25)*100
