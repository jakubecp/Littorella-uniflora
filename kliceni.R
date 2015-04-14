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
setwd ("C:/Users/pavel/Downloads/Dropbox/Litorela uniflora/")
data=read.csv ("data.csv", header=TRUE, sep=";") #data with only final values are used
names(data)
str(data)
#probability of success is counted and stored as "p"
p=data$germ/data$n
p
#check for interactions
interaction.plot(data$exp,data$treatment, p) #no interaction was found

y=cbind(data$germ, data$n - data$germ)
#model with an interaction
m1=glm(y~data$exp*data$treatment, family=binomial)
anova(m1, test="Chi")
#model without an interaction
m2=update(m1, ~. -data$exp:data$treatment)
anova(m2,test="Chi")
summary(m2)
#check if one of the models is significantly better
AIC(m1,m2)
#plot the selected model to check the fit
plot (m2, which=2)

