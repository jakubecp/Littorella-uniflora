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
binom.test(7, 68, 0.1333333333)

#test if true probability of germination of seeds covered by 3 and 5 cm of soil is the same as for the control
binom.test(0, 75, 0.1333333333)

#I have doubts about these test because i am doing multiple comparisons
#, which is not statisticaly correct. I should try something else...



#some bulshit - delete it
local = cbind (c(10,7,0,0), c(65,68,75,75))
fisher.test (local)

