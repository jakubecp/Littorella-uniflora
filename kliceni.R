#1. zpracovat proklicovani - binomical analysis, kontrola + 3 treatmenty - hypoth- je rozdil mezi threatmenty
# zkusit jestli je zdrzeni - otakzka jestli to ma smysl... zdrzeni i u kliceni...
#2. proklicovani - puvodni venkovni experiment - stejne zpracovani, bacha kontrola je z jineho data,
# takze spis jen pro nastaveni nejake hranice klicivosti

#EXAMPLE!!!!!!!!!!!!!!!
#test for feeding choice experiment (8 ladybirds ate the egg out of 24)

binom.test(8, 24, 0.5)
# test for difference between number of parasitized ladybirds which ate the boiled eggs and which did not (p = 0.38).
binom.test(7, 10, 0.38)
#test for difference between number of parasitized ladybirds between wageningen and radwijk
local = rbind (c(60,10), c(22,15))
local
?fisher.test (local)


#LITORELA

local = cbind (c(10,7,0,0), c(65,68,75,75))
fisher.test (local)

