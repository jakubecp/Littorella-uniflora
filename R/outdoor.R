data=read.csv ("data/data2.csv", header=TRUE, sep=";") 
names(data)
str(data)
data_lab =data[data$exp == "lab",]
data_ext =data[data$exp == "ext",]

#probability of success is counted and stored as "p"
p=data$germ/data$n
p_lab=data_lab$germ/data_lab$n
p_ext=data_ext$germ/data_ext$n
