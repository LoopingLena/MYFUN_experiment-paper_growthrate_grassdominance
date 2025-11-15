### allomotries
###
### IDEA: The idea is to find a relationship between the biomass and traits
###       (height, diameter) of the seedlings to estimate the biomass
###       of the experimental plants in March. 
###       This biomass can then be used to calculate together with the biomass
###       of the end of the experiment the growth rate, as well as simply correct
###       the final biomass by the starting biomass. This is needed, as some 
###       species started with larger seedlings than other species.

#
#load raw data, but now prepared data is available as RData files (use this) 
seedlings <- read.csv("D:/University/IMEM/TFG/R y analsis/R y analsis/LIZA_seedling_database_without_outliers.csv", header=T, dec=",", sep=";")
seedlings$species<- as.factor(seedlings$species)
march<- read.csv("D:/University/IMEM/TFG/R y analsis/R y analsis/database_march_LIZA.csv", header=T, dec=",", sep=";")
march$species<- as.factor(march$species)

#prepared data files (TO USE), continue with biomass models
load("./seedlings_edited.RData")
load("./march_edited.RData")

#already done
#preparación tabla seedlings y march con cono, cobertura y cilindro + logroots y logleaves

seedlings$cono <- pi*(seedlings$dia1/2)*(seedlings$dia2/2)*seedlings$height*1/3
seedlings$cilindro <- pi*(seedlings$dia1/2)*(seedlings$dia2/2)*seedlings$height
seedlings$cobertura <- pi*(seedlings$dia1/2)*(seedlings$dia2/2)
seedlings$logcono <- log(pi*(seedlings$dia1/2)*(seedlings$dia2/2)*seedlings$height*1/3)
seedlings$logcilindro <- log(pi*(seedlings$dia1/2)*(seedlings$dia2/2)*seedlings$height)
seedlings$logcobertura <- log(pi*(seedlings$dia1/2)*(seedlings$dia2/2))
seedlings$logleaves <- log(seedlings$biomass_leaves)
seedlings$logroots <- log(seedlings$biomass_roots)
seedlings$logheight <- log(seedlings$height)

march$cono <- pi*(march$dia1/2)*(march$dia2/2)*march$height*1/3
march$cilindro <- pi*(march$dia1/2)*(march$dia2/2)*march$height
march$cobertura <- pi*(march$dia1/2)*(march$dia2/2)
march$logcono <- log(pi*(march$dia1/2)*(march$dia2/2)*march$height*1/3)
march$logcilindro <- log(pi*(march$dia1/2)*(march$dia2/2)*march$height)
march$logcobertura <- log(pi*(march$dia1/2)*(march$dia2/2))
march$logheight <- log(march$height)

save(seedlings, file ="seedlings_edited.RData")
save(march, file ="march_edited.RData")

#### Escripto (trocitos) para sacar los valores de biomassa, una vez que hemos sacado las relaciones buenas.
#species 1: Achillea millefolium
######
subAchillea <- seedlings[seedlings$species == "Achillea millefolium",]
m1leaves<- lm(biomass_leaves ~ 0 + logcilindro, subAchillea)
m1roots<- lm(logroots ~ 0 + logleaves, subAchillea)
subAchilleaMarch<- march[march$species == "Achillea millefolium",]

biomass_l<-  m1leaves$coefficients*subAchilleaMarch$logcilindro


biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subAchilleaMarch$biomass_leaves<- biomass_l
subAchilleaMarch$biomass_roots<- biomass_r

save(subAchilleaMarch, file ="subAchilleaMarch.RData")

#comprobar

subAchillea[1:5, c(7,14,6,11,12)]
subAchilleaMarch[1:5, c(14,12,15,16)]

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 2: Agrostis capillaris
######

subAgrostis <- seedlings[seedlings$species == "Agrostis capillaris",]
m1leaves <- lm(biomass_leaves ~ 0 + logcilindro, subAgrostis)
m1roots<- lm(biomass_roots ~ 0 + cobertura, subAgrostis) 

subAgrostisMarch<- march[march$species == "Agrostis capillaris",]
biomass_l<- m1leaves$coefficient*subAgrostisMarch$logcilindro
biomass_r <- m1roots$coefficients*subAgrostisMarch$cobertura

subAgrostisMarch$biomass_leaves<- biomass_l
subAgrostisMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subAgrostis[1:5, c(6,7,14,10)]
subAgrostisMarch[1:5, c(16,15,12,10)]

save(subAgrostisMarch, file ="subAgrostisMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 3: Bellis perennis
######

subBellis <- seedlings[seedlings$species == "Bellis perennis",]
m1leaves <- lm(biomass_leaves ~ 0 + logcilindro, subBellis)
m1roots<- lm(logroots ~ 0 + logleaves, subBellis) 

subBellisMarch<- march[march$species == "Bellis perennis",]
biomass_l<-m1leaves$coefficients*subBellisMarch$logcilindro
biomass_r<- exp(m1roots$coefficients*log(biomass_l))
#Para quitar el valor el valor pequeño que sale negativo por biomass_leaves (fila 15) --> cambiamoslo en la proporción con el 
#valor de cilindro de otro más pequeño: entonces lo sustituimos y añadimos a biomass_l: > biomass_l[15] -0.000280314
#biomass_l[15] <-0.000620635 (valor sacado de la proporción). Volver a run biomass_r.
biomass_l
biomass_l[15]
biomass_l[15] <-0.000620635
biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subBellisMarch$biomass_leaves<- biomass_l
subBellisMarch$biomass_roots<- biomass_r
 
#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subBellis[1:5, c(6,7,12,14)]
subBellisMarch[1:5, c(16,15,12)]

save(subBellisMarch, file ="subBellisMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)


######
#species 4: Brachypodium retusum
######

subBrachypodium <- seedlings[seedlings$species == "Brachypodium retusum",]
m1leaves <- lm(biomass_leaves ~ 0 + cobertura, subBrachypodium)
m1roots<- lm(biomass_roots ~ 0 + cono, subBrachypodium) 

subBrachypodiumMarch<- march[march$species == "Brachypodium retusum",]
biomass_l<-m1leaves$coefficients*subBrachypodiumMarch$cobertura
biomass_r<- m1roots$coefficients*subBrachypodiumMarch$cono

subBrachypodiumMarch$biomass_leaves<- biomass_l
subBrachypodiumMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subBrachypodium[1:5, c(8,10,7,6)]
subBrachypodiumMarch[1:5, c(8,10,15,16)]

save(subBrachypodiumMarch, file ="subBrachypodiumMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 5: Dactylis glomerata
######

subDactylis <- seedlings[seedlings$species == "Dactylis glomerata",]
m1leaves <- lm(biomass_leaves ~ 0 + height, subDactylis)
m1roots<- lm(logroots ~ 0 + logleaves, subDactylis) 

subDactylisMarch<- march[march$species == "Dactylis glomerata",]
biomass_l<-m1leaves$coefficients*subDactylisMarch$height
biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subDactylisMarch$biomass_leaves<- biomass_l
subDactylisMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subDactylis[1:5, c(5,14,7,6)]
subDactylisMarch[1:5, c(5,15,16)]

save(subDactylisMarch, file ="subDactylisMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 6: Deschampsia cespitosa
######

subDeschampsia <- seedlings[seedlings$species == "Deschampsia cespitosa",]
m1leaves <- lm(biomass_leaves ~ 0 + logcono, subDeschampsia)
m1roots<- lm(logroots ~ 0 + logleaves, subDeschampsia) 

subDeschampsiaMarch<- march[march$species == "Deschampsia cespitosa",]
biomass_l<-m1leaves$coefficients*subDeschampsiaMarch$logcono
biomass_r<- exp(m1roots$coefficients*log(biomass_l))
#Para quitar el valor el valor pequeño que sale negativo por biomass_leaves (fila 15) --> cambiamoslo en la proporción con el 
#valor de cilindro de otro más pequeño: entonces lo sustituimos y añadimos a biomass_l: > biomass_l[15] -0.000280314
#biomass_l[15] <-0.000620635 (valor sacado de la proporción). Volver a run biomass_r.
biomass_l
biomass_l[27]
subDeschampsiaMarch

biomass_l[27] <-0.00675803
biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subDeschampsiaMarch$biomass_leaves<- biomass_l
subDeschampsiaMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subDeschampsia[1:5, c(11,7,6)]
subDeschampsiaMarch[1:5, c(11,15,16)]

save(subDeschampsiaMarch, file ="subDeschampsiaMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 7:  Festuca rubra
######

subFestuca <- seedlings[seedlings$species == "Festuca rubra",]
m1leaves <- lm(biomass_leaves ~ 0 + cobertura, subFestuca)
m1roots<- lm(logroots ~ 0 + logleaves, subFestuca) 

subFestucaMarch<- march[march$species == "Festuca rubra",]
biomass_l<-m1leaves$coefficients*subFestucaMarch$cobertura
biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subFestucaMarch$biomass_leaves<- biomass_l
subFestucaMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subFestuca[1:5, c(10,7,6)]
subFestucaMarch[1:5, c(10,15,16)]

save(subFestucaMarch, file ="subFestucaMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 8: Lolium perenne
######

subLolium <- seedlings[seedlings$species == "Lolium perenne",]
m1leaves <- lm(biomass_leaves ~ 0 + logcobertura, subLolium)
m1roots<- lm(logroots ~ 0 + logleaves, subLolium) 

subLoliumMarch<- march[march$species == "Lolium perenne",]
biomass_l<-m1leaves$coefficients*subLoliumMarch$logcobertura
biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subLoliumMarch$biomass_leaves<- biomass_l
subLoliumMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subLolium[1:5, c(13,7,6)]
subLoliumMarch[1:5, c(13,15,16)]

save(subLoliumMarch, file ="subLoliumMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 9: Phleum pratensis
######

subPhleum <- seedlings[seedlings$species == "Phleum pratensis",]
m1leaves <- lm(biomass_leaves ~ 0 + logcilindro, subPhleum)
m1roots<- lm(logroots ~ 0 + logleaves, subPhleum) 

subPhleumMarch<- march[march$species == "Phleum pratensis",]
biomass_l<-m1leaves$coefficients*subPhleumMarch$logcilindro
biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subPhleumMarch$biomass_leaves<- biomass_l
subPhleumMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subPhleum[1:5, c(12,7,6)]
subPhleumMarch[1:5, c(12,15,16)]

save(subPhleumMarch, file ="subPhleumMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 10: Planatago albicans
######

subPlantago <- seedlings[seedlings$species == "Planatago albicans",]
m1leaves <- lm(biomass_leaves ~ 0 + cono, subPlantago)
m1roots<- lm(logroots ~ 0 + logleaves, subPlantago) 

subPlantagoMarch<- march[march$species == "Plantago albicans",]
biomass_l<-m1leaves$coefficients*subPlantagoMarch$cono 
biomass_r<- exp(m1roots$coefficients*log(biomass_l))

subPlantagoMarch$biomass_leaves<- biomass_l
subPlantagoMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subPlantago[1:5, c(8,7,6)]
subPlantagoMarch[1:5, c(8,15,16)]

save(subPlantagoMarch, file ="subPlantagoMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 11: Poa pratensis
######

subPoa <- seedlings[seedlings$species == "Poa pratensis",]
m1leaves <- lm(biomass_leaves ~ 0 + height, subPoa)
m1roots<- lm(biomass_roots ~ 0 + biomass_leaves, subPoa) 

subPoaMarch<- march[march$species == "Poa pratensis",]
biomass_l<-m1leaves$coefficients*subPoaMarch$height
biomass_r<- m1roots$coefficients*biomass_l

subPoaMarch$biomass_leaves<- biomass_l
subPoaMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subPoa[1:5, c(5,7,6)]
subPoaMarch[1:5, c(5,15,16)]

save(subPoaMarch, file ="subPoaMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 12:  Prunella vulgaris
######

subPrunella <- seedlings[seedlings$species == "Prunella vulgaris",]
m1leaves <- lm(biomass_leaves ~ 0 + logcobertura, subPrunella)
m1roots<- lm(biomass_roots ~ 0 + cobertura, subPrunella) 

subPrunellaMarch<- march[march$species == "Prunella vulgaris",]
biomass_l<-m1leaves$coefficients*subPrunellaMarch$logcobertura
biomass_r<-m1roots$coefficients*subPrunellaMarch$cobertura

subPrunellaMarch$biomass_leaves<- biomass_l
subPrunellaMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subPrunella[1:5, c(10,13,7,6)]
subPrunellaMarch[1:5, c(10,13,15,16)]

save(subPrunellaMarch, file ="subPrunellaMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 13: Rumex acetosa
######

subRumex <- seedlings[seedlings$species == "Rumex acetosa",]
m1leaves <- lm(biomass_leaves ~ 0 + height, subRumex)
m1roots<- lm(biomass_roots ~ 0 + height, subRumex) 

subRumexMarch<- march[march$species == "Rumex acetosa",]
biomass_l<-m1leaves$coefficients*subRumexMarch$height
biomass_r<- m1roots$coefficients*subRumexMarch$height

subRumexMarch$biomass_leaves<- biomass_l
subRumexMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subRumex[1:5, c(5,7,6)]
subRumexMarch[1:5, c(5,15,16)]

save(subRumexMarch, file ="subRumexMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 14: Sanguisorba minor
######

subSanguisorba <- seedlings[seedlings$species == "Sanguisorba minor",]
m1leaves <- lm(biomass_leaves ~ 0 + logcobertura, subSanguisorba)
m1roots<- lm(biomass_roots ~ 0 + biomass_leaves, subSanguisorba) 

subSanguisorbaMarch<- march[march$species == "Sanguisorba minor",]
biomass_l<-m1leaves$coefficients*subSanguisorbaMarch$logcobertura
biomass_r<- m1roots$coefficients*biomass_l

subSanguisorbaMarch$biomass_leaves<- biomass_l
subSanguisorbaMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subSanguisorba[1:5, c(13,7,6)]
subSanguisorbaMarch[1:5, c(13,15,16)]

save(subSanguisorbaMarch, file ="subSanguisorbaMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 15: Silene vulgaris
######

subSilene <- seedlings[seedlings$species == "Silene vulgaris",]
m1leaves <- lm(biomass_leaves ~ 0 + cobertura, subSilene)
m1roots<- lm(biomass_roots ~ 0 + logcilindro, subSilene) 

subSileneMarch<- march[march$species == "Silene vulgaris",]
biomass_l<-m1leaves$coefficients*subSileneMarch$cobertura
biomass_r<- m1roots$coefficients*subSileneMarch$logcilindro

subSileneMarch$biomass_leaves<- biomass_l
subSileneMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subSilene[1:5, c(10,12, 7,6)]
subSileneMarch[1:5, c(10,12,15,16)]

save(subSileneMarch, file ="subSileneMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)

######
#species 16: Taraxacum officinalis
######

subTaraxacum <- seedlings[seedlings$species == "Taraxacum officinalis",]
m1leaves <- lm(biomass_leaves ~ 0 + cobertura, subTaraxacum)
m1roots<- lm(biomass_roots ~ 0 + biomass_leaves, subTaraxacum) 

subTaraxacumMarch<- march[march$species == "Taraxacum officinale",]
biomass_l<-m1leaves$coefficients*subTaraxacumMarch$cobertura
biomass_r<- m1roots$coefficients*biomass_l

subTaraxacumMarch$biomass_leaves<- biomass_l
subTaraxacumMarch$biomass_roots<- biomass_r

#para controlar si hace mas o menos sentido --> log tira la relacion al reves, usamos la relacion sin log
subTaraxacum[1:5, c(10,7,6)]
subTaraxacumMarch[1:5, c(10,15,16)]

save(subTaraxacumMarch, file ="subTaraxacumMarch.RData")

rm(biomass_l)
rm(biomass_r)
rm(m1leaves)
rm(m1roots)


## CODE TO SAVE ALL THESE TO THE EXCEL FILE
install.packages('data.table')
# Load the necessary packages
library(data.table)

# Create an empty data frame to store the combined data
combined_data <- data.frame()

# List the .RData files that you want to combine
biomass_marzo <- list(subAchilleaMarch, subAgrostisMarch, subBellisMarch,subBrachypodiumMarch,subDactylisMarch,subDeschampsiaMarch, subFestucaMarch, subLoliumMarch, subPhleumMarch,subPlantagoMarch,
                      subPoaMarch,subPrunellaMarch, subRumexMarch, subSanguisorbaMarch, subSileneMarch,subTaraxacumMarch)

save(biomass_marzo, file ="alldatamarch.RData")


marzo_completo <- rbind(subAchilleaMarch, subAgrostisMarch, subBellisMarch,subBrachypodiumMarch,subDactylisMarch,subDeschampsiaMarch, subFestucaMarch, subLoliumMarch, subPhleumMarch,subPlantagoMarch,
                        subPoaMarch,subPrunellaMarch, subRumexMarch, subSanguisorbaMarch, subSileneMarch,subTaraxacumMarch)
write.table(marzo_completo, file="marzo_completo.txt")

marzo_poa <- rbind(subPoaMarch)
write.table(marzo_poa, file="marzo_poa.txt")

marzo_sanguisorba <- rbind(subSanguisorbaMarch)
write.table(marzo_sanguisorba, file="marzo_sanguisorba.txt")

marzo_plantago <- rbind(subPlantagoMarch)
write.table(marzo_plantago, file="marzo_plantago.txt")

marzo_agrostis <- rbind(subAgrostisMarch)
write.table(marzo_agrostis, file="marzo_agrostis.txt")

marzo_brach <- rbind(subBrachypodiumMarch)
write.table(marzo_brach, file="marzo_brach.txt")
