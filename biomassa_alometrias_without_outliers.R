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
#load data
seedlings<- read.csv("LIZA_seedling_database_without_outliers.csv", header=T, dec=",", sep=";")

seedlings$species<- as.factor(seedlings$species)
#prepared data files (TO USE), continue with biomass models
load("./seedlings_edited.RData")
load("./march_edited.RData")

# if you want only access the correlation matrices, load the file directly
#load("./corr_list.RData")

### trying out some trait relations - that could have a relation with biomass
#volume cono  - V = 1/3 · π · r2 · h
#volume cylindro  - V = π · r2 · h
#cobertura  - A = π · r2 

seedlings$cono<- 1/3 * pi * seedlings$height* ((seedlings$dia1+seedlings$dia2)/4)^2
seedlings$cobertura<- ((seedlings$dia1+seedlings$dia2)/4)^2 * pi
seedlings$cylindro<- pi * seedlings$height * ((seedlings$dia1+seedlings$dia2)/4)^2

#making plots - one per species over all 7-8 trait variables
######

library(reshape2)
library(ggplot2)
######
#plots - leaves
plot_list_leaves<- list()
plot_list_leaves_height<- list()
plot_list_leaves_logheight<- list()
plot_list_leaves_cobertura<- list()
plot_list_leaves_logcobertura<- list()
plot_list_leaves_cilindro<- list()
plot_list_leaves_logcilindro<- list()


for(i in 1:16){
  sp<- levels(seedlings$species)[i]
  sub<- seedlings[seedlings$species==sp,]
  sub1<- (sub[,c(5:16)])
  melt_sub<- melt(sub1)
  sub2<- data.frame(rep((sub$biomass_leaves), times=ncol(sub1)),
                    melt_sub)
  
  colnames(sub2)<- c("biomass_leaves", "variable", "value")
  
  val<- paste(colnames(sub2)[3],sp, sep = " - ")
  
  
  p<- ggplot(sub2, aes(x=value, y=biomass_leaves)) +
    facet_wrap(. ~ variable, ncol = 2, scales="free_x")+
    geom_point() +
    scale_x_continuous(name=val)+

    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  

  val2<- paste("height (cm)",sp, sep = " - ")
  #log.model2 <-lm(log(biomass_leaves) ~ height, sub)
  #log.model.df2 <- data.frame(x = sub$height,
                           #  y = exp(fitted(log.model2)))
  
  p2<- ggplot(sub, aes(x=height, y=biomass_leaves)) +
    geom_point() +
    scale_x_continuous(name=val2)+
   
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_leaves_height[[i]]<- p2
  plot_list_leaves[[i]]<- p
  
  val3<- paste("coverage (cm2)",sp, sep = " - ")

  p3<- ggplot(sub, aes(x=cobertura, y=biomass_leaves)) +
    geom_point() +
    scale_x_continuous(name=val3)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_leaves_cobertura[[i]]<- p3
  
  val4<- paste("logcoverage (cm2)",sp, sep = " - ")
  
  p4<- ggplot(sub, aes(x=logcobertura, y=biomass_leaves)) +
    geom_point() +
    scale_x_continuous(name=val4)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_leaves_logcobertura[[i]]<- p4
  
  val5<- paste("cilynder (cm3)",sp, sep = " - ")
  
  p5<- ggplot(sub, aes(x=cilindro, y=biomass_leaves)) +
    geom_point() +
    scale_x_continuous(name=val5)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_leaves_cilindro[[i]]<- p5
  
  val6<- paste("logcilynder (cm3)",sp, sep = " - ")
  
  p6<- ggplot(sub, aes(x=logcilindro, y=biomass_leaves)) +
    geom_point() +
    scale_x_continuous(name=val6)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_leaves_logcilindro[[i]]<- p6 
  
  val7<- paste("logheight (cm)",sp, sep = " - ")
  
  p7<- ggplot(sub, aes(x=logheight, y=biomass_leaves)) +
    geom_point() +
    scale_x_continuous(name=val7)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_leaves_logheight[[i]]<- p7
   }

names(plot_list_leaves)<- levels(seedlings$species)
names(plot_list_leaves_height)<- levels(seedlings$species)
names(plot_list_leaves_cobertura)<- levels(seedlings$species)
names(plot_list_leaves_logcobertura)<- levels(seedlings$species)
names(plot_list_leaves_cilindro)<- levels(seedlings$species)
names(plot_list_leaves_logcilindro)<- levels(seedlings$species)
names(plot_list_leaves_logheight)<- levels(seedlings$species)

#save plots
pdf("allometries_leaves_plots.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_leaves[[i]])
}
dev.off()

pdf("allometries_leaves_height_plots.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_leaves_height[[i]])
}
dev.off()

pdf("allometries_leaves_cobertura_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_leaves_cobertura[[i]])
}
dev.off()

pdf("allometries_leaves_logcobertura_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_leaves_logcobertura[[i]])
}
dev.off()

pdf("allometries_leaves_cilindro_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_leaves_cilindro[[i]])
}
dev.off()

pdf("allometries_leaves_logcilindro_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_leaves_logcilindro[[i]])
}
dev.off()

pdf("allometries_leaves_logheight_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_leaves_logheight[[i]])
}
dev.off()

######
######
#plots - roots
plot_list_roots<- list()

plot_list_roots_height<- list()
plot_list_roots_logheight<- list()
plot_list_roots_cobertura<- list()
plot_list_roots_logcobertura<- list()
plot_list_roots_cilindro<- list()
plot_list_roots_logcilindro<- list()
plot_list_roots_biomassleaves<- list()
plot_list_roots_logleaves<- list()


for(i in 1:16){
  sp<- levels(seedlings$species)[i]
  sub<- seedlings[seedlings$species==sp,]
  sub1<- (sub[,c(5,7:10)])
  melt_sub<- melt(sub1)
  sub2<- data.frame(rep((sub$biomass_roots), times=ncol(sub1)),
                    melt_sub)
  
  colnames(sub2)<- c("biomass_roots", "variable", "value")
  
  val<- paste(colnames(sub2)[3],sp, sep = " - ")
  p<- ggplot(sub2, aes(x=value, y=biomass_roots))+
    geom_point()+
    scale_x_continuous(name=val)+
    facet_wrap(. ~ variable, ncol = 3, scales="free")
  
  
  plot_list_roots[[i]]<- p
  
  
  val2<- paste("height (cm)",sp, sep = " - ")
  
  p2<- ggplot(sub, aes(x=height, y=biomass_roots)) +
    geom_point() +
    scale_x_continuous(name=val2)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_height[[i]]<- p2

  
  val3<- paste("coverage (cm2)",sp, sep = " - ")
  
  p3<- ggplot(sub, aes(x=cobertura, y=biomass_roots)) +
    geom_point() +
    scale_x_continuous(name=val3)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_cobertura[[i]]<- p3
  
  val4<- paste("logcoverage (cm2)",sp, sep = " - ")
  
  p4<- ggplot(sub, aes(x=logcobertura, y=biomass_roots)) +
    geom_point() +
    scale_x_continuous(name=val4)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_logcobertura[[i]]<- p4
  
  val5<- paste("cilynder (cm3)",sp, sep = " - ")
  
  p5<- ggplot(sub, aes(x=cilindro, y=biomass_roots)) +
    geom_point() +
    scale_x_continuous(name=val5)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_cilindro[[i]]<- p5
  
  val6<- paste("logcilynder (cm3)",sp, sep = " - ")
  
  p6<- ggplot(sub, aes(x=logcilindro, y=biomass_roots)) +
    geom_point() +
    scale_x_continuous(name=val6)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_logcilindro[[i]]<- p6 
  
  val7<- paste("logheight (cm)",sp, sep = " - ")
  
  p7<- ggplot(sub, aes(x=logheight, y=biomass_roots)) +
    geom_point() +
    scale_x_continuous(name=val7)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_logheight[[i]]<- p7
  
  val8<- paste("biomass leaves (g)",sp, sep = " - ")
  
  p8<- ggplot(sub, aes(x=biomass_leaves, y=biomass_roots)) +
    geom_point() +
    scale_x_continuous(name=val8)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_biomassleaves[[i]]<- p8
  
  val9<- paste("logleaves",sp, sep = " - ")
  
  p9<- ggplot(sub, aes(x=logleaves, y=logroots)) +
    geom_point() +
    scale_x_continuous(name=val9)+
    
    geom_smooth(method="lm", aes(color="Linear Model"), se=FALSE, color=3)+
    theme_bw()+
    guides(color = guide_legend("Model Type"))
  
  plot_list_roots_logleaves[[i]]<- p9
}

names(plot_list_roots)<- levels(seedlings$species)
names(plot_list_roots_height)<- levels(seedlings$species)
names(plot_list_roots_cobertura)<- levels(seedlings$species)
names(plot_list_roots_logcobertura)<- levels(seedlings$species)
names(plot_list_roots_cilindro)<- levels(seedlings$species)
names(plot_list_roots_logcilindro)<- levels(seedlings$species)
names(plot_list_roots_logheight)<- levels(seedlings$species)
names(plot_list_roots_biomassleaves)<- levels(seedlings$species)
names(plot_list_roots_logleaves)<- levels(seedlings$species)

#save plots
pdf("allometries_roots_plotsfinal.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots[[i]])
}
dev.off()

pdf("allometries_roots_height_plotsfinal.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_height[[i]])
}
dev.off()

pdf("allometries_roots_cobertura_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_cobertura[[i]])
}
dev.off()

pdf("allometries_roots_logcobertura_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_logcobertura[[i]])
}
dev.off()

pdf("allometries_roots_cilindro_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_cilindro[[i]])
}
dev.off()

pdf("allometries_roots_logcilindro_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_logcilindro[[i]])
}
dev.off()

pdf("allometries_roots_logheight_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_logheight[[i]])
}
dev.off()

pdf("allometries_roots_biomassleaves_plots_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_biomassleaves[[i]])
}
dev.off()

pdf("allometries_roots_logleaves_final.pdf", width=12, height=6)
for (i in 1:16) {
  print(plot_list_roots_logleaves[[i]])
}
dev.off()

#####
#### Testing strength and significance of relationships

# testing the significance of correlations through cor-test (rank correlation)
install.packages("psych")
library("psych")
corr_list<- list()
for (i in 1:16){
  sp<- levels(seedlings$species)[i]
  sub<- seedlings[seedlings$species==sp,]
  sub1<- (sub[,c(5:16)])
  sub1$logleaves<-log(sub1$biomass_leaves)
  sub1$logroots<-log(sub1$biomass_roots)
  corr_list[[i]]<- psych::corr.test(sub1, adjust="none")
}
names(corr_list)<- levels(seedlings$species)
save(corr_list, file="corr_list.RData")

#check the individual correlations per species
# the first two tables (Correlation matrix and Probability values
# show the values we need
# correlation matrix = correlation strength or R2
# probability value = p-value (<0.05 = significant)

# go through the tables for each species look for each species
# highlight the parameters that are significantly correlated with
# a - biomass of leaves
# b- biomass of roots
# which of the significant have the highest correlation (r) value. 

# the matrix nb 3 with the confidence intervals we can forget for the moment

print(corr_list[[1]], short=FALSE) #species 1 = Achillea millefolium

names(corr_list)[1]



