# Content ####

# R Code for data analysis of the study: Effects of mycorrhizal fungi 
# and the functional diversity of plants on ecosystem functioning: 
# an experimental approach. 

# Authors of the code: L Neuenkamp & L.Shpilkina 

# 1. data upload ####
expdata<- read.csv("expdata.csv", header=T, sep=";", dec=",")
expdata_dna<- read.csv("expdata_dna.csv", header=T, sep=";", dec=",")

#data prep - only mixtures
expdata<- expdata[!expdata$gf=="monoculture",]
expdata_dna<- expdata_dna[!expdata_dna$gf=="monoculture",]

# 2. package upload ####
library(vegan)
library(ggplot2)
library(car)
library(patchwork)
library(ggcorrplot)
library(GGally)
library(interactions)

# 3. Modelling logic (all models)####

# Modelling logic is to always go with the simplest model
# We start with testing effects of grass dominance and treatment
# as well as their interaction.
# if the interaction term turns out to be non-significant
# we test grass dominance and treatment only as additive effects
# Log transformation ins response and explanatory variables are performed
# where needed to assure variance homogeneity and normality of model
# residuals

# We always model response variables with grass dominance and
# treatment or AMF richness as interactive predictors. 

# 4. Models growth rate ####

### 4.a. Models leaf growth rate ####

#### i) model description ####
# no log transformation
m_leaf_growthrate1<- lm(gr_leaf ~ treat*grassdom, data=expdata)
m_leaf_growthrate2<- lm(gr_leaf ~ log(rarespecnb_amf+1)*grassdom, data=expdata_dna)

#### ii) model validation ####
# model leaf growth rate & treatment
par(mfrow=c(1,2))
plot(predict(m_leaf_growthrate1),residuals(m_leaf_growthrate1), 
     main="variance homogeneity")
#aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_leaf_growthrate1), main="normality")
#aim: plots within the blue area on one line

#result: residuals show heterogeneity -> log transformation of response variable

# model leaf growth rate & AMF richnes
par(mfrow=c(1,2))
plot(predict(m_leaf_growthrate2),residuals(m_leaf_growthrate2), 
     main="variance homogeneity")
#aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_leaf_growthrate2), main="normality")
#aim: plots within the blue area on one line

#result: residuals show heterogeneity -> log transformation of response variable

#### iii) model adjustment ####
# log transformation
m_leaf_growthrate1<- lm(log(gr_leaf+1) ~ treat*grassdom, data=expdata)
m_leaf_growthrate2<- lm(log(gr_leaf+1) ~ log(rarespecnb_amf+1)*grassdom, data=expdata_dna)

#### iv) adjusted model validation ####

# model leaf growth rate & treatment
par(mfrow=c(1,2))
plot(predict(m_leaf_growthrate1),residuals(m_leaf_growthrate1), 
     main="variance homogeneity")
# aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_leaf_growthrate1), main="normality")
# aim: plots within the blue area on one line

# results: now it looks good, and we can test the significances with ANOVA 
# Type III which can handle interactions


# model leaf growth rate & AMF richness
par(mfrow=c(1,2))
plot(predict(m_leaf_growthrate2),residuals(m_leaf_growthrate2), 
     main="variance homogeneity")
#aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_leaf_growthrate2), main="normality")
#aim: plots within the blue area on one line

# results: now it looks good, and we can test the significances with ANOVA 
# Type III which can handle interactions

#### v) significance testing ####

# logic: Testing first with type III Anova for interactions.
#        As type II Anova is more powerful, in case of non-sig. interactions
#        re-modelling with only additive effects, re-testing with type II Anova.

car::Anova(m_leaf_growthrate1, type="III") # interaction ns --> re-modelling
car::Anova(m_leaf_growthrate2, type="III") # interaction sig (p<0.1)

# re-modelling of leaf growth rate*treatment model

m_leaf_growthrate1<- lm(log(gr_leaf+1) ~ treat+grassdom, data=expdata)
car::Anova(m_leaf_growthrate1, type="II")

#direction of effects
summary(m_leaf_growthrate1)
summary(m_leaf_growthrate2)


### 4.b. Models root growth rate ####

#### i) model description ####
# no log transformation
m_root_growthrate1<- lm(gr_root ~ treat*grassdom, data=expdata)
m_root_growthrate2<- lm(gr_root ~ log(rarespecnb_amf+1)*grassdom, data=expdata_dna)

#### ii) model validation ####
# model leaf growth rate & treatment
par(mfrow=c(1,2))
plot(predict(m_root_growthrate1),residuals(m_root_growthrate1), 
     main="variance homogeneity")
#aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_root_growthrate1), main="normality")
#aim: plots within the blue area on one line

#result: residuals show heterogeneity -> log transformation of response variable

# model leaf growth rate & AMF richnes
par(mfrow=c(1,2))
plot(predict(m_root_growthrate2),residuals(m_root_growthrate2), 
     main="variance homogeneity")
#aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_root_growthrate2), main="normality")
#aim: plots within the blue area on one line

#result: residuals show heterogeneity -> log transformation of response variable

#### iii) model adjustment ####
# log transformation
m_root_growthrate1<- lm(log(gr_root+1) ~ treat*grassdom, data=expdata)
m_root_growthrate2<- lm(log(gr_root+1) ~ log(rarespecnb_amf+1)*grassdom, data=expdata_dna)

#### iv) adjusted model validation ####

# model leaf growth rate & treatment
par(mfrow=c(1,2))
plot(predict(m_root_growthrate1),residuals(m_root_growthrate1), 
     main="variance homogeneity")
# aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_root_growthrate1), main="normality")
# aim: plots within the blue area on one line

# results: now it looks good, and we can test the significances with ANOVA 
# Type III which can handle interactions


# model leaf growth rate & AMF richness
par(mfrow=c(1,2))
plot(predict(m_root_growthrate2),residuals(m_root_growthrate2), 
     main="variance homogeneity")
#aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_root_growthrate2), main="normality")
#aim: plots within the blue area on one line

# results: now it looks good, and we can test the significances with ANOVA 
# Type III which can handle interactions

#### v) significance testing ####

# logic: Testing first with type III Anova for interactions.
#        As type II Anova is more powerful, in case of non-sig. interactions
#        re-modelling with only additive effects, re-testing with type II Anova.

car::Anova(m_root_growthrate1, type="III") # interaction ns --> re-modelling
car::Anova(m_root_growthrate2, type="III") # interaction ns --> re-modelling

# re-modelling of root growth rate models

m_root_growthrate1<- lm(log(gr_root+1) ~ treat+grassdom, data=expdata)
car::Anova(m_root_growthrate1, type="II")

m_root_growthrate2<- lm(log(gr_root+1) ~ log(rarespecnb_amf+1)+grassdom, data=expdata_dna)
car::Anova(m_root_growthrate2, type="II")

#direction of effects
summary(m_leaf_growthrate1)
summary(m_leaf_growthrate2)

### 4.c. plotting results ####

My_Theme = theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 12),
  legend.text = element_text(size=12),
  legend.title = element_text(size=14))

expdata$treatment<- expdata$treat
levels(expdata$treatment)<- c("NM", "M")

#### i) growthrate, treatment & grass dominance ####
pleaf<- ggplot(expdata, aes(x=grassdom, y=gr_leaf, col=treat))+
  geom_point()+
  geom_smooth(method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log')))+
  labs(colour = "treatment", x =" proportion of grasses (%)", 
       y="growth rate of leaf biomass (g increase March-June)" )+
  scale_color_manual(values = c("#d95f02", "#1b9e77"))+
  ylim(c(0,7))+
  theme_classic()+
  annotate("text", x=0, y=6, 
           label= "Anova Type II (log(leaf growth rate+1) ~ treatment+prop. grasses):
           \nn = 160\ntreatment: df = 1, F = 3.574, p = 0.061\nprop. grasses: df = 1, F = 11.514, p = 0.001",
           hjust=0, size=3) + 
  My_Theme

pleaf

proot<- ggplot(expdata, aes(x=grassdom, y=gr_root, col=treat))+
  geom_point()+
  geom_smooth(method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log')))+
  labs(colour = "treatment", x =" proportion of grasses (%)", 
       y="growth rate of root biomass (g increase March-June)" )+
  scale_color_manual(values = c("#d95f02", "#1b9e77"))+
  ylim(c(0,7))+
  theme_classic()+
  annotate("text", x=0, y=6, 
           label= "Anova Type II (log(root growth rate+1) ~ treatment+prop. grasses):
           \nn = 160\ntreatment: df = 1, F = 6.735, p = 0.010\nprop. grasses: df = 1, F = 1.866, p = 0.174",
           hjust=0, size=3) + 
  My_Theme

proot

pleaf+proot + 
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'A')

#### ii) growthrate, AMF richness & grass dominance ####

# plotting interaction between AMF richness and growth rate:
# create predictions for min and max grass prop
# using interaction plot

#link; https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html

lamf_p<- interact_plot(m_leaf_growthrate2, pred = rarespecnb_amf, 
                       modx = grassdom, 
                       plot.points = TRUE,
                       modx.values = c(0, 25, 50, 75, 100),
                       x.label = "rarefied AMF richness", 
                       y.label = "(log) growth rate of leaf biomass (g increase March-June)",
                       legend.main = "prop. grasses")


lamf_p

lamf_p2<- lamf_p+
  annotate("text", x=20, y=1.60, 
           label= "Anova Type III (log(leaf growth rate+1) ~ log(AMF richness+1)*prop. grasses):
           \nn = 80\nAMF richness: df = 1, F = 0.047, p = 0.829\nprop.grasses: df = 1, F = 7.198, p = 0.009\nAMF*grasses: df=1, F = 3.564, p = 0.063",
           hjust=0, size=3)

lamf_p2
#anova results need to be added in pdf later
#grass dom sig, int marg. sig

ramf_p<- interact_plot(m_root_growthrate2, pred = 
                         rarespecnb_amf, 
                       modx = grassdom, 
                       plot.points = TRUE,
                       modx.values = c(0, 25, 50, 75, 100),
                       x.label = "rarefied AMF richness", 
                       y.label = "(log) growth rate of root biomass (g increase March-June)",
                       legend.main = "prop. grasses")

ramf_p

ramf_p2<- ramf_p+
  annotate("text", x=20, y=1.55, 
           label= "Anova Type II (log(root growth rate+1) ~ log(AMF richness+1)+prop. grasses):
           \nn = 80\nAMF richness: df = 1, F = 8.810, p = 0.004\nprop.grasses: df = 1, F = 0.818, p = 0.369\n   ",
           hjust=0, size=3)

ramf_p2

lamf_p2+ramf_p2 + 
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'A')

# 5. Models AMF richness ####
## 5.a. Models AMF Treatment Effects ####

# we use rarefied richness where we control for 
# differential sampling depth (nb of reads)
# which influence measures of species number

# Modelling logic is as for growth rate.

#### i) model description ####

mamf<- lm(rarespecnb_amf ~ treat*grassdom, data=expdata_dna)

#### ii) model validation ####
par(mfrow=c(1,2))
plot(predict(mamf),residuals(mamf), 
     main="variance homogeneity")
# aim: chaotic point cloud, no triangle
car::qqPlot(residuals(mamf), main="normality")
# aim: plots within the blue area on one line

# result: residuals show heterogeneity -> log transformation of response variable

#### iii) model adjustment ####

mamf<- lm(log(rarespecnb_amf+1) ~ treat*grassdom, data=expdata_dna)

#### iv) adjusted model validation ####
par(mfrow=c(1,2))
plot(predict(mamf),residuals(mamf), 
     main="variance homogeneity")
# aim: chaotic point cloud, no triangle
car::qqPlot(residuals(mamf), main="normality")
# aim: plots within the blue area on one line

# result: now it looks good, and we can test the significance with ANOVA 
# Type III which can handle interactions

#### v) significance testing ####

# logic: Testing first with type III Anova for interactions.
#        As type II Anova is more powerful, in case of non-sig. interactions
#        re-modelling with only additive effects, re-testing with type II Anova.

car::Anova(mamf, type="III") # interaction not significant --> re-modelling

mamf<- lm(log(rarespecnb_amf+1) ~ treat+grassdom, data=expdata_dna)
car::Anova(mamf, type="II")

#effect direction
summary(mamf)

## 5.b. plotting results ####

expdata_dna$treatment<- expdata_dna$treat
levels(expdata_dna$treatment)<- c("NM", "M")

pr<- ggplot(expdata_dna, aes(x=treatment, y=rarespecnb_amf, col=treatment))+
  geom_boxplot()+
  theme_classic()+
  xlab("treatment")+
  ylab("rarefied AMF richness")+
  scale_color_manual(values = c("#d95f02", "#1b9e77"))+
  annotate("text", x=1.025, y=70, 
           label= "Anova Type II (log(raref. AMF+1) ~ treatment+prop. grasses):
           \nn = 80\ntreatment: df = 1, F = 7.319, p = 0.008\nprop. grasses: df = 1, F = 1.169, p = 0.187",
           hjust=0, size=3.5) +
  annotate("text", x=1.5, y=40,
           label="**", hjust=0, size=8)+
  My_Theme

pr


