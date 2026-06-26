# Content ####

# R Code for data analysis of the study: Effects of mycorrhizal fungi 
# and the functional diversity of plants on ecosystem functioning: 
# an experimental approach. 

# Authors of the code: L Neuenkamp & L.Shpilkina 

# 1. data upload ####
expdata<- read.csv("expdata.csv", header=T, sep=";", dec=",")
expdata_dna<- read.csv("expdata_dna.csv", header=T, sep=";", dec=",")

#check if data has no NA - lines, otherwise remove them
expdata<- expdata[1:256,1:16]
expdata_dna<- expdata_dna[1:112, 1:18]

#data prep - only mixtures
expdata$gf<- as.factor(expdata$gf)
expdata_dna$gf<- as.factor(expdata_dna$gf)

expdata<- expdata[!expdata$gf=="monoculture",]
expdata_dna<- expdata_dna[!expdata_dna$gf=="monoculture",]

expdata$gf<- droplevels(expdata$gf)
expdata_dna$gf<- droplevels(expdata_dna$gf)

expdata$grassdom_fac<- factor(expdata$grassdom, ordered=T)
expdata_dna$grassdom_fac<- factor(expdata_dna$grassdom, ordered=T)

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

# We also account for the potential influence of shifts in
# soil nutrient availability, by including a aggregated soil variable
# via principal component analysis (1st PC axis) summarising the
# three measured variables ammonia, nitrate and phosphate in the soil,
# at the moment of harvest.

# 4. Aggregation of soil nutrients (PCA) ####

## 4.a. creation of soil PCAs ####
names(expdata) 
names(expdata_dna)

nutr<- as.data.frame(cbind(expdata$NTR, expdata$AMO, expdata$POLSEN))
nutr_dna<- as.data.frame(cbind(expdata_dna$NTR, expdata_dna$AMO, expdata_dna$POLSEN))

soilPCA<- prcomp(nutr) 
soilPCA_dna<- prcomp(nutr_dna)

summary(soilPCA) #PC1 explain about 80% of variance in soil nutrients
summary(soilPCA_dna) #PC1 explain about 80% of variance in soil nutrients

expdata$soilPC1<- soilPCA$x[,1]
expdata_dna$soilPC1<- soilPCA_dna$x[,1]

expdata$treat<- as.factor(expdata$treat)
expdata_dna$treat<- as.factor(expdata_dna$treat)

levels(expdata$treat)
levels(expdata$treat)<- c("fungal inoculum", "control inoculum")

levels(expdata_dna$treat)
levels(expdata_dna$treat)<- c("fungal inoculum", "control inoculum")


save(expdata, file="expdata.RData")
save(expdata_dna, file="expdata_dna.RData")

## 4.b. visualisation of soil variation ####
par(mfrow=c(1,1))
boxplot(expdata$NTR ~ expdata$treat*expdata$gf,
        ylab="nitrate in µgrN-NO3/gr soil", col=c("royalblue", "grey"))
boxplot(expdata$AMO ~ expdata$treat*expdata$gf,
        ylab="ammonia in µgrN-NH4/gr soil", col=c("royalblue", "grey"))
boxplot(expdata$POLSEN ~ expdata$treat*expdata$gf,
        ylab="Olsen P in µgrP-PO4/gr soil", col=c("royalblue", "grey"))

boxplot(expdata_dna$NTR ~ expdata_dna$treat*expdata_dna$gf,
        ylab="nitrate in µgrN-NO3/gr soil", col=c("royalblue", "grey"))
boxplot(expdata_dna$AMO ~ expdata_dna$treat*expdata_dna$gf,
        ylab="ammonia in µgrN-NH4/gr soil", col=c("royalblue", "grey"))
boxplot(expdata_dna$POLSEN ~ expdata_dna$treat*expdata_dna$gf,
        ylab="Olsen P in µgrP-PO4/gr soil", col=c("royalblue", "grey"))

# 5. Models growth rate ####

### 5.a. Models leaf growth rate ####

#### i) model description ####
# no log transformation
m_leaf_growthrate2<- lm(gr_leaf ~ log(rarespecnb_amf+1)*grassdom+soilPC1, data=expdata_dna)

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
m_leaf_growthrate2<- lm(log(gr_leaf+1) ~ log(rarespecnb_amf+1)*grassdom + soilPC1, data=expdata_dna)

#### iv) adjusted model validation ####

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

car::Anova(m_leaf_growthrate2, type="III") # interaction sig (p<0.1)

# direction of effects
summary(m_leaf_growthrate2)


#### vi) model comparison ####
# using proportion of grasses as ordered factor
# in the final model
# idea: seeing whether the patterns hold

m_leaf_growthratefac<- lm(log(gr_leaf+1) ~ log(rarespecnb_amf+1)*grassdom_fac + soilPC1, data=expdata_dna)
# model leaf growth rate & treatment
par(mfrow=c(1,2))
plot(predict(m_leaf_growthratefac),residuals(m_leaf_growthratefac), 
     main="variance homogeneity")
# aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_leaf_growthratefac), main="normality")

# significance testing
car::Anova(m_leaf_growthratefac, type="III")
car::Anova(m_leaf_growthrate2, type="III")

# conclusion: pattern remain mostly the same,
#             marg. sig. interaction of prop. grasses with AMF richness
#             becomes non-significant. This likely 
#             reflects the lower stat. power due to more
#             factor levels when prop. grasses is a ordered factor.
#             Prop. grasses remains as cont. variable in the 
#             published analysis, yet weakness of 
#             effect is highlighted.

### 5.b. Models root growth rate ####

#### i) model description ####
# no log transformation
m_root_growthrate2<- lm(gr_root ~ log(rarespecnb_amf+1)*grassdom+soilPC1, data=expdata_dna)

# model leaf growth rate & AMF richness
par(mfrow=c(1,2))
plot(predict(m_root_growthrate2),residuals(m_root_growthrate2), 
     main="variance homogeneity")
#aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_root_growthrate2), main="normality")
#aim: plots within the blue area on one line

#result: residuals show heterogeneity -> log transformation of response variable

#### iii) model adjustment ####
# log transformation
m_root_growthrate2<- lm(log(gr_root+1) ~ log(rarespecnb_amf+1)*grassdom+soilPC1, data=expdata_dna)

#### iv) adjusted model validation ####

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

car::Anova(m_root_growthrate2, type="III") # interaction ns --> re-modelling

# re-modelling of root growth rate models

m_root_growthrate2<- lm(log(gr_root+1) ~ log(rarespecnb_amf+1)+grassdom+soilPC1, data=expdata_dna)
car::Anova(m_root_growthrate2, type="II")

#direction of effects
summary(m_root_growthrate2)

#### vi) model comparison ####
# using proportion of grasses as ordered factor
# in the final model
# idea: seeing whether the patterns hold

m_root_growthratefac<- lm(log(gr_root+1) ~ log(rarespecnb_amf+1)+grassdom_fac+soilPC1, data=expdata_dna)

# model leaf growth rate & treatment
par(mfrow=c(1,2))
plot(predict(m_root_growthratefac),residuals(m_root_growthratefac), 
     main="variance homogeneity")
# aim: chaotic point cloud, no triangle
car::qqPlot(residuals(m_root_growthratefac), main="normality")

# significance testing
car::Anova(m_root_growthratefac, type="II")
car::Anova(m_root_growthrate2, type="II")

# conclusion: patterns stay the same, prop. grasses remains unimportant.

### 5.c. plotting results ####

My_Theme = theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 12),
  legend.text = element_text(size=12),
  legend.title = element_text(size=14))+
  theme_minimal()

expdata$treat<- as.factor(expdata$treat)
levels(expdata$treat)
levels(expdata$treat)<- c("fungal inoculum", "control inoculum")
expdata$treatment<- expdata$treat
levels(expdata$treatment)<- c("NM", "M")

#### i) growthrate, treatment & grass dominance ####
pleaf<- ggplot(expdata, aes(x=grassdom, y=gr_leaf, col=treat))+
  geom_point()+
  geom_smooth(method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log')))+
  labs(colour = "treatment", x =" proportion of grasses (%)", 
       y="growth rate of leaf biomass (g increase March-June)" )+
  scale_color_manual(values = c("royalblue", "black"))+
  ylim(c(0,7))+
  theme_classic()+
  annotate("text", x=0, y=6, 
           label= "Anova:\ntreatment - p < 0.05\nprop. grasses - p < 0.05",
           hjust=0, size=4) + 
  My_Theme

pleaf

proot<- ggplot(expdata, aes(x=grassdom, y=gr_root, col=treat))+
  geom_point()+
  geom_smooth(method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log')))+
  labs(colour = "treatment", x =" proportion of grasses (%)", 
       y="growth rate of root biomass (g increase March-June)" )+
  scale_color_manual(values = c("royalblue", "black"))+
  ylim(c(0,7))+
  theme_classic()+
  annotate("text", x=0, y=6, 
           label= "Anova:\ntreatment - p < 0.05\nprop. grasses - n.s.",
           hjust=0, size=4) + 
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
  annotate("text", x=10, y=1.60, 
           label= "Anova:\nAMF richness - n.s.\nprop.grasses - p < 0.05\nAMF*grasses - p < 0.1",
           hjust=0, size=4)

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
  annotate("text", x=10, y=1.55, 
           label= "Anova: \nAMF richness - p < 0.05\nprop.grasses - n.s.\nAMF*grasses - n.s.",
           hjust=0, size=4)

ramf_p2

lamf_p2+ramf_p2 + 
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'A')

# 6. Models AMF richness ####
## 6.a. Models AMF Treatment Effects ####

# we use rarefied richness where we control for 
# differential sampling depth (nb of reads)
# which influence measures of species number

# Modelling logic is as for growth rate.

#### i) model description ####

mamf<- lm(rarespecnb_amf ~ treat*grassdom+soilPC1, data=expdata_dna)

#### ii) model validation ####
par(mfrow=c(1,2))
plot(predict(mamf),residuals(mamf), 
     main="variance homogeneity")
# aim: chaotic point cloud, no triangle
car::qqPlot(residuals(mamf), main="normality")
# aim: plots within the blue area on one line

# result: residuals show heterogeneity -> log transformation of response variable

#### iii) model adjustment ####

mamf<- lm(log(rarespecnb_amf+1) ~ treat*grassdom+soilPC1, data=expdata_dna)

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

mamf<- lm(log(rarespecnb_amf+1) ~ treat+grassdom+soilPC1, data=expdata_dna)
car::Anova(mamf, type="II")

#effect direction
summary(mamf)


## 6.b. plotting results ####

expdata_dna$treatment<- expdata_dna$treat
expdata_dna$treatment<- as.factor(expdata_dna$treatment)
levels(expdata_dna$treatment)<- c("fungal inoculum", "control inoculum")

pr<- ggplot(expdata_dna, aes(x=treatment, y=rarespecnb_amf, col=treatment))+
  geom_boxplot()+
  theme_classic()+
  xlab("treatment")+
  ylab("rarefied AMF richness")+
  scale_color_manual(values = c("royalblue", "black"))+
  annotate("text", x=0.5, y=70, 
           label= "Anova: \ntreatment - p < 0.05\nprop. grasses - n.s.",
           hjust=0, size=4) +
  annotate("text", x= 1.5, y=40,
           label="**", hjust=0, size=8)+
  My_Theme

pr


