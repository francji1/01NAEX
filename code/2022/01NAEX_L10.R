################################################################
# 01NAEX -  Lecture 10
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
###############################################################


######################
# get requirements 
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr",
                      "MASS","nlme","lme4","agricolae","gmodels",
                      "mgcv","rsm","glmmTMB","geoR","merTools")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

#library(FrF2)          #for 2^k  factorial design
#library(DoE.base)      # Full factorials, orthogonal arrays
#                         and base utilities for DoE packages
######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

# Define directory, if you do not use relative path.
#setwd("H:/01NAEX/")

##############################################################################
# Inspired by and credits for
# DTU course - Analysis of correlated data: Mixed Linear Models
# and  Montgomery DAOE - Chapter 13 
##############################################################################


# Read data
hpnir1 <- read.table("data/hplcnir1.txt",header=TRUE,sep=",")

#Data handling:   attach, $-notation, subset, transform  -----------------------------------------
hpnir1
hpnir1$hplc
hpnir1$nir
hpnir1  <- transform(hpnir1,d=hplc-nir)

#attach(hpnir1)
#hpnir12 <- subset(hpnir1,hplc>10.4)
#hpnir12
#hpnir13 <- transform(hpnir1,lognir=log(nir))
#hpnir13
#detach(hpnir1)

# Creating Graphs : par, plot, ...  ----------------------------------------

attach(hpnir1)
par(mfrow=c(1,1))
plot(hplc,nir,main="Plot of NIR vs HPLC, Example 1",  sub="The data was kindly provided by Lundbeck A/S")
abline(lsfit(hplc,nir))
detach(hpnir1)

#   Introductory example: NIR predictions of HPLC measurements-------------
# Simple analysis -------------------------------------------------------

attach(hpnir1)
mean(d)
var(d)
sd(d)
range(d)
quantile(d)
t.test(d)

# alternative
# t.test(hplc, nir, paired = TRUE)


#Simple analysis by an ANOVA approach: Data re-structuring  -------------
temp <- read.table("data/hplcnir1.txt",header=TRUE,sep=",") %>% 
  mutate(tablet=1:10) %>% 
  pivot_longer(cols = c("hplc","nir"),
               names_to = "method",
               values_to = "y") %>% 
  mutate(tablet = as.factor(tablet),
         method = as.factor(method))

with(temp,summary(aov(y~method+tablet)))
boxplot(y~method, col = "blue",data = temp, main = "Boxplot", xlab = "Method", ylab = "Content of active substance ")


#  ANOVA approach: Using function lm  ------------------------------------------------------

# options(contrasts=c(unordered="contr.SAS",ordered="contr.poly"))
# Why to run the "options" function?
# The default in R is to use the first level as zero. 
# WHEN properly understood, everything can be just as easily carried out in the default R parametrization!


attach(temp)
model1<-lm(y~method+tablet)
model1
summary(model1)
anova(model1)

# balanced design - same resutls for all types
#Anova(model1)
#Anova(model1,type=c("III"))


#  ANOVA approach: Using function estimable(LSMEANS and ESTIMATE)  ---------------------------

hplclsmeans <- matrix(c(1,1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),nrow=1)
rownames(hplclsmeans)="LS HPLC"
estimable(model1,hplclsmeans,conf.int=0.95)
summary(model1)

nirlsmeans <- matrix(c(1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),nrow=1)
rownames(nirlsmeans)="LS HPLC"
estimable(model1,nirlsmeans,conf.int=0.95)

diflsmeans <- matrix(c(0,1,0,0,0,0,0,0,0,0,0),nrow=1)
rownames(diflsmeans)=c("LS DIF")
estimable(model1,diflsmeans,conf.int=0.95)

myestimates <- matrix(c(1,1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
                        1,0,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
                        0,1,0,0,0,0,0,0,0,0,0),
                       nrow=3,byrow=T)
rownames(myestimates)=c("LS HPLC","LS NIR","LS DIF")
estimable(model1,myestimates,conf.int=0.95)

#  Analysis by mixed model  -------------------------------------------------------------

model2<-lme(y~method, random = ~1|tablet,data=temp)
model2
summary(model2)
anova(model2)

model2<-lme(y~method, random=list(tablet=~1),data=temp)
model2
summary(model2)
anova(model2)
# print  random effect and as random intercept 
ranef(model2)

model2 = lmer(y ~ method + (1 | tablet), data = temp)
# print  random effect and as random intercept 
ranef(model2)

# use functions from library(merTools) to visualise predictions
predictInterval(model2)  
REsim(model2)           
plotREsim(REsim(model2))


myestimates <- matrix(c(1,1,
                        1,0,
                        0,1),
       nrow=3,byrow=T)
rownames(myestimates)=c("LS HPLC","LS NIR","LS DIF")
estimable(model2,myestimates,conf.int=0.95)


model3<-lme(y~method-1,random=~1| tablet,data=temp)
model3
summary(model3)

myestimates <- matrix(c(1,0,
                        0,1,
                        1,-1),
               nrow=3,byrow=T)
rownames(myestimates)=c("LS HPLC","LS NIR","LS DIF")
estimable(model3,myestimates,conf.int=0.95)




#   Example with missing values==================================================================

hpnir2 <- read.table("data/hplcnir2.txt",header=TRUE,sep=",")


tmp1<-subset(hpnir2,select=hplc)
tmp1<-transform(tmp1,method="hplc",y=hplc,tablet=1:20)
tmp1<-subset(tmp1,select=c(tablet,method,y))

tmp2<-subset(hpnir2,select=nir)
tmp2<-transform(tmp2,method="nir",y=nir,tablet=1:20)
tmp2<-subset(tmp2,select=c(tablet,method,y))

temp2 <- merge(tmp1,tmp2,all=TRUE)
temp2



#  Simple analysis by an ANOVA approach  ---------------------------------------------

temp2$tablet<-factor(temp2$tablet)
temp2$method<-factor(temp2$method)

attach(temp2)

model1<-lm(y~tablet+method)
anova(model1)
summary(model1)



myestimates <- matrix(c(1,rep(0.05,19),1,
                        1,rep(0.05,19),0,
                        0,rep(0,19),1),
      nrow=3,byrow=T)
rownames(myestimates)=c("LS HPLC","LS NIR","LS DIF")
estimable(model1,myestimates,conf.int=0.95)




#  Analysis by mixed model  ---------------------------------------------------------------


temp2t <-subset(temp2,!is.na(y))
model2<-lme(y~method,random=~1| tablet,data=temp2t)
anova(model2)
summary(model2)


myestimates <- matrix(c(1,1,
                        1,0,
                        0,1),
     nrow=3,byrow=T)
rownames(myestimates)=c("LS HPLC","LS NIR","LS DIF")
estimable(model2,myestimates,conf.int=0.95)

###From: An Introduction to Statistical Methods and Data Analysis
tablets <- read.table(file="data/tablets.txt",header=TRUE)
tablets$site = factor(tablets$site)
tablets$batch = factor(tablets$batch)

# wrong way
summary(aov(content ~ site*batch , data=tablets))

#better way
summary(aov(content ~ site + batch%in%site , data=tablets))
summary(aov(content ~ site/batch, data=tablets) )
summary(aov(content ~ site + Error(batch %in% site), data=tablets))

#good way
summary(lme (content ~ 1,    random = ~1|site/batch, data = tablets))
summary(lme (content ~ site, random = ~1|site/batch, data = tablets))
summary(lmer(content ~ site + (1|site/batch),        data = tablets))

m1 = lme (content ~ site, random = ~1|site/batch, data = tablets)
m2 = lm (content ~ site, data = tablets)
anova(m1,m2)


### From: Montgomery DAO

data_purity <- read.table("data/purity.txt",header=TRUE,sep=";")
data_purity$Suppliers <- factor(data_purity$Suppliers, labels = c("Supplier 1", "Suplier 2", "Suplier 3"))
data_purity$Batches   <- factor(data_purity$Batches)
head(data_purity)
str(data_purity)

# Wrong way !!!!
purity_m1_lm   <- lm(Purity ~ Suppliers*Batches, data = data_purity)
summary(purity_m1_lm)
anova(purity_m1_lm)
# Better, but still wrong !
data_purity$Batches2 <- factor(c(1,2,3,4,5,6,7,8,9,10,11,12))
purity_m2_lm   <- lm(Purity ~ Suppliers+Batches2, data = data_purity)
summary(purity_m2_lm)
anova(purity_m2_lm)
# Better, but still wrong !
purity_m1_lme   <- lme(Purity ~ Suppliers, random = ~1|Batches, data = data_purity)
summary(purity_m1_lme)
anova(purity_m1_lme)

# Better - acount with nested design, but not mixed one
purity_m1_lm   <- lm(Purity ~ Suppliers/Batches, data = data_purity)
summary(purity_m1_lm)
anova(purity_m1_lm)

summary(aov(Purity ~ Suppliers + Batches%in%Suppliers , data=data_purity))
summary(aov(Purity ~ Suppliers/Batches, data=data_purity))
summary(aov(Purity ~ Suppliers + Error(Batches %in% Suppliers), data=data_purity))


# Right way
purity_m2_lme   <- lme(Purity ~ Suppliers, random = ~1|Suppliers/Batches, data = data_purity)
summary(purity_m2_lme)
anova(purity_m2_lme)
summary(aov(Purity ~ Suppliers, random = ~1|Suppliers/Batches, data = data_purity))

# Same
purity_m2_lmer <- lmer(Purity ~ Suppliers + (1|Batches), data = data_purity)
summary(purity_m2_lmer)
anova(purity_m2_lmer)



#### From Marianne Muller: Applied Analysis of Variance and Experimental Design (AS 2012)
paint <- read.table(file="data/paint.txt",header=TRUE)
paint$BATCH = factor(paint$BATCH)
paint$SAMPLE = factor(paint$SAMPLE)

# wrong way
summary(aov(MOISTURE ~ BATCH * SAMPLE, data=paint))
#good way
summary(aov(MOISTURE ~ BATCH + SAMPLE %in% BATCH, data=paint))
summary(aov(MOISTURE ~ BATCH/SAMPLE, data=paint))
#The analysis of variance table contains the correct test for the factor SAMPLE.
#To get the correct test for BATCH, you have to specify the appropriate error term for the factor BATCH:
summary(aov(MOISTURE ~ BATCH + Error(SAMPLE %in% BATCH), data=paint))

summary(lme(MOISTURE~1, random =~1|BATCH/SAMPLE, data=paint))



##### another example
strength <- read.table("data/strength.txt",header=TRUE,sep=";")
colnames(strength) = c("Looms", "Obs", "y")
strength$Looms <- factor(strength$Looms)
strength$Obs <- factor(strength$Obs)
summary(strength)
strength.lme <- lme(y~Looms,random=~1,data=strength)


### Another example from the package ###

fm1 <- lme(distance ~ age + Sex, data = Orthodont) # random is ~ age
fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
summary(fm1)
summary(fm2)
anova(fm1)


#############################
## Exercise 10
#########################
planks <- read.table("data/planks.txt",header=TRUE,sep=",")
head(planks)
planks$plank  <-factor(planks$plank)
planks$width  <-factor(planks$width)
planks$depth  <-factor(planks$depth)
attach(planks)

with(planks, interaction.plot(width,plank,humidity,legend=T))
with(planks, interaction.plot(depth,plank,humidity,legend=T))
with(planks, interaction.plot(width,depth,humidity,legend=T))
with(planks, interaction.plot(depth,width,humidity,legend=T))

