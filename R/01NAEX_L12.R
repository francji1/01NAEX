################################################################
# 01NAEX -  Lecture 12
# Introduction to Longitudinal Data Analysis
###############################################################

######################
# get requirements 
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr",
                      "MASS","agricolae","nlme","lme4","agricolae",
                      "scatterplot3d","FrF2","rsm","DoE.base","geoR",
                      "alr4","sjstats","lsmeans","lmerTest",
                      "gdata","gmodels","gplots","gtools") # "qualityTools"
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)
######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

#install.packages("lcsm")
#library(lcsm)

# Define directory, if you do not use relative path.
#setwd("M:/01NAEX/")

# Inspired by and credits for
# DTU course - Analysis of correlated data: Mixed Linear Models
# and  Montgomery DAOE - Chapter 13 
##############################################################################




#### Orthodont data analysis: dental study:   Pothoff and Roy (1964)

data ( Orthodont ) 
head(Orthodont) 
summary(Orthodont) 

attach(Orthodont)
table(age)
table(Subject)

# Visualisation
# by xyplot from lattice

xyplot(distance ~ age | Subject, data=Orthodont, as.table=T)

xyplot(distance~age | Subject,data=Orthodont,
       prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
       xlab = "Age", ylab = "distance",
       panel = function(x, y) {
           panel.xyplot(x, y)
           panel.loess(x,y, family="gaussian") },
        as.table=T)

xyplot(distance~age | Subject,data=Orthodont,
       panel = function(x, y){
           panel.xyplot(x, y)
           panel.lmline(x, y)
       }, as.table=T)

xyplot(distance ~ age  | Sex, data = Orthodont, groups = Subject,
       type = "o", panel = panel.superpose)


xyplot(distance ~ age  | Sex, data = Orthodont, groups = Subject,
       type = "o", panel = function(x, y){
           panel.xyplot(x, y)
           panel.lmline(x, y)
       })


boxplot(distance[Sex == "Male"] ~ age[Sex == "Male"], xlab = "Age", ylab = "Distance", main ="Boxplot for Male" )
boxplot(distance[Sex == "Female"] ~ age[Sex == "Female"], xlab = "Age", ylab = "Distance", main ="Boxplot for Female" )


interaction.plot(age[Sex == "Male"], Subject[Sex == "Male"], distance[Sex == "Male"]) 


## by ggolot

ggplot(Orthodont, aes(factor(as.numeric(age)), distance), fill = Sex) +
    geom_violin() +
    geom_boxplot(width = 0.1, outlier.colour = "blue") +
    theme_classic()


ggplot(Orthodont, aes(factor(as.numeric(age)), y = distance, fill = Sex)) +
    geom_boxplot(alpha=0.7)+
    theme_classic()

ggplot(Orthodont, aes(x = age, y = distance)) +
    geom_point() +
    geom_line() +
 #   stat_smooth(method = "loess", se = F, span = .9) +
    facet_wrap(~Subject)


ggplot(Orthodont,aes(x = age, y = distance)) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    facet_wrap(~Subject)


###############################  Pooling
# wrong way


fit1 <- lm ( distance ~ age * Sex , Orthodont) 
summary(fit1) 

summary(aov( distance ~ age * Sex , Orthodont))


fit2 <- lm( distance ~ age + age:Sex ,Orthodont ) 
summary( fit2 ) 


fit3 <- lm( distance ~ age + Sex ,Orthodont ) 
summary( fit3 ) 


summary(lm ( distance ~ age + Sex , Orthodont))
summary(aov( distance ~ age + Sex , Orthodont))




summary(lm( distance ~ age *Subject , Orthodont))

##Ftest date by date
fValues <- rep(0, 4)
names(fValues) <- levels(as.factor(age))
for (i in 1:4){
    fValues[as.integer(i)] <- anova(lm(distance[age == unique(age)[i]] ~ Sex[age == unique(age)[i]], data = Orthodont))[1, 4]
    }
fValues
qf(0.95,1,25)

#### Correlation structure

# Plot autocorrelation
fit0 <- lme( distance ~ age * Sex, Orthodont,random = ~ 1 + age | Subject) 
fit0
plot(ACF(fit0), alpha=.05)

#AR(1)
fitAR1 <- update(fit0, correlation=corARMA(p=1,q=0, form=~ 1 | Subject))
#ARMA(1,1)
#fitARMA11 <- update(fit0, correlation=corARMA(p=1,q=1, form=~ 1 | Subject))
AIC(fit, fitAR1)


fit <- lme( distance ~ age * Sex, Orthodont, 
            random = ~ 1 + age | Subject, 
            correlation  = corAR1 ( form = ~ 1 | Subject)) 
fit
summary(fit)

m_gls<-gls(distance ~ age + Sex,
        correlation=corCompSymm(form=~1|Subject),data=Orthodont)
m_gls
summary(m_gls)



m_lme<-lme(distance ~ age * Sex,
        random=~1+age|Subject,
        correlation=corAR1(form=~1|Subject),
        data=Orthodont)
m_lme
summary(m_lme)
plot(Variogram(m_lme, form = ~1|Subject, data = Orthodont))



# ML to compare models without outlier M09
m_ml <- lme(distance  ~  Sex + I(age-11), random =  ~  I(age-11) | Subject,
                    data=Orthodont, method="ML")
m_ml2 <- update(m_ml, fixed = distance  ~  Sex * I(age-11))

anova(m_ml,m_ml2)
# update back to reml
m_reml <- update(m_ml2, method="REML")
summary(m_reml)

m_ml_ar <- update(m_ml2, correlation=corAR1(form= ~age|Subject))
anova(m_ml2, m_ml_ar)


# Correlation structure fro VarCorr is text, to obtain numeric  vector use ..
m_lme1 <- lme(distance ~ Sex, data = Orthodont, random = ~ age|Subject)
vc <- VarCorr(m_lme1)
m_lme1
suppressWarnings(storage.mode(vc) <- "numeric")
vc
vc[1:2,"StdDev"]


#simCor1(phi=0.4,sdgrp=2,sdres=1,seed=42) 

m_lme2<-lme(distance ~ age * Sex,
           random=~1+age|Subject,
           correlation=corExp(form=~1|Subject),
           data=Orthodont)
m_lme2
summary(m_lme2)


# better for longer time series

plot(Variogram(m_lme2, form = ~1|Subject, data = Orthodont))
intervals(m_lme)
VarCorr( m_lme )
getVarCov( m_lme )


fm1 <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary)
plot(Variogram(fm1, form = ~ Time | Mare, maxDist = 0.7))


# more about variogram on
# https://sakai.unc.edu/access/content/group/2842013b-58f5-4453-aa8d-3e01bacbfc3d/public/Ecol562_Spring2012/docs/lectures/lecture32.htm
