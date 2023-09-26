################################################################
# 01NAEX -  Lecture 08
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code are from R Companion to Montgomery's DAoE and
################################################################

######################
# get requirements 
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr",
                      "MASS","agricolae","nlme","lme4","agricolae",
                      "scatterplot3d","FrF2","rsm","DoE.base","geoR",
                      "qualityTools","alr4")
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
#setwd("H:/01NEX_2020/")

##############################################################################
# Follow Chapter 8 - Montgomery DAOE
##############################################################################

##############################################################################
### One half fraction of the 2^k design
##############################################################################

#EXAMPLE 8_1
k = 4
design8_1          <-  FrF2(2^(k-1), k, replications = 1, randomize = FALSE,factor.names = c("A", "B", "C", "D"))
Filtration         <-  c(45,100,45,65,75,60,80,96)
design8_1          <-  add.response(design8_1, Filtration)
summary(design8_1)


# Main Effects plotfor response variable
MEPlot(design8_1)
# Interaction Plot matrix for response variable
IAPlot(design8_1)

#Boxplots
opar <- par(mfrow=c(2,2))
with(design8_1, boxplot(Filtration ~ A ,col = "lightgray", main = "Factor A - Temperature"))
with(design8_1, boxplot(Filtration ~ B ,col = "lightgray", main = "Factor B - Pressure"))
with(design8_1, boxplot(Filtration ~ C ,col = "lightgray", main = "Factor C - Concentration"))
with(design8_1, boxplot(Filtration ~ D ,col = "lightgray", main = "Factor D - Stirring rate"))
title(main="Box plots of all factors", line=-1, outer=T, cex.main=1)
par(opar)


## Daniel plot and Half normal plot are usually used for unreplicatd design8_1
# Daniel Plot with alpha = 0.5 and only significant factors
DanielPlot(design8_1,code=TRUE)
# Classical effects qqplot
qqplot(DanielPlot(design8_1)$x,DanielPlot(design8_1)$y)
qqline(DanielPlot(design8_1)$y)
# half normal plot of effects
DanielPlot(design8_1,code=TRUE,alpha=0.05,half=TRUE)

qqline(DanielPlot(design8_1,code=TRUE,alpha=0.05,half=TRUE)$y)
# Pareto plot
design8_1_qt = fracDesign(k = 4, p =1, replicates = 1,random.seed 	= 0)
Filtration_qt= design8_1$Filtration[design8_1_qt[,1]]
response(design8_1_qt) = Filtration_qt
paretoPlot(design8_1_qt)

summary(aov(Filtration~as.factor(A)+as.factor(B)+as.factor(C)+as.factor(D), data = design8_1) )
summary(aov(Filtration~as.factor(A)+as.factor(C)+as.factor(D), data = design8_1) )
summary(aov(Filtration~as.factor(A)+as.factor(C)+as.factor(D), data = design8_1) )


design8_1$A.num <- 2*(as.numeric(design8_1$A)-1.5)
design8_1$B.num <- 2*(as.numeric(design8_1$B)-1.5)
design8_1$C.num <- 2*(as.numeric(design8_1$C)-1.5)
design8_1$D.num <- 2*(as.numeric(design8_1$D)-1.5)
design8_1.lm    <- lm(Filtration~-1+A.num+B.num+C.num+D.num+A.num:B.num+A.num:C.num+A.num:D.num,data=design8_1)
summary(design8_1.lm )


design8_1.lm2    <- lm(Filtration~A.num+C.num+D.num+A.num:C.num+A.num:D.num,data=design8_1)
summary(design8_1.lm2 )

summary(aov(Filtration ~ A*C*D + B -C:D - A:C:D, data = design8_1 ))


#EXAMPLE 8_2
k = 5
design8_2          <-  FrF2(2^(k-1), k, replications = 1, randomize = FALSE,factor.names = c("A", "B", "C", "D", "E"))
yield              <-  c(8,9,34,52,16,22,45,60,6,10,30,50,15,21,44,63)
design8_2          <-  add.response(design8_2, yield)
summary(design8_2)

# Main Effects plotfor response variable
MEPlot(design8_2)
# Interaction Plot matrix for response variable
IAPlot(design8_2)

#Boxplots
opar <- par(mfrow=c(2,3))
with(design8_2, boxplot(yield ~ A ,col = "lightgray", main = "Factor A - Aperture Setting"))
with(design8_2, boxplot(yield ~ B ,col = "lightgray", main = "Factor B - Explosure Time"))
with(design8_2, boxplot(yield ~ C ,col = "lightgray", main = "Factor C - Develop Time"))
with(design8_2, boxplot(yield ~ D ,col = "lightgray", main = "Factor D - Mask Dimension"))
with(design8_2, boxplot(yield ~ E, col = "lightgray", main = "Factor E - Etch Time"))
title(main="Box plots of all factors", line=-1, outer=T, cex.main=1)
par(opar)    


## Daniel plot and Half normal plot are usually used for unreplicatd design8_2
# Daniel Plot with alpha = 0.5 and only significant factors
DanielPlot(design8_2,code=TRUE)
# Classical effects qqplot
qqplot(DanielPlot(design8_2)$x,DanielPlot(design8_2)$y)
qqline(DanielPlot(design8_2)$y)
# half normal plot of effects
DanielPlot(design8_2,code=TRUE,alpha=0.5,half=TRUE)

qqline(DanielPlot(design8_2,code=TRUE,alpha=0.05,half=TRUE)$y)
# Pareto plot
design8_2_qt = fracDesign(k = 5, p =1, replicates = 1,random.seed 	= 0)
yield_qt= design8_2$yield[design8_2_qt[,1]]
response(design8_2_qt) = yield_qt
paretoPlot(design8_2_qt)


summary(aov(yield~A*B +C, data = design8_2) )

par(mfrow=c(2,2))
plot(lm(yield~A*B +C, data = design8_2) )




# one quater fraction of the 2^k design
#Example 8-4
k = 6
design8_4          <-  FrF2(2^(k-2), k, replications = 1, randomize = FALSE,generators = c("ABC","BCD") ,factor.names = c("A", "B", "C", "D", "E", "F"))    #
Shringage          <-  c(6,10,32,60,4,15,26,60,8,12,34,60,16,5,37,52)
design8_4          <-  add.response(design8_4, Shringage)
summary(design8_4)

 #Boxplots
opar <- par(mfrow=c(2,3))
with(design8_4, boxplot(yield ~ A ,col = "lightgray", main = "Factor A -Mold Temperature"))
with(design8_4, boxplot(yield ~ B ,col = "lightgray", main = "Factor B - Screw Speed"))
with(design8_4, boxplot(yield ~ C ,col = "lightgray", main = "Factor C - Holding Time"))
with(design8_4, boxplot(yield ~ D ,col = "lightgray", main = "Factor D - Cycle Time"))
with(design8_4, boxplot(yield ~ E, col = "lightgray", main = "Factor E - gate Size"))
with(design8_4, boxplot(yield ~ F, col = "lightgray", main = "Factor E - Hold Pressure"))
title(main="Box plots of all factors", line=-1, outer=T, cex.main=1)
par(opar)

# Main Effects plotfor response variable
MEPlot(design8_4)
# Interaction Plot matrix for response variable
IAPlot(design8_4)


## Daniel plot and Half normal plot are usually used for unreplicatd design8_4
# Classical effects qqplot
qqplot(DanielPlot(design8_4)$x,DanielPlot(design8_4)$y)
qqline(DanielPlot(design8_4)$y)
# half normal plot of effects
DanielPlot(design8_4,code=TRUE,alpha=0.5,half=TRUE)

# Pareto plot
design8_4_qt = fracDesign(k = 5, p =1, replicates = 1,random.seed 	= 0)
Shringage_qt= design8_4$Shringage[design8_4_qt[,1]]
response(design8_4_qt) = Shringage_qt
paretoPlot(design8_4_qt)

summary(aov(yield~A*B, data = design8_4) )

design8_4$A.num <- 2*(as.numeric(design8_1$A)-1.5)
design8_4$B.num <- 2*(as.numeric(design8_1$B)-1.5)
design8_4.lm    <- lm(Shringage~A.num*B.num,data=design8_4)
summary(design8_4.lm )

opar <- par(mfrow=c(2,2))
plot(design8_4.lm )
par(opar)


# https://cran.r-project.org/web/views/ExperimentalDesign.html



# Example of BP design from ?pb
pb(12,randomize=FALSE)
pb(12,randomize=FALSE,n12.taguchi=TRUE)
pb(20,seed=29869)
pb(16,factor.names=list(A="",B="",C="",D=c("min","max"),
                        E="",F="",G="",H="",J=c("new","old")))
pb(8,default.levels=c("current","new"))
test <- pb(40) ## design created by doubling the 20 run design
summary(test)
pb(12, ncenter=6) ## 6 center points with default placement









