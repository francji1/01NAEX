################################################################################
# 01NAEX - Exercise 04
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment
################################################################################

######################
# get requirements for the Exercise
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr","MASS",
                      "agricolae","scatterplot3d","FrF2","DoE.base","alr3")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

# Define directory, if you do not use relative path in R project.
#setwd("M:/01NAEX/")
###############################################################################

# Solve Problems 4-23, 4.40, and 4-42 from Montgomery.

Problem_4_23 <- read.table("data/Problem_4_23.txt",header=TRUE,sep=";")
Problem_4_40 <- read.table("data/Problem_4_40.txt",header=TRUE,sep=";")
Problem_4_42 <- read.table("data/Problem_4_42.txt",header=TRUE,sep=";")

Problem_4_23
Problem_4_23 <- Problem_4_23 %>% 
  mutate(Order= factor(Order),
         Operator = factor(Operator))
anova(lm(Time~Method+Order+Operator,Problem_4_23))


Problem_4_40
Problem_4_40 <- Problem_4_40 %>% 
  mutate(Additive= factor(Additive),
         Car = factor(Car))
str(Problem_4_40)
summary(aov(Mileage~Additive+Car+Error(Car),Problem_4_40))

# Not this way
anova(lm(Mileage~Car+Additive,Problem_4_40))
anova(lm(Mileage~Additive+Car,Problem_4_40))


Problem_4_42
Problem_4_42 <- Problem_4_42 %>% 
  mutate(Concentration = factor(Concentration ),
         Days = factor(Days))
summary(aov(Strength~Concentration+Days+Error(Days),Problem_4_42))

# Not this way
anova(lm(Strength~Concentration+Days,Problem_4_42))
anova(lm(Strength~Days+Concentration,Problem_4_42))




