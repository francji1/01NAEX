# NAEX - Exercise 4
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment

#Instalation of any library
library(car)          # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice)      # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)         # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)         # for handling mixed-effects models;
library(pwr)          # power analysis
library(agricolae)    # for Fisher LSD method
library(scatterplot3d)# for 3d scatter plot
library(alr3)
library(FrF2)          #for 2^k  factorial design
library(DoE.base)      # Full factorials, orthogonal arrays and base utilities for DoE packages
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)
library(tidyverse)      # Full factorials, orthogonal arrays and base utilities for DoE packages


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




