################################################################
# 01NAEX - Exercise 06
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment 
################################################################

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

# Task: Solve Problems 6.31 from Montgomery DAoE ed.8 - chapter 6
Problem_631 <- read.table("data/Problem_6_31.txt",header=TRUE,sep=";")

m         <- FrF2(2^4, 4, replications = 1, randomize = F,
                  factor.names = c("A", "B","C", "D"))
Weight    <- Problem_631[1:16,"Weight"]
Viscosity <- Problem_631[1:16,"Viscosity"]
m         <- add.response(m,Weight)
m         <- add.response(m,Viscosity)

response.names(m)
print(m)



