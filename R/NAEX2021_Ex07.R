################################################################
# 01NAEX - Exercise 07
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

###############################
######### Exercise ###########
##############################

Problem_06_26    <- read.table("data/Ex06_26.csv",header=TRUE, sep = ";")
Ex26             <-  FrF2(2^5, 5, replications = 1, randomize = FALSE,
                          factor.names = c("A", "C", "D","E","F"),repeat.only=TRUE,ncenter=4)
Yield            <-  Problem_06_26$Yield
Ex26             <-  add.response(Ex26, Yield)
summary(Ex26)



