################################################################
# 01NEX - Exercise 05
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment - Chapter 04
################################################################

######################
# get requirements for Lecture 4
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr","MASS","agricolae","scatterplot3d","FrF2","DoE.base","alr3")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

# Define directory, if you do not use relative path.
#setwd("M:/01NEX_2020/")

# Task: Solve Problems 6.1 and 6.2 from Montgomery DAoE ed.8 - chapter 6
Problem_61 <- read.table("data/Problem_6_1.txt",header=TRUE,sep=";")


