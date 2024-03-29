################################################################
# 01NAEX - Exercise 1
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment - Chapter 02
################################################################

######################
# get requirements for Lecture 1
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr","MASS")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

######################
# check our settings
getwd()
print(.libPaths())
print(sessionInfo())
print(version)

# Define directory, if you do not use relative path.
#setwd("M:/01NEX_2020/")

# Read data
Problem20 <- read.table("data/Ex02_20.csv",header=TRUE,sep=";")
Problem26 <- read.table("data/Ex02_26.csv",header=TRUE,sep=";")
Problem32 <- read.table("data/Ex02_30.csv",header=TRUE,sep=";")

# Solve all three problems from lecture slides:

