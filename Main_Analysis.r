################
#Here we load the csv and all the relevant libraries
################

options(scipen = 999)  
options(digits = 5)
setwd("C:/Users/omrid/Desktop/TAU/Second year/First_semester/Ecronometrics 101/Final project/WhatHelpsStudentsMore/Data")
library(tidyverse)
library(car)
library(sandwich)
library(lmtest)
library(whitestrap)
df <- read.csv("term_paper_data.csv")


################
#Part 2
################

#First we will split the data into the three groups(without SFSP group):
#SSP:
ssp <- df %>% filter(ssp_signup==1)
#SFP:
sfp <- df %>% filter(sfp_signup==1)
#Control:
control <- df %>% filter(control==1)


#Now we will hold the avg for Female , GPA , age , whether or not mother's tongue is English.


#SSP:
ssp_mean_Female <- mean(ssp$female)
ssp_mean_GPA <- mean(ssp$HS_GPA)
ssp_mean_age <- mean(ssp$age)
ssp_mean_english <- mean(ssp$english)
print(ssp_mean_Female)


#SFP:
sfp_mean_Female <- mean(sfp$female)
sfp_mean_GPA <- mean(sfp$HS_GPA)
sfp_mean_age <- mean(sfp$age)
sfp_mean_english <- mean(sfp$english)


#Control:
control_mean_Female <- mean(control$female)
control_mean_GPA <- mean(control$HS_GPA)
control_mean_age <- mean(control$age)
control_mean_english <- mean(control$english)
print(control_mean_age)




