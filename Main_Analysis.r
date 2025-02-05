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


#Question 1


#First we will split the data into the three groups(without SFSP group):
#SSP:
ssp <- df %>% filter(ssp_offer==1)
#SFP:
sfp <- df %>% filter(sfp_offer==1)
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

#All_together:
all_mean_Female <- mean(df$female)
all_mean_GPA <- mean(df$HS_GPA)
all_mean_age <- mean(df$age)
all_mean_english <- mean(df$english)


#Question  3 :


ssp_and_control <- df %>% filter(ssp_offer==1 | control==1)
#Altough we know that in LPM model the Homoskedasticity assumption doesn't accure, we ran white and BP test just to make sure. 
background_effects_model <- lm(ssp_offer ~ female + HS_GPA + age + english , data = ssp_and_control)
summary(background_effects_model)

#We want to check Heteroskedasticity:
#Breusch-Pagan test: 
ssp_and_control$u_hat<-residuals(background_effects_model)
ssp_and_control$u_hat_sq<-(ssp_and_control$u_hat)^2
bp_model <- lm(u_hat_sq~ female + HS_GPA + age + english , data = ssp_and_control )
summary(bp_model)
#White Test
white_test(background_effects_model)
#We found that there is  heteroskedasticity (E[u|x] != u_hat)
#Now we need to fix this using white correction.
coeftest(background_effects_model , vcov = vcovHC(background_effects_model,"HC1"))

#Now we will check for each individual variabl, and do white correction for them.
age_effects_model <- lm(ssp_offer ~  age, data = ssp_and_control)
coeftest(age_effects_model , vcov = vcovHC(age_effects_model,"HC1"))

HS_GPA_effects_model <- lm(ssp_offer ~  HS_GPA, data = ssp_and_control)
coeftest(HS_GPA_effects_model , vcov = vcovHC(HS_GPA_effects_model,"HC1"))

female_effects_model <- lm(ssp_offer ~  female, data = ssp_and_control)
coeftest(female_effects_model , vcov = vcovHC(female_effects_model,"HC1"))

english_effects_model <- lm(ssp_offer ~  english, data = ssp_and_control)
coeftest(english_effects_model , vcov = vcovHC(english_effects_model,"HC1"))




################
#Part 3
################

#Question 4

SFP_vs_SSP_first_sem_model <- lm(first_sem_grade ~ ssp_offer + sfp_offer , data =df )
summary(SFP_vs_SSP_first_sem_model)


#Question 5

#To check which variuables affect the most on the grade, we created a big linear model that takes into account all the possible vars
all_affects_on_grade_model <- lm (first_sem_grade ~ ssp_offer + sfp_offer+ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad + mom_HS_grad + mom_college_grad + 
                                    uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans + last_min , data = df )
summary(all_affects_on_grade_model)



#Now we will take only those that we found they are distinct statistically to affect the grades.

distinct_affects_on_grade_model <- lm(first_sem_grade~ssp_offer + sfp_offer+ HS_GPA + age + female + english + finish_in_4_yrs , data = df  )
summary(distinct_affects_on_grade_model)

#Now we will check for Homoskedasticity using white test
white_test(distinct_affects_on_grade_model)
#We found that P-value is lower then 0.1 so we will do the white correction
coeftest(distinct_affects_on_grade_model , vcov = vcovHC(distinct_affects_on_grade_model,"HC1"))





