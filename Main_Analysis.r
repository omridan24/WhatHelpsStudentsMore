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

#Now we will check for each individual variable, and do white correction for them.
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
#White Test
white_test(SFP_vs_SSP_first_sem_model)

#Question 5

#To check which variables affect the most on the grade, we created a big linear model that takes into account all the possible vars
all_affects_on_grade_model <- lm (first_sem_grade ~ ssp_offer + sfp_offer+ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad + mom_HS_grad + mom_college_grad + 
                                    uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans + last_min , data = df )
summary(all_affects_on_grade_model)



#Now we will take only those that we found they are distinct statistically to affect the grades.

distinct_affects_on_grade_model <- lm(first_sem_grade~ssp_offer + sfp_offer+ HS_GPA + age + female + english + finish_in_4_yrs , data = df  )
summary(distinct_affects_on_grade_model)

#Now we will check for Homoskedasticity using white test
white_test(distinct_affects_on_grade_model)
#We found that P-value is lower then 0.1 so we will do the white correction
fixed_distinct_affects_on_grade_model <- vcovHC(distinct_affects_on_grade_model,"HC1")



#Question 6

linearHypothesis(distinct_affects_on_grade_model, "sfp_offer = ssp_offer",vcov=fixed_distinct_affects_on_grade_model)

critical_f_value <- qf(0.9 , 1 ,998) 



#Question 7 

#Assuming that the same 5 variables are the distinct ones even after one year and two years:
#We will run the same model as we ran for one semester, for the end of first year and end of second year.
#BUT! we have a difference between the grades in first semester(in range 0-100) and the grades in first year in second year(in range 0-4):
#So We create another column with 25 * grade to get 0-100 range.
df$year_1_GPA_times_25 <- 25*df$GPA_year1
distinct_affects_on_grade_first_year_model <- lm(year_1_GPA_times_25~ssp_offer + sfp_offer+ HS_GPA + age + female + english + finish_in_4_yrs , data = df  )
summary(distinct_affects_on_grade_first_year_model)
#We will check using white test for Homoskedasticity:
white_test(distinct_affects_on_grade_first_year_model)
#we found that there is Homoskedasticity
summary(distinct_affects_on_grade_first_year_model)

#Now we will do the same for second year
df$year_2_GPA_times_25 <- 25*df$GPA_year2
distinct_affects_on_grade_second_year_model <- lm(year_2_GPA_times_25~ssp_offer + sfp_offer+ HS_GPA + age + female + english + finish_in_4_yrs , data = df  )

white_test(distinct_affects_on_grade_second_year_model)
#We found that there is heteroskedasticity so we will do the white correction
coeftest(distinct_affects_on_grade_second_year_model , vcov = vcovHC(distinct_affects_on_grade_second_year_model,"HC1"))



#Question 8
#We will first check median of the HS_GPA of students in the sfp + control groups.
sfp_and_control <- df %>% filter(sfp_offer==1 | control==1)

median_HS_GPA <- median(sfp_and_control$HS_GPA, na.rm = TRUE)


sfp_and_control <- sfp_and_control %>% 
  mutate(above_median = ifelse(HS_GPA > median_HS_GPA, 1, 0))

#Now we will create the linear model that will show the affect of SFP on the grades according to the 2 groups:
#1) above the median in High school
#2) below the median in High school

above_and_below_median_model <- lm(first_sem_grade ~ sfp_offer * above_median, data = sfp_and_control)
summary(above_and_below_median_model)

white_test(above_and_below_median_model)
#We found that there is no need for white correction.
#Now we will check out hypotesys that B3 is 0 (above*SFP)
linearHypothesis(above_and_below_median_model, "sfp_offer:above_median = 0")



################
#Part 4
################


#Question 9

#We will create a new df that holds all of the students that were offered with SFP.
only_sfp_offered <- df %>% filter(sfp_offer==1)

#Now we will calc the means of the background varuables for each group
#Group 1 - signed up for SFP
#Group 2 - Didn't sign to SFP
group_1 <- only_sfp_offered %>% filter(sfp_signup==1)
female_mean_signup<-mean(group_1$female)
GPA_mean_signup<-mean(group_1$HS_GPA)
english_mean_signup<-mean(group_1$english)
age_mean_signup<-mean(group_1$age)

group_2 <- only_sfp_offered %>% filter(sfp_signup==0)
female_mean_not_signup<-mean(group_2$female)
GPA_mean_not_signup<-mean(group_2$HS_GPA)
english_mean_not_signup<-mean(group_2$english)
age_mean_not_signup<-mean(group_2$age)


#Question 9
#First we will take the SFP and control group to look at.

sfp_and_control <- df %>% filter(sfp_offer==1 | control==1)


#Question 10:
#Now we will run the model that was requested.
grade_for_sfp_signup_model <- lm (first_sem_grade ~ sfp_signup ,  data = sfp_and_control)
summary(grade_for_sfp_signup_model)


#Question 11:
covariance <- cov(sfp_and_control$sfp_signup, sfp_and_control$sfp_offer)


sfp_and_control <- df %>% filter(sfp_offer==1 | control==1)
#Altough we know that in LPM model the Homoskedasticity assumption doesn't accure, we ran white and BP test just to make sure. 
background_effects_model <- lm(sfp_offer ~ female + HS_GPA + age + english , data = sfp_and_control)
summary(background_effects_model)

#We want to check Heteroskedasticity:
#Breusch-Pagan test: 
sfp_and_control$u_hat<-residuals(background_effects_model)
sfp_and_control$u_hat_sq<-(sfp_and_control$u_hat)^2
bp_model <- lm(u_hat_sq~ female + HS_GPA + age + english , data = sfp_and_control )
summary(bp_model)
#White Test
white_test(background_effects_model)
#We found that there is  heteroskedasticity (E[u|x] != u_hat)
#Now we need to fix this using white correction.
coeftest(background_effects_model , vcov = vcovHC(background_effects_model,"HC1"))

#Now we will check for each individual variable, and do white correction for them.
age_effects_model <- lm(sfp_offer ~  age, data = sfp_and_control)
coeftest(age_effects_model , vcov = vcovHC(age_effects_model,"HC1"))

HS_GPA_effects_model <- lm(sfp_offer ~  HS_GPA, data = sfp_and_control)
coeftest(HS_GPA_effects_model , vcov = vcovHC(HS_GPA_effects_model,"HC1"))

female_effects_model <- lm(sfp_offer ~  female, data = sfp_and_control)
coeftest(female_effects_model , vcov = vcovHC(female_effects_model,"HC1"))

english_effects_model <- lm(sfp_offer ~  english, data = sfp_and_control)
coeftest(english_effects_model , vcov = vcovHC(english_effects_model,"HC1"))




#Now we will do the 2sls test:
two_sls_first_step <- lm(sfp_signup~sfp_offer , data=sfp_and_control)
summary(two_sls_first_step)

sfp_and_control$sfp_signup_hat <- fitted.values(two_sls_first_step)

two_sls_second_step <- lm(first_sem_grade~sfp_signup_hat , data = sfp_and_control)

white_test(two_sls_second_step)
#White test did not find heteroskedasticity so we won't fix this.

summary(two_sls_second_step)

#Now we will do this using the IV variable 

offer_to_grade <- cov(sfp_and_control$sfp_offer , sfp_and_control$first_sem_grade)
offer_to_signup <- cov(sfp_and_control$sfp_offer , sfp_and_control$sfp_signup)
beta_IV <- offer_to_grade / offer_to_signup





