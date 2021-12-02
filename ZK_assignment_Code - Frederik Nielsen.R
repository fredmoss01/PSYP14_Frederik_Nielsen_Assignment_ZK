






#######
#######
#######
#######
#######
#######
#######
#######
#######          Part 1
#######
#######
#######
#######
#######
#######
#######
#######
#######













data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")


library(psych)
library(tidyverse)	
library(gridExtra)
library(data.table)
library(car)
library(lmtest)	
library(sandwich)
library(boot)
library(lmboot) 
library(r2glmm) 
library(lme4) 
library(lmerTest) 
library(MuMIn)
library(lm.beta)
library(influence.ME) 
library(lattice) 
library(lme4) 
library(lmerTest) 
#



#
#
#
#
#
#
#
#
#
#

#
#
#
#
#
#
#
View(data_sample_1) #Viewing data

data_sample_1 %>% #overview of the summary of the data
  summary()




model1 = lm(pain ~ age + sex, data = data_sample_1) 
# Creating the first model with predictors age and sex as a dummy variable.



model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)
#Here I create the second model with the other predictor variables. 
#I use cortisol_serum as the variable to represent patients cortisol levels, 
#because it is regarded in medical research as more reliably related to stress.



summary(model2)


#plotting all of the predictor variables against the dependant variable pain

plot1 = data_sample_1 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

plot2 = data_sample_1 %>% 	
  ggplot() +	
  aes(x = sex, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

plot3 = data_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

plot4 = data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

plot5 = data_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

plot6 = data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

?slice


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3)

#By looking at plots I notice that there are some very concerning outliers. 
#This makes me think that I should go and examine the date more careful and find a way to deal with them.
#There seems to be a participant who reported very high level of pain (over the level of 10, 
#which should not be possible since the variable was measured between 1-10).
#There also seems to be a participant who has a STAI_trait score below 20,
#which is also not possible according to measure which states that the lowest score is 20.


View(data_sample_1) #viewing the date to examine which participants have these odd recorded data.

#Participant with ID 34 has a reported STAI_trait at 4.2, 
#The result of the particpant on the STAI_trait may have been wrongly entered into the data as 4.2 instead of 42,
#But there is no way for me to check this, so I will have to recode the participants value on the STAI_trait to NA

#Participant with ID 88 had a reported pain level of 55.
#This result seems to be wrongly entered into the data and the participant may actually responded 5 here.
#There is unfourtunately no way for me to know for sure what has happened here,
#So I decide to report the pain level for person with ID 88 as NA.



#I have decided to recode the values of these wrongly inputted values into the mean score so that I don't complete remove the data.
#I recoded ID 34 STAI_trait level into 40, which was the average score for STAI_trait
#I recoded ID 88 pain level into 5, which was the average score for pain.

data_sample_1$STAI_trait[data_sample_1$STAI_trait== 4.2] <- 40

data_sample_1$pain[data_sample_1$pain== 55] <- 5

#I now have to re-run the model code 

model1 = lm(pain ~ age + sex, data = data_sample_1) 




model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

#Checking for leverage of outliers
#
#

model1 %>% 	
  plot(which = 5)	

model2 %>% 	
  plot(which = 5)	

model1 %>% 
  plot(which = 4)

model2 %>% 
  plot(which =4)




#
#
#
#Checking for normality
#
#
#



model1 %>% 
  plot(which = 2)

model2 %>% 
  plot(which =2)

# histogram	

residuals_model1 = enframe(residuals(model1))	
residuals_model1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

residuals_model2 = enframe(residuals(model2))	
residuals_model2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(model1))	

describe(residuals(model2))

?plot

#
#
#
# Linearity
#
#
#

model2 %>% 	
  residualPlots()	

model1 %>% 	
  residualPlots()	

#
#
#
#
### Homoscedasticty	
#
#
#
#
#


model2 %>% 	
  plot(which = 3)	

model2 %>% 	
  ncvTest() # NCV test	

model2 %>% 	
  bptest() # Breush-Pagan test	


model1 %>% 	
  plot(which = 3)	

model1 %>% 	
  ncvTest() # NCV test	

model1 %>% 	
  bptest() # Breush-Pagan test	




#
#
#
#
#
### Multicollinearity
#
#
#
#
#


model1 %>% 
  vif()


model2 %>% 
  vif()


#
#
#
#
#
### Test Statistics for model 1 and model 2
#
#
#
#
#
#


#Summary
model1 %>% 
  summary()

model2 %>% 
  summary()


 
#Confidence intervals, Standardised coefficients, p-values

sm_table1 = coef_table(model1)	
sm_table1

sm_table2 = coef_table(model2)	
sm_table2




#
#
#
#
#
#
#
########## Model comparison
#
#
#
#
#
#
#
#
#

### 
AIC(model1)	
AIC(model2)	


anova(model1, model2)


#######
#######
#######
#######
#######
#######
#######
#######
#######          Part 2
#######
#######
#######
#######
#######
#######
#######
#######
#######



HS2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")


library(psych) # for describe	
library(tidyverse) # for tidy code	
library(gridExtra)
library(data.table)
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(lm.beta)
#
#
#
#
#
#
#
#
#
#

view(HS2)
HS2 %>% 
 summary()


#
#
#
#
#
#
#
#
#
#
#
#
#
#


# Computing the booststrapped confidence interval for a linear model using wild bottstrapping as descibed by Wu (1986) <doi:10.1214/aos/1176350142>	
# requires the lmboot pakcage	

wild.boot.confint <- function(model, data = NULL, B = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  
  wild_boot_estimates = wild.boot(formula(model), data = data, B = B)	
  
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,probs=c(.025,.975))))	
  
  return(result)	
  
}	




#The following code is what I need to run the backwards regression, compare models and fit the model to new dataset.
#I will also run model diagnostics on the new model with all the predictors


view(data_sample_1)
data_sample_1 %>% 
  summary()

model3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1)


mod3_back = step(model3, direction = "backward")	

mod3_back

model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)
#
#
#
####Model diagnostics
#
#
#


#Outliers

model3 %>% 	
  plot(which = 5)	

model3 %>% 
  plot(which = 4)

mod3_back %>% 	
  plot(which = 5)	

mod3_back %>% 
  plot(which = 4)

#Normality 


model3 %>% 
  plot(which = 2)


mod3_back %>% 
  plot(which = 2)

# histogram	

residuals_model3 = enframe(residuals(model3))	
residuals_model3 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

residuals_mod3_back = enframe(residuals(mod3_back))	
residuals_mod3_back %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	


# skew and kurtosis	
describe(residuals(model3))	

describe(residuals(mod3_back))	




#
#
#
# Linearity
#
#
#

model3 %>% 	
  residualPlots()	

mod3_back %>% 	
  residualPlots()	


#
#
#
#
### Homoscedasticty	
#
#
#
#
#


model3 %>% 	
  plot(which = 3)	

model3 %>% 	
  ncvTest() # NCV test	

model3 %>% 	
  bptest() # Breush-Pagan test	



mod3_back %>% 	
  plot(which = 3)	

mod3_back %>% 	
  ncvTest() # NCV test	

mod3_back %>% 	
  bptest() # Breush-Pagan test	




#
#
#
#
#
### Multicollinearity
#
#
#
#
#


model3 %>% 
  vif()

mod3_back %>% 
  vif()

#I remove Cortisol_saliva from the model because it has the highest VIF value and is probably correlated with the cortisol_serum. 5.1 to be exact

#Check of VIF after remove of saliva

#The model has no multicollinearity issue now after removing cortisol_saliva














#
#
#
#
#



mod3_back %>% 
  summary()


sm_table3 = coef_table(mod3_back)	
sm_table3


anova(mod3_back, model2)	
anova(mod3_back, model3)	

summary(model2)$adj.r.squared
summary(mod3_back)$adj.r.squared	
summary(model3)
summary(mod3_back)
summary(model2)
AIC(model2)	
AIC(mod3_back)	
AIC(model3)

anova(mod3_back, model3)


# calculate predicted values 	
pred_test <- predict(model2, HS2)	
pred_test_back <- predict(mod3_back, HS2)	

# now we calculate the sum of squared residuals 	
RSS_test = sum((HS2[,"pain"] - pred_test)^2)	
RSS_test_back = sum((HS2[,"pain"] - pred_test_back)^2)	
RSS_test	
RSS_test_back	


RSS_diff = 249.68 - 243.78

RSS_diff






#######
#######
#######
#######
#######
#######
#######
#######
#######          Part 3
#######
#######
#######
#######
#######
#######
#######
#######
#######






HS2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")
HS3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
HS4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")


coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	
#
#
#
#
#
#
#

confint.boot <- function(model, data = NULL, R = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)), ncol = 2))	
  row.names(boot.ci_output_table) = names(coef(model))	
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")	
  results.boot = results <- boot(data=data, statistic=bs_to_boot, 	
                                 R=1000, model = model)	
  
  for(i in 1:length(coef(model))){	
    boot.ci_output_table[i,] = unlist(unlist(boot.ci(results.boot, type="bca", index=i))[c("bca4", "bca5")])	
  }	
  
  return(boot.ci_output_table)	
}	

wild.boot.confint <- function(model, data = NULL, B = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  
  wild_boot_estimates = wild.boot(formula(model), data = data, B = B)	
  
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,probs=c(.025,.975))))	
  
  return(result)	
  
}	


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}




view(HS2)
view(HS3)
view(HS4)

summary(HS2)
summary(HS3)
summary(HS4)
#One participant had their sex entered as woman instead of female. I will change this below


filter(HS3, household_income < 0 )
#1 participant had an income under 0

HS3$household_income[HS3$household_income < 0] <- 70034 #70034 is the mean for the household_income before I change it.

HS3$sex[HS3$sex == "woman"] <- "female" #I use this function to change participant #25s coded value for sex from woman to female


?slice

HS3 = HS3 %>% 	
  mutate(hospital = factor(hospital), sex = factor(sex))	#changing hospital into a factor variable

HS4 = HS4 %>% 	
  mutate(hospital = factor(hospital), sex = factor(sex))	#changing hospital into a factor variable

#I have an issue with newlevels when trying to predict HS4 with the random_intercept_model from HS3
#Create equal levels in both datasets
HS3$sex <- ifelse(HS3$sex == "male", 1, 0)  
HS4$sex <- ifelse(HS4$sex == "male", 1, 0) 



#Creating random intercept model
Random_intercept_model = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = HS3)




confint(Random_intercept_model)

stdCoef.merMod(Random_intercept_model)

summary(Random_intercept_model)
Random_intercept_model
# marginal R squared with confidence intervals	
r2beta(Random_intercept_model, method = "nsj", data = HS3)	

# marginal and conditional R squared values	
r.squaredGLMM(Random_intercept_model)	


####
#### Testing the predictablity for the random_intercept_model
####

# calculate predicted values 	
pred_test_2 <- predict(Random_intercept_model, HS4, allow.new.levels = TRUE)


# now we calculate the sum of squared residuals 	
RSS_test_2 = sum((HS4[,"pain"] - pred_test_2)^2)	

RSS_test_2


mod_mean <- lm(pain ~ 1, data = HS3)

TSS = sum((HS4$pain - predict(mod_mean))^2)
TSS
R2 = (1 - (RSS_test_2/TSS))
R2



#### Random intercept and slope model



# random slope model	



mod_int_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = HS3)


isSingular(mod_int_slope)


HS3_1 = HS3 %>% 		
  mutate(pred_slope = predict(mod_int_slope)		) 



slope_plot<-HS3_1 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)		
slope_plot	



















