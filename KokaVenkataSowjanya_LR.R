# Koka Venkata Sowjanya
#=========================================================================

# Question 1 : Read the dataset straight from the OpenIntro webpage

###Answer:
## Reading the dataset from webpage
NC_Births <- read.table("https://www.openintro.org/data/csv/ncbirths.csv",sep = ",",header = TRUE)

## Displaying the first three records from the dataset
head(NC_Births,3)
#NC_Births[1:3,]

# Question 2 : Create a new variable, netgain = gained − weight, call it netgain and make it part of the dataset.
###Answer:

NC_Births_New <- NC_Births # Creating a copy of original dataset
head(NC_Births_New)
# Creating a new variable netgain
NC_Births_New$netgain <- (NC_Births_New$gained - NC_Births_New$weight)
head(NC_Births_New)

# Checking if the new variable is attached to the dataset
names(NC_Births_New)

# Question 3 : Estimate the following model and display the summary results
#weight = b0 + b1weeks + b2mage + b3netgain

###Answer:

# Creating the model
m1 <- lm(formula = weight ~ weeks+mage+netgain, data = NC_Births_New)

# Displaying the summary results of the model
summary(m1)

# Question 4 : What is the value and the meaning of the slope of “weeks”? Explain the implication of the
#value of the coefficient, and the implication (not the meaning) of the p-value. Three sentences suffice.

###Answer:
# The value of slope coefficient of weeks is 0.342249 which means at 0.05 level of significance, the evidence suggests that the slope of weeks is different from zero.
# For each unit increase in length of pregnancy of mother, we would expect the weight of a baby at birth would increase 0.342249 times of number of weeks(pounds)
# The p-value is(2e-16) which is very close to 0 and less than level of significance(0.05) which indicates, we have enough evidence to 
# reject the null hypothesis(there is no relationship between weight of the baby at birth in pounds and length of pregnancy of mother in weeks)

# Question 5 : What is the meaning of the p-value for the slope coefficient of “mage”? One sentence
# suffices.

###Answer:
# The P-value of slope coefficient of “mage” is small(0.000617) meaning the random variation because of the sampling process alone is not likely to account for the observed difference and
# the t-value = 3.435 indicates the evidence is 3.435 standard deviations away (far) from the null hypothesis

# Question 6 :  Assess the goodness of fit of this model to the data? Avoid flowery and meaningless qualitative
#assessment (e.g., “quite well”, “not too well”). Give me a quantitative/objective assessment based on
#the appropriate statistic(s). What do the values mean? Three sentences are sufficient.

###Answer:
#The Multiple R-squared value is  0.4473 and residual standard error is 1.111 ,
#The R-squared value means that the model explains about 45% of the variability in weight and 
#The residual standard error(RMSE) indicates that the typical deviation in a prediction of weight is about 1.111 pounds 

# Question 7 : Briefly, analyze the residuals and assess the Gauss-Markov Theorem (i.e., mean residualzero? heteroscedasticity?, serial correlation?) and the Normality of residuals. 
#I expect you to show the results of the appropriate inferential methods and to make the assessments based on those
#results. Four sentences should suffice.

###Answer:

#The residuals of the model are
resid_values <- residuals(m1)
#Displaying the residual values
head(resid_values)

# checking the mean of residuals
#• mean residual
mean(resid_values)

###Answer:
#The mean residual value is very close to zero by which we can say that the,
#the errors have an expectation of zero
library(lmtest)

#• heteroscedasticity
#The errors have equal variances
#Ho : Homoscedastic
#Ha : Heteroscedastic
bptest(formula = m1)
###Answer:(Homoscedastic)
# The p-value we have from bptest is 0.1812 which is higher than the level of significance(0.05),
# indicating we do not have enough evidence to reject the null hypothesis(Homoscedastic) 
# we did not find evidence for Heteroscedastic(unequal variances in errors) so our estimates for the slopes 
# cannot be effected by the model 


#•serial correlation
#The residuals must not be correlated among themselves
#Ho : No first order correlation
#Ha : First order correlation
dwtest(formula = m1,alternative = "two.sided")
###Answer:(No first order correlation)
# The p-value we have from dwtest is 0.9324 which is higher than the level of significance(0.05),
# indicating we do not have enough evidence to reject the null hypothesis(No first order correlation), 
# we did not find evidence for first order autocorrelation (the error of consecutive observations are not correlated) in our model


#• normality of residuals
#Testing for normality
#Ho : Gaussian distributed (normal)
#Ha : Not Gaussian distributed (not normal)

hist(resid_values, col = "lightblue") 
# kind of symmetric

library(car)

qqPlot(x=resid_values) 
#There are slight departures

shapiro.test(resid_values)
###Answer:(Gaussian distributed)
# The p-value we have from shapiro test is 0.1713 which is higher than the level of significance(0.05),
# indicating we do not have enough evidence to reject the null hypothesis(Gaussian distributed (normal)) 
# we did not find evidence for Not Gaussian distributed(not normally distributed) in our model and
# proceed with the distribution of residuals is Gaussian

