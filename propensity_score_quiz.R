#install.packages("tableone")
#install.packages("Matching")
#install.packages("MatchIt")

library(tableone)
library(Matching)
library(MatchIt)
library(tidyverse)

#Now load the lalonde data (which is in the MatchIt package):
data(lalonde)
summary(lalonde)


# The data have n=614 subjects and 10 variables
# 
# age age in years.
# educ years of schooling.
# black indicator variable for blacks.
# hispan indicator variable for Hispanics.
# married indicator variable for marital status.
# nodegree indicator variable for high school diploma.
# re74 real earnings in 1974.
# re75 real earnings in 1975.
# re78 real earnings in 1978.
# treat an indicator variable for treatment status.
# The outcome is re78 – post-intervention income.
# The treatment is treat – which is equal to 1 if the subject received the labor training and equal to 0 otherwise.

# The potential confounding variables are: age, educ, black, hispan, married, nodegree, re74, re75.



# 1. Find the standardized differences for all of the confounding variables 
# (pre-matching). What is the standardized difference for married (to nearest hundredth)?

confounders <- c('age', 'educ', 'black', 'hispan', 'married', 'nodegree',
                 're74', 're75')

# try overall (not stratified)
t1 <- CreateTableOne(vars = confounders, data=lalonde, smd=T)
print(t1, smd=T)

# use the strata parameter to split by treatment group- THIS ONE!
t1b <- CreateTableOne(vars = confounders, strata='treat', data=lalonde, test = F)
print(t1b,smd=T)
  


# 2. What is the raw (unadjusted) mean of real earnings in 1978 for treated 
# subjects minus the mean of real earnings in 1978 for untreated subjects?

re78_treat1 <- lalonde[lalonde$treat==1, 're78']
re78_treat0 <- lalonde[lalonde$treat==0, 're78']
mean(re78_treat1) - mean(re78_treat0)

#verify visually
ggplot(lalonde, aes(x=re78, fill=treat)) + geom_histogram(bins=50) +
  facet_grid(~treat)



# INSTRUCTIONS
# Fit a propensity score model. Use a logistic regression model, where the 
# outcome is treatment. Include the 8 confounding variables in the model as 
# predictors, with no interaction terms or non-linear terms (such as squared terms). 
# Obtain the propensity score for each subject.

prop <- glm(data=lalonde, 
            treat~age+educ+black+hispan+married+nodegree+re74+re75,
            family='binomial')

prop_score <- prop$fitted.values


#3. What are the minimum and maximum values of the estimated propensity score?
min(prop_score)
max(prop_score)


# Now carry out propensity score matching using the Match function.
#Before using the Match function, first do:
set.seed(931139)

#Setting the seed will ensure that you end up with a matched data set that is the
#same as the one used to create the solutions.
#Use options to specify pair matching, without replacement, no caliper.
#Match on the propensity score itself, not logit of the propensity score. 
#Obtain the standardized differences for the matched data.


match <- Match(Tr=lalonde$treat, M=1, X=prop_score, replace = F)
summary(match)
str(match)


#4. What is the standardized difference for married?

# get indices of the matched records
matched_treated <- match$index.treated
matched_control <- match$index.control

#subset original data
lalonde_match <- lalonde[c(matched_treated, matched_control),]

# create the table1 again, only using matched records
t1_matched <- CreateTableOne(vars=confounders, strat='treat', data=lalonde_match, test=F )
print(t1_matched, smd=T)
  
# INSTRUCTIONS
# Re-do the matching, but use a caliper this time. Set the caliper=0.1 
# in the options in the Match function. Again, before running the Match function, set the seed:

set.seed(931139)


match_cp1 <- Match(Tr=lalonde$treat, M=1, X=prop_score, replace = F, caliper = .1)

#6. How many matched pairs are there?
summary(match_cp1)


# 7. Use the matched data set (from propensity score matching with caliper=0.1) 
# to carry out the outcome analysis.
# 
# For the matched data, what is the mean of real earnings in 1978 for treated 
# subjects minus the mean of real earnings in 1978 for untreated subjects?


# get indices of the matched records
matched_treated_cp1 <- match_cp1$index.treated
matched_control_cp1 <- match_cp1$index.control

#subset original data
lalonde_match_cp1 <- lalonde[c(matched_treated_cp1, matched_control_cp1),]


re78_treat1_mcp1 <- lalonde_match_cp1[lalonde_match_cp1$treat==1, 're78']
re78_treat0_mcp1 <- lalonde_match_cp1[lalonde_match_cp1$treat==0, 're78']
mean(re78_treat1_mcp1) - mean(re78_treat0_mcp1)

# get the difference between matched earnings in treatment & control 
diffy <- re78_treat1_mcp1 - re78_treat0_mcp1

#run a t-test to compare groups
t.test(diffy)


  