# Causal Inference Lab 2: Conditional Expectation Function
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

library(tidyverse)
library(haven)

cps <- read_dta('https://www.ssc.wisc.edu/~bhansen/econometrics/cps09mar.dta') %>% 
  mutate(log_earn = log(earnings))

# Today, we're going to be thinking about the conditional expectation function (CEF)
# and its relationship to linear regression. 

# The CEF, which is written E[Y_i | X_i], is a function that takes a set of 
# covariates X and estimates a population average outcome Y. For example, when we 
# consider an experiment with a binary treatment, E[Y_i | D_i] is the CEF that
# produces an estimate of the population average as a function of treatment status.
# We can complicate the CEF, of course, by making X have more categories or by 
# including k covariates in X.

# Important to our lab today is the fact that the CEF is equivalent to linear 
# regression under some assumptions. You're going to prove that in your homework, 
# and so, I leave that fun to you. Today, we're going to take that as a given and
# explore the implications of such a proof and consider when the CEF and
# linear regression are/are not equivalent and why we care. This lab should give
# some intuition about what the CEF and linear regression are doing.

# To do that, we'll be using some data from the 2009 current population survey. 
# This is an economics dataset with information about respondents' age, education,
# salary, sex, etc. The data are available courtesy of Bruce Hansen
# (https://www.ssc.wisc.edu/~bhansen/econometrics/). You have already imported it
# if you ran lines 6-10 above.

# Our dependent variable for all analyses will be `log_earn` (logged earnings). 
# Also, we're going to ignore causality today to just focus on the CEF.

# Working in groups, please do the following:

# TODO

# 1. Using the binary variable, female... 
# a) use the CEF to estimate the gender gap: E[earnings | female] - E[earnings | male].

# The CEF maps our data to outcomes. E[earnings | male] is simply the average
# earnings in our data conditioning on being male. And similarly for females.

# male average earnings, E[earnings | male]
male_earn <- cps %>% filter(female == 0) %>% summarise(m = mean(log_earn)) %>% pull(m)
# female average earnings, E[earnings | female]
female_earn <- cps %>% filter(female == 1) %>% summarise(f = mean(log_earn)) %>% pull(f)

male_earn
female_earn

cef_diff <- female_earn - male_earn # E[earnings | female] - E[earnings | male]
cef_diff

# Women earn about -0.29 fewer logged dollars than men on average.

# b) Do the same thing, but in a regression framework. Are these estimates the 
# same? Why or why not?

ols_gender <- lm(log_earn ~ female, cps) # ols of earnings on female
ols_diff <- unname(coef(ols_gender)[2]) # coefficient on female
ols_diff

# The coefficent is the same as our difference above
round(cef_diff,4) == round(ols_diff,4)
# This makes sense because the CEF estimates the average for each gender and we
# compute the difference.  The intercept of the regression is the 
# average logged earnings among men, the coefficient tells us what the average is
# among women once we add it to the intercept. Thus, the coefficient on female 
# is the average difference between men and women. A one unit-change in female
# (going from male to female) produces a -0.29 change in Y, logged earnings.

# c) Create one plot of your results. It should include i) the raw data, ii) the 
# individual estimates of E[earnings | male] and E[earnings | female], ii) a line 
# connecting these points, and iii) the best-fit regression line. Your plot should 
# look like this (https://www.timlrx.com/blog/notes-on-regression-approximation-of-the-conditional-expectation-function)
# and also include the best fit regression line. What do you see?

# To make our plot, we need to create a small df with our CEF estimates from 
# part a) 
cef_gender_est <- tibble(female = c(0,1), log_earn = c(male_earn, female_earn))

# This plot shows: 
# 1) the distribution of male and female earnings
# 2) The CEF estimates of male and female earnings (red points)
# 3) A line connecting the two CEF estimates (dashed red)
# 4) The best-fit regression line (solid blue)

# The best fit regression line and the line connecting our two CEF estimates
# are the same line. This makes sense because we only have two points, so the line
# connecting them should be the same. 

ggplot(cps, aes(x = female, y = log_earn)) + 
  geom_point() + 
  geom_jitter(width = .01, height = .01) +
  geom_smooth(method = 'lm') +
  labs(x = 'Female', y = 'Logged Earnings') + 
  geom_point(data = cef_gender_est, aes(x = female, y = log_earn), 
             color = 'red', size = 2) +
  geom_line(data = cef_gender_est, aes(x = female, y = log_earn),
            color = 'red', linetype = 'dashed')

# 2. Using the continuous variable, hours...
# a) Use the conditional expectation function to estimate the expected logged earnings
# for bins of approximately size 10. That is, e.g., E[earnings | hours \in [50,60)]. 
# Given the distribution of our data, create a bin for [min(.),50), then [50,60) 
# .... [80,90), [90,max(.)). Use the bin's midpoint as your X = x value in the CEF.

# Here, our CEF will be a little more complicated than in problem 1. Rather than just
# two bins of data (male, female), we're going to estimate expected earnings
# using several bins of data.

binned_hr_dat <- cps %>% # full data with new variable, bin_hr for the bins
  mutate(bin_hr = case_when(
    hours >= min(cps$hours) & hours < 50 ~ as.numeric(median(min(cps$hours):50)),
    hours >= 50 & hours < 60 ~ as.numeric(55),
    hours >= 60 & hours < 70 ~ as.numeric(65),
    hours >= 70 & hours < 80 ~ as.numeric(75),
    hours >= 80 & hours < 90 ~ as.numeric(85),
    hours >= 90 & hours <= max(cps$hours) ~ median(90:max(cps$hours))
    )) 

hour_cef <- binned_hr_dat %>% # E[earnings | hours \in bin ] 
  group_by(bin_hr) %>% 
  summarise(cef_hr = mean(log_earn))

hour_cef

# Interestingly, the relationship between hours and earnings appears to be quadratic
# rather than linear. People who work many or few hours seem to earn less than those
# who work middling hours. You might already suspect that linear regression will
# not capture this relationship in the data well.

# b) Use linear regression to estimate this same quantity using the full continuous
# variable from the original data set. What does the coefficient on hours represent?

lm(log_earn ~ hours, cps)

# The coefficient on hours represents the average increase in log earnings from
# working one more hour. 

# d) As in 1c), create one plot that includes i) the raw data, ii) the binned point
# estimates, iii) the line connecting those estimates, and iv) the best fitting 
# regression line. Describe what you're seeing.

# The CEF estimates are red points, connected by a red line. The best-fit regression
# line is in blue. 

ggplot(cps, aes(x = hours, y = log_earn)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  geom_jitter(width = .3, height = .01) +
  labs(x =  'Hours Worked', y = 'Logged Earnings') +
  geom_point(data = hour_cef, aes(x = bin_hr, y = cef_hr), color = 'red', size = 2) +
  geom_line(data = hour_cef, aes(x = bin_hr, y = cef_hr), color = 'red')

# The CEF is non-parametric, but IF the CEF is linear, it is equivalent to least 
# squares (Linear CEF theorem). However, the relationship between hours and earnings 
# is quadratic. As such, linear regression fails to capture this relationship 
# whereas the CEF better captures the non-linear structure. Nonetheless, our OLS 
# estimates can still be useful because they do approximate the CEF. See Angrist
# and Pischke (2009).

# d) Can you think of a plausible explanation for the non-linear relationship 
# identified in  part d? If so, can you add a variable to your data that makes 
# the relationship more linear after controlling for it? Recreate your plot 
# from part d) with this new variable.

# Perhaps people who are working 60+ hours but are earning less than the median 
# wage are working two, full-time jobs (and thus, no overtime) whereas those 
# working 60+ hours and earning more than the median wage are working one, 
# high-paying job (e.g., CEO) or earning overtime. 

# Let's suppose we went back and re-interviewed each person in our data and found 
# out whether they were working two full-time jobs. As it turns out, our hypothesis 
# was true. We add an indicator variable to the data set where 1 indicates that 
# someone works 60+ hours and earns less than the median wage and 0 otherwise.

cps <- cps %>% 
  mutate(two_jobs = if_else(hours > 60 & log_earn < median(cps$log_earn), 1, 0))

# We can also join the binned hour data and re-estimate the CEF, this time by
# adding two covariates, hours and two_jobs.
cef_twojobs <- left_join(cps, binned_hr_dat) %>% 
  group_by(bin_hr, two_jobs) %>% 
  summarise(cef_earn = mean(log_earn))

# Now, we can create the same plot as in part d, adjusting for the type of job 
# situation one has by interacting hours * two_jobs. Now see the relationship 
# between hours is more linear among these groups, but not totally linear.

# We also include our new CEF estimates in green and the original CEF estimate from
# the plot in c) in blue. We can see that the new regression better approximates 
# these CEFs: E[earnings | hours, two_jobs].

ggplot(cps, aes(x = hours, y = log_earn, color = as.factor(two_jobs))) + 
  geom_point() +
  geom_jitter(width = .3, height = .01) +
  geom_smooth(aes(group = as.factor(two_jobs)), method = 'lm', color = c('black')) +
  labs(x = 'Hours Worked', y = 'Logged Earnings', color = 'Two Jobs') +
  geom_point(data = cef_twojobs, aes(x = bin_hr, y = cef_earn, group = two_jobs), 
             color = 'dark green', size = 2) +
  geom_line(data = cef_twojobs, aes(x = bin_hr, y = cef_earn, group = two_jobs),
            color = 'dark green') +
  geom_point(data = hour_cef, aes(x = bin_hr, y = cef_hr), color = 'blue', size = 2) +
  geom_line(data = hour_cef, aes(x = bin_hr, y = cef_hr), color = 'blue')

# Some key takeaways:
# - The CEF gives us the expected value of Y conditional on values of X.
# - Angrist and Pischke (2009) argues that the CEF is what we actually care about. 
# - The CEF is easy to compute with one covariate and binary data, but what if 
#   we have continuous data, lots of covariates? OLS is easier, and if the CEF is
#   linear, OLS reproduces it. But may not give us the exact same results when 
#   the underlying relationship is non-linear.
# - OLS gives a good approximation of the CEF most of the time. It's the best 
#   linear predictor of a non-linear CEF (Angrist and Pischke 2009).



