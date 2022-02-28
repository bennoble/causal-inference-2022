# Causal Inference Lab 3: Power Analysis
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

library(tidyverse)

# Statistical power is the probability we reject H_0 when it is false, that is
# Pr(Reject H_0 | H_0 = F). Just as we conventionally define statistical 
# significance as a = 0.05, we conventionally define power as 1 - B = 0.8. That 
# is, we want to be able to detect the true effect at least 80% of the time. 

# Power is an important concept generally because we want to learn something. If 
# we fail to reject the false null, maybe that's not as bad as rejecting a true null, 
# but it's still not great! So we want to consider whether we are sufficiently powered
# to detect true effects generally. However, power comes up more often in experimental 
# design because you need money, and funders want to make sure you are sufficiently
# powered to detect a true effect. Even if you're not planning to do experimental 
# work, you might actually end up doing it (it happened to me!) because experiments 
# are a good way to add a dose of causality to an observational argument and it's 
# a popular paper strategy. And even if you're not planning to do experiments, you
# may still get questions about whether you're sufficiently powered using other 
# research designs. All of this is to say that power is a thing worth thinking about. 

# There are various formulas for calculating power, but in practice, it is often 
# easier to do power analysis via simulation or using R packages. On your homework,
# you're going to have some practice with statistical power via simulation. This 
# lab is designed to supplement those exercises by showing you how I apply those 
# concepts in my actual research. 

# As an introduction/review, let's do a quick simulation. Suppose we have 1000 
# people in our sample. We assign 500 to treatment (D = 1) and administer some
# treatment with a true effect of Y = .1 with sd = 0.5. 

set.seed(2138)
p_ex <- tibble(D = c(rep(1, 500), rep(0, 500)), 
               Y = c(rnorm(500, .1, .5), rnorm(500, 0, .5)))

# We run a regression to estimate the effect of Y ~ D, and we find (as expected)
# that D^ = 0.1 with p ~= 0.0001. So we have identified the true effect and it is 
# statistically significant. We reject the null and publish our paper in APSR.

summary(lm(Y ~ D, p_ex))

# But should we expect to reject the null often with this setup, or was this a lucky
# fluke? Let's replicate our experiment 1000 times and calculate the number of 
# p-values < 0.05.

set.seed(2139)
d1_pv <- vector()
for(i in 1:1000){
  p_ex1 <- tibble(D = c(rep(1, 500), rep(0, 500)), 
                  Y = c(rnorm(500, .1, .5), rnorm(500, 0, .5)))
  ols_res1 <- summary(lm(Y ~ D, p_ex1)) 
  d1_pv <- c(d1_pv, ols_res1$coefficients[2,4]) # extract the p-value
}

# How many p-values are above 0.05? 866/1000. This is pretty good. We are above 
# the conventional 0.8 level. We are sufficiently powered to detect the true effect.
table(d1_pv < 0.05) 

# Now, suppose the effect size was smaller, just 0.05 holding the sd fixed. 

set.seed(2139)
d2_pv <- vector()
for(i in 1:1000){
  p_ex2 <- tibble(D = c(rep(1, 500), rep(0, 500)), 
                 Y = c(rnorm(500, .05, .5), rnorm(500, 0, .5)))
  ols_res2 <- summary(lm(Y ~ D, p_ex2)) 
  d2_pv <- c(d2_pv, ols_res2$coefficients[2,4]) # extract the p-value
}

# How many p-values are above 0.05? 329/1000. This is not great! Our current design
# is not sufficiently powered to reject the false null. No one would fund our 
# experiment with this kind of power analysis (and we wouldn't want to run it).
# It would probably be a waste of time and money. We basically fail to identify the
# true effect 2/3 of the time.
table(d2_pv < 0.05) 

# What can we do? As we saw above, a larger effect size will increase our power. 
# Alternatively, a larger sample size will also increase our power. What if instead
# of 500 per condition, we had 2000 per condition?

set.seed(2139)
d3_pv <- vector()
for(i in 1:1000){
  p_ex3 <- tibble(D = c(rep(1, 2000), rep(0, 2000)), 
                 Y = c(rnorm(2000, .05, .5), rnorm(2000, 0, .5)))
  ols_res3 <- summary(lm(Y ~ D, p_ex3)) 
  d3_pv <- c(d3_pv, ols_res3$coefficients[2,4]) # extract the p-value
}

# Now our power analysis shows that we reject the false null 881/1000, which is 
# above 1 - B = 0.8. So this is better. We "just" need to 4x our sample size...
table(d3_pv < 0.05) 

# In practice, increasing your sample size is not always feasible! It is expensive 
# to recruit people. Just for reference, if you paid MTurkers federal minimum wage 
# ($7.25) to take a 10m survey, you would pay each one approximately $1.20...so 
# a total increase in your budget of ~$3600 to recruit 3000 more people to detect
# our effect of 0.05 (and that isn't even counting Amazon's cut which is 40%). 
# And note, this is a pretty simple design! It can get even trickier when you're 
# doing interactions, subgroup effects, a more complicated design, etc.

# Now, the "trick" with power analysis is this: if we knew the effect size and
# variation in the effect, we wouldn't need to do the experiment in the first
# place! So this is all made up! *face palm*. Even so, it can still be useful for 
# thinking through how large a sample/effect size you'd need to detect a true effect.
# This can shape whether you think the experiment is worth running, if there are
# other designs that might be able to help, etc. You can use power analysis to test
# your assumptions and consider what would need to go right/wrong for your experiment
# to succeed. It is worth thinking about and doing so you don't run an experiment
# with 1000 people hoping to detect an effect N(0.05, 0.5).

# Your task: we're going to do a power analysis for an "encouragement design." 
# We will learn about instrumental variables later, but here is the basic
# idea: we are going to recruit 1000 respondents and randomly encourage 500 of 
# them to hang up an American flag outside their house. Then, a week later,
# we're going to ask them how patriotic they feel on a 5 point scale where 1 is 
# not patriotic at all and 5 is extremely patriotic. The complication is that we 
# can't FORCE them to hang up the flag or observe them doing it. We just ask them 
# to do it. This introduces some complications we'll deal with later.

# Let's suppose we think (from reading the literature and our assumptions) that 
# people who hang a flag outside their house will feel 0.5 points more patriotic 
# on average. 

# a) Finish creating this population df by assigning treatment, Z (which is the 
# encouragement to hang the flag—not the actual act of hanging the flag). Let's 
# assume everyone complies (that is, those we asked to hang the flag do, and those 
# who we didn't encourage don't). So Z = D. Now, assign treatment effects Y 
# which are normally distributed with mean 0.2 and sd 1. The control group will 
# have an effect of mean 0 and sd 1 (allowing some of them to feel randomly more 
# or less patriotic) due to other events. 

population <- tibble(Z = c(rep(1, 500), rep(0, 500)))

# add the effect
set.seed(2134)
population <- population %>% mutate(Y = c(rnorm(500, .2, 1), rnorm(500, 0, 1)))

# b) Run a regression estimating Y ~ Z. What do you observe? 

# FYI: This is called an intent-to-treat effect. Even if we have no information 
# about whether these people actually hung up flags, we can still conduct this 
# analysis defining treatment as the encouragement. In the future, we'll also
# learn about local average treatment effects among the compliers.

# We recover the true treatment effect and it is statistically significant.
summary(lm(Y ~ Z, population))

# c) Do a power simulation as above. Replicate this analysis 1000 times. What is 
# your simulated power?

set.seed(2139)
pv <- vector()
for(i in 1:1000){
  pop <- tibble(Z = c(rep(1, 500), rep(0, 500)), 
                Y = c(rnorm(500, .2, 1), rnorm(500, 0, 1)))
  ols <- summary(lm(Y ~ Z, pop)) 
  pv <- c(pv, ols$coefficients[2,4]) # extract the p-value
}

# We basically have 1 - B = 0.87. So this is a good design. 
table(pv < 0.05)

# d) Now, let's suppose instead that, no matter how many people we recruit, 20% of
# the population just isn't going to put up a flag—they don't have one, they don't
# want to buy one, they don't want to, they forget etc. We just don't know who. 
# Create a new outcome variable, Y2, that is this new treatment effect: that is, 
# 400 treated units get the usual treatment effect, but 100 of our TREATED units 
# get the control effect because they didn't hang up the flag.

set.seed(2139)
population <- population %>% mutate(Y2 = c(rnorm(400, .2, 1), rnorm(600, 0, 1)))

# With this new outcome, estimate the treatment effect using OLS and conduct a 
# simulated power analysis. What do you observe? 

# Now it appears as though there's no effect...
summary(lm(Y2 ~ Z, population))

set.seed(2139)
pv_d <- vector()
for(i in 1:1000){
  pop <- tibble(Z = c(rep(1, 500), rep(0, 500)), 
                Y2 = c(rnorm(400, .2, 1), rnorm(600, 0, 1)))
  ols_d <- summary(lm(Y2 ~ Z, pop)) 
  pv_d <- c(pv_d, ols_d$coefficients[2,4]) # extract the p-value
}

# Now our power doesn't look as good, we only have approx. 1 - B = 0.7. So, we can
# detect the true effect, and we got unlucky above, but we are below the conventional
# power level.
table(pv_d < 0.05)


# e) Of course, we're just assuming 20% of the sample doesn't comply. So, let's 
# extend the assumption and make a power curve with 1 - B on the y-axis and the
# number of compliers on the x-axis ranging from 0 compliers to 500 compliers. 
# Do 100 replications of each sample rather than 1000. What do you see?

set.seed(2139)
id <- B <- vector()
for(j in 1:500){
  id <- c(id, j)
  pv <- vector()
  for(i in 1:100){
    pop <- tibble(Z = c(rep(1, 500), rep(0, 500)), 
                  Y2 = c(rnorm(j, .2, 1), rnorm(1000-j, 0, 1)))
    ols <- summary(lm(Y2 ~ Z, pop)) 
    pv <- c(pv, ols$coefficients[2,4]) # extract the p-value
  }
  B <- c(B, sum(table(pv < .05)[2])/100)
}

comply_df <- tibble(n_comply = id, power = B)

# Our power curve slopes upward. When no one complies, unsurprisingly, we don't
# ever detect a true effect. However, as more people comply, we are more likely to 
# detect true effects. With this sample and effect size, we'd need at least 414 
# people to achieve 1 - B = 0.8.

ggplot(comply_df, aes(x = n_comply, y = power)) + 
  geom_line() + 
  geom_smooth() + 
  labs(x = 'Number of Compliers', y = 'Power') + 
  geom_vline(xintercept = comply_df %>% filter(B >= .8) %>% pull(n_comply) %>% min(),
             linetype = 'dashed')

# f) Someone scoops us! They do a similar study of flags on patriotism with a larger 
# sample and discover an effect of approximately 0.3 with an sd of 1. Oh well. Now
# we know. As an exercise, redo your power analysis from d) with the new effect
# size of 0.3. What do you observe? 

set.seed(2139)
pv_f <- vector()
for(i in 1:1000){
  pop <- tibble(Z = c(rep(1, 500), rep(0, 500)), 
                Yf = c(rnorm(400, .3, 1), rnorm(600, 0, 1)))
  ols_f <- summary(lm(Yf ~ Z, pop)) 
  pv_f <- c(pv_f, ols_f$coefficients[2,4]) # extract the p-value
}

# We have great power even assuming 100 people do not comply. We would be 
# comfortable conducting the study if we hadn't been scooped! 
table(pv_f < 0.05)


# Some takeaways:
# - I find this exercise useful because we can think through sample size and effect
#   size issues under more complex designs. From this simulation, you can see that
#   you'd need a large sample size if we assume some level of non-compliance and
#   a certain effect size. Good to know!
# - At the end of the day, it's made up. But think through reasonable assumptions
#   from the literature, your expected funding, etc and make a plan. If you know
#   your maximum ask is e.g., $1,500, then can you actually detect a small-ish 
#   effect? Is it worth it? 




# Some other ways to do power analysis:
# R package 'pwr'. 
# But really only good for basic designs.

library(pwr)
pwr.r.test(n = 1000, r = .1, sig.level = 0.05)

# There is also the R package 'DeclareDesign' (https://declaredesign.org/r/declaredesign/articles/DeclareDesign_101.html)
# which is an entire environment for testing various designs and calculating power. 
# I have not used this, but it seems really good. 

# GPower (https://www.psychologie.hhu.de/arbeitsgruppen/allgemeine-psychologie-und-arbeitspsychologie/gpower)
# is a user-friendly program that does power analysis with various designs/parameters.
# It is more feature-rich than pwr.




