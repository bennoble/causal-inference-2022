# Causal Inference Lab 4: Subclassification
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

# This exercise is heavily based on one in Scott Cunningham's Causal Inference
# Mixtape (https://mixtape.scunning.com/matching-and-subclassification.html#subclassification-exercise-titanic-mathrmdata-set). 
# However, there are some small differences conceptually and the code below is 
# my own. 

library(tidyverse)
library(haven)

titanic <- read_dta("https://raw.github.com/scunning1975/mixtape/master/titanic.dta")

# Today, we're going to focus on estimating average treatment effects using 
# subclassification like we discussed in class. To do that, we're going to be 
# using data about passengers on the Titanic (which you probably know crashed
# into an iceberg and sunk in 1912). In total, 1514 people lost their lives
# while 710 survived.

# Maybe it will (or won't) surprise you to learn that passengers in first class
# were more likely to survive. There could be a few reasons for that, but one in 
# particular was that first class was higher up on the boat. So those people had 
# more time to escape. If you don't believe me, see for yourself. 

# 1) Compute the naive difference in means of the probability of survival as a 
# function of the treatment (being in first class). Note: you'll need to create
# this treatment variable from the data. 

titanic <- titanic %>% 
  # create first class variable
  mutate(first_class = if_else(class == 1, 1, 0))

# naive difference in means
naive_dim <- titanic %>% 
  group_by(first_class) %>% 
  summarise(means = mean(survived)) %>% 
  pull(means) %>% 
  diff()

# See, being in first class increased your probability of survival by about 35%.
naive_dim

# Ok ok, I hear you. Maybe there are other complicating factors here that we 
# haven't accounted for. Maybe age and sex have something to do with one's 
# propensity to end up in first class. Also, women and children were given first
# priority for life boats. So both of these variables affect treatment and 
# outcomes. Confounders! But let's suppose (a BIG supposition) that once we account
# for those two variables, we have satisfied conditional ignorability and could
# causally estimate the ATE.

# 2a) Stratify the data into 4 groups based on age and sex (i.e., adult males,
# adult females, child males, child females). Present your results as a table that 
# includes the following: age/sex label, first class (treatment) indicator, 
# the probability of survival and the size of the group. What do you notice?

titanic <- titanic %>%
  # create strata
  mutate(age_sex = case_when(
    age == 1 & sex == 1 ~ 'adult male',
    age == 0 & sex == 1 ~ 'child male',
    age == 1 & sex == 0 ~ 'adult female',
    age == 0 & sex == 0 ~ 'child female'
  ))

group_stats <- titanic %>% 
  group_by(age_sex, first_class) %>% 
  summarise(prob_survive = mean(survived), # probability of survival
            group_size = n()) # total number per group

# Some things to note: there are only five child males in first class and only one
# (!) child female in first class. Small cells! Also, there are a lot of adult men
# in not-first class (this is because we are including the crew in our analysis 
# which was overwhelmingly male). Our naive ATE is probably biased.
group_stats

# 2b) Compute i) the difference in survival probabilities within each strata and 
# ii) weights for your ATE by dividing the total number of people in each age/sex 
# cell (including both treated and untreated) and dividing by the total number of 
# people on board.

diff_stats <- group_stats %>% 
  pivot_wider(names_from = 'first_class', values_from = c('prob_survive', 'group_size')) %>% 
  mutate(p_diff = prob_survive_1 - prob_survive_0, size = group_size_0 + group_size_1) 

weights_df <- diff_stats %>% 
  mutate(prop = size/sum(diff_stats$size))

# 2c) Calculate the subclassification ATE of first class status by i) multiplying 
# the weights by the survival probabilities within each strata and ii) summing 
# each strata-specific (or conditional) ATE. Describe the result and how they differ 
# from the naive estimate.

s_ate <- weights_df %>%  
  mutate(s_ate = p_diff*prop) %>% 
  pull(s_ate) %>% 
  sum()

# The subclassification estimator produces a smaller ATE than the naive 
# estimate, once we account for the ways age and sex might be related to the 
# assignment to treatment and the outcome.
s_ate

# The key thing to recognize is that if you look at the difference in means for
# each strata (without the size-based weights), we can recover the original
# naive difference in means by simply taking the unweighted mean of the p_diff
# category:

weights_df
mean(weights_df$p_diff)

# But this estimator gives just as much weight to our estimates of the six first
# class children as it does to our estimate from the 175 adult men—many of who
# did not survive. In fact, our subclassification estimate looks more similar 
# to the adult male estimate versus the the other estimates because there are so many
# more adult men on the boat. 

# In the end, first class passengers DO have a higher chance of surviving, but 
# it is about half as large as the naive estimate led us to believe.

