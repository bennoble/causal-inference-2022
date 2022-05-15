# Causal Inference Lab 9: Difference-in-Differences
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

# To improve our understanding of difference-in-differences (DiD) designs, we'll
# be using data from a recent PS: Political Science and Politics paper about 
# COVID and gender inequality in academia. The paper is by Kim and Patterson (2021)
# and can be found here: doi:10.1017/S1049096521001049. Credit to David Miller 
# for directing my attention to this article.

# The authors theorize that the pandemic disproportionately affected womens' 
# academic careers given disparities in the share of household work and childcare 
# women do relative to men. To assess the impact of the pandemic on careers of 
# women academics, the authors measure the frequency with which women academics 
# tweet about work before and after the pandemic using a keyword-based approach. 
# Of course, there is no counter-factual world where women are not "treated" by 
# the pandemic, so there is no true control group. As such, the authors use a 
# DiD in which men serve as  the counter-factual control group given an 
# assumption that mens' care-giving responsibilities did not increase as a 
# consequence of the pandemic. The treatment in the paper is the day Trump 
# declared a national emergency which occurred on 03/13/2020.

library(dataverse)
library(lubridate)
library(lfe)
library(tidyverse)

# Read in Kim and Patterson (2021) data, takes a few seconds to load; df will be 
# called `tweet_dat`.
tweet_dat <- tibble(
  get_dataframe_by_name(
    filename = 'ps_data.RDS',
    dataset = 'doi:10.7910/DVN/IDCU63',
    server = "dataverse.harvard.edu",
    .f = read_rds,
    original = TRUE)
  ) %>% 
  mutate(date = as_date(date), # convert character dates to dates
         # follow their coding scheme and convert dates to weeks beginning with
         # Saturdays
         wk = floor_date(date, unit = 'week', week_start = 6),
         female = if_else(gender == 1, '1', '0')) %>% # convert female to character
  select(-gender) # drop gender which we no longer need

# Key variables:
#  - `wk`: the week-year a tweet was sent
#  - `female` 1 if twitter user is female, 0 o/w
#  - `pre_post` 1 if after the pandemic was declared, 0 before
#  - `work` 1 if the tweet is about work, 0 o/w
# 
# (NB: `pandemic` is about pandemic-related tweets, not the treatment)

# 1a) Before we proceed with our DiD design, we need to make one important
# assumption. What is that assumption? Describe it in your own words and using
# the notation in class. Can we actually test this assumption directly?

# The key assumption is the parallel trends assumption. This assumption states that
# we need to assume that there are no time-variant, group-specific unobservables.
# Another way of saying this is that we need to assume that in the absence of 
# the treatment, the differences in the two groups would have been the same over 
# time. Mathematically, we can write the parallel trends assumption as: E[Y_i1(0) 
# - Y_i0(0)|G=1] - E[Y_i1(0) - Y_i0(0)|G_i = 0] = 0 (see slide 12). We cannot 
# formally test this because doing so requires counter-factual observations, but 
# we try to provide some support by plotting the pre-period trend line for both
# groups.

# 1b) Provide support for this assumption with a plot (this is Figure 2, top 
# panel in the article). First group your data by `female` and `date` and compute
# the total number of work related tweets by group and divide by the total number
# of tweets sent by that group to get the proportion of work-related tweets. Then
# create a plot with the day on the x-axis and the proportion of work related 
# tweets on the y-axis. Use the color aesthetic for `female` and use the line of
# code below (from authors' replication file) to create the smoothed loess line.
# Additionally, I've included some code to mark two key dates from the paper—the
# date of the first COVID case in the US (2020-01-21) and the date Trump declared
# a national emergency (2020-03-13). Is the assumption from (1a) satisfied? 

daily_tweets_by_gender <- tweet_dat %>% 
  group_by(female, date) %>% 
  summarise(s_work = sum(work), n = n()) %>% 
  mutate(p_work = s_work/n)

# This plot presents graphical evidence of parallel trends. There is some
# concern that this assumption does not hold around October 2019, when women
# seem to be tweeting more about work than men—but generally, the trends are 
# parallel and this seems to be particularly true in early 2020 preceding the
# key cut-point of 03/13/2020.
ggplot(daily_tweets_by_gender, aes(x = date, y = p_work, color = female)) + 
  geom_point() + 
  geom_vline(xintercept = c(as_date('2020-03-13'), as_date('2020-01-21')), 
             linetype = 'dashed') + 
  geom_smooth(aes(x = date, y = p_work, fill = female), span = 0.2) +
  labs(x = 'Date', y = 'Proportion Work Tweets', 
       color = 'Female', fill = 'Female') 
  
  
# Although the data is quite rich and would allow us to do a more sophisticated
# analysis (as the authors do) using multiple pre and post-period time points
# and fixed effects, to illustrate the core components of DiD designs, I'm going 
# to collapse the data into a single pre-period and post-period for each person.

tweet_dat_prepost <- tweet_dat %>% 
  group_by(screen_name, pre_post, female) %>% 
  summarise(s_work = sum(work), n = n()) %>% 
  mutate(p_work =s_work/n)

# 2a) Use the `tweet_pre_post` data to calculate the average proportion of work 
# tweets by female/male x pre_post and use that information to fill in the table 
# below (see also slide 17). From this table, what do you conclude?

#                     After (T_i = 1)     Before (T_i = 0)      After - Before
# Treated (G_i = 1)   
# Control (G_i = 0)
# Treated - Control

tweet_dat_prepost_bin <- tweet_dat_prepost %>% 
  group_by(pre_post, female) %>% 
  summarise(mp_work = mean(p_work))

#                     After (T_i = 1)     Before (T_i = 0)      After - Before
# Treated (G_i = 1)    0.118               0.157                -0.039                
# Control (G_i = 0)    0.142               0.164                -0.022
# Treated - Control   -0.024              -0.007                -0.017
# 
# From this table, we can conclude that the pandemic had a negative causal impact
# on womens' work-related tweet behavior. A decrease of about 1.7 percentage points. 
# It's not clear if this is statistically significant, but it is in line with 
# the authors' hypothesis.

# 2b) Using your table above (or the dataframe you used to create the table),
# create a plot showing the difference in differences effect similar to that on 
# slide 11. This plot will have six points. Four of the points will come from 
# the table above, and the other two will be counter-factual estimates of female
# tweet behavior in the absence of treatment (you'll need to create these 
# yourself and bind those rows to the df above). Then, plot those six points and
# three lines connecting them. (Hint: color by `female` and linetype by counter-
# factual or observed outcomes). Do you recover the DiD estimate from (2a)?

# The key thing to note is that we use womens' pre-treatment tweet proportion and
# add mens' pre-post difference to get womens' counter-factual post-treatment 
# outcome.
did_plot <- bind_rows(tweet_dat_prepost_bin %>% mutate(cf = '0'),
          tibble(pre_post = c(0,1), female = c('1','1'), 
                 mp_work = c(0.157, 0.157 - 0.022), cf = '1'))

ggplot(did_plot, aes(x = pre_post, y = mp_work, color = female, linetype = cf)) +
  geom_point() +
  geom_line() + 
  labs(x = 'Pre/Post', y = 'Proportion Work Tweets', 
       color = 'Female', linetype = 'Counterfactual\nOutcome') + 
  geom_segment(aes(x = 1, xend = 1, y = 0.135, yend = 0.118), 
               linetype = 'dashed', color = 'black') + 
  geom_text(aes(x = 0.85, y = 0.13), label = 'DiD\nEstimate >', color = 'black')

# 3) Using OLS, estimate the difference-in-differences effect using the
# `tweet_dat_prepost` data. That is, regress the proportion of work related 
# tweets on the treatment, `female`, and the interaction. Which coefficient is
# the DiD effect?

# Here, we recover the estimates from the table. The coefficient on the 
# interaction is equivalent to the difference-in-difference estimate in the table
# (with some rounding). Here, it appears that there is a significant
# difference in tweeting behavior as a consequence of the pandemic.
summary(lm(p_work ~ pre_post * female, tweet_dat_prepost))
