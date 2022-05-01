# Causal Inference Lab 5: Matching
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

# To get a sense of how matching works, we're going to conduct an exact matching 
# exercise "by hand" to estimate whether women are more effective lawmakers than 
# men in the U.S. Senate. Why might we suspect this? Volden and Wiseman (2010)
# argue that women are more likely to adopt strategies of exerting high effort, 
# consensus building, and issue specialization, making them more effective than
# male lawmakers. The authors are not making a causal argument (which seems hard
# to do), but we're going to pretend, for the sake of our exercise, that we could
# use matching to investigate this question causally.

# Our data come from Volden and Wiseman (2014), which can be found on their
# website https://thelawmakers.org/data-download. Their definition of legislative
# effectiveness (hereafter, LES) is based on a lawmaker's ability to get their
# bills through Congress, from introducing it to passing it into law. You can learn
# more about their methodology on their website. 

# To begin, I've imported the data for you from the website. To make our "by hand"
# analysis more tractable, I have also filtered our data to the 116th (2019-2020) 
# Congress. 

library(tidyverse)
library(haven)

leg_df <- read_dta('https://thelawmakers.org/wp-content/uploads/2021/03/CELSenate93to116Reduced.dta') %>% 
  filter(congress == 116)

# There are several variables in the data, but we're going to focus on a small subset: 
#   - Our dependent variable is the `les` variable, a measure of each lawmaker's 
#     effectiveness in the 116th Congress. 
#   - Our treatment is going to be `female`, which is 1 when the lawmaker is female
#     and 0 otherwise. 

# Of course, there may be confounding factors at play. One way these two groups
# might differ is in terms of their legislative experience. More experienced 
# legislators might be better able to advance their legislation. If experience 
# is correlated with both sex and effectiveness, then we might have a problem.

# We'll define experience in terms of years served in the Senate. You can use the
# variable `elected`, which tells us the year a senator was first elected to 
# the Senate. Theoretically, we might think that men have more experience in the 
# Senate than women because of the pre-existing gender gap that is only slowly 
# being made up in recent years.

# 1) Create a 2 x 3 table that shows us i) the female indicator, ii) mean years
# in each group, and iii) average LES in each group. Then, create a density plot 
# or histogram plotting the distribution of `elected` by sex. What do you observe?

leg_df %>% 
  group_by(female) %>% 
  summarise(prop_female = mean(elected), mean_les = mean(les))

# On average, men have 3 more years of experience. So, our theoretical supposition
# is confirmed. But is three years significant? Also, we see that without any 
# adjustment, we would conclude that men are more effective, however, this is not 
# an accurate estimate.

ggplot(leg_df, aes(x = elected, fill = as.factor(female))) + 
  geom_density(alpha = 0.5) + 
  labs(x = 'Year Elected', y = 'Density', fill = 'Female')

# We further confirm this theory with a density plot. We can see that the distribution 
# of `elected` for women is pretty far to the right of the male distribution, 
# indicating that as a whole, women have less experience in the Senate than men.

# To get a better estimate of `les ~ female`, we're going to use exact matching.
# (This is going to be a bit of a pain...but instructive!). I'll help you get
# started with the coding...

# We're going to start by getting a sense of how many men and women were elected
# in each year. 

n_by_female <- leg_df %>% 
  group_by(elected,female) %>% 
  summarise(n = n()) %>% 
  # `pivot_wider()` spreads out our data...can be confusing but very useful as is
  # its sibling, `pivot_longer()`.
  pivot_wider(names_from = 'female', values_from = 'n') %>% 
  rename(male = `0`, female = `1`)

n_by_female

# We now have a bird's-eye view of the number of men and women elected in each 
# year. We're going to focus on the ATT: the average treatment effect for women
# as compared to men who differ ~only~ in sex conditional on Senate experience. 
# (Here, I say ~only~ because we're pretending experience is the only confounder
# when of course there are many here we do not account for). To estimate the ATT
# we're going to subset on women and find comparable men. (We could estimate the
# ATE instead by preserving all data and finding comparable women for cells where
# there are not any to compare to men.)

# 2a) Subset the `n_by_female` df to just those cells where we observe women.

f_drop <- n_by_female %>% filter(!is.na(female))
f_drop

# 2b) Notice that, actually, there are areas where we do not have comparable men!
# Because we're focused on exact matching, drop those cells where we have no 
# comparable men. (Were we doing nearest-neighbor or approximate matching, we could
# preserve these cells.)

f_drop2 <- f_drop %>% filter(!is.na(male))
f_drop2

# Alright. Now we're getting closer. But notice, there are some years for which 
# we have more than one women meaning we'll need to find more than one male match.
# However, there are some years where we have more men than women and other years
# where the opposite is true. We're going to have to deal with these issues. There 
# are a number of defensible decisions one could make (which is why Christopher 
# highlighted the issue of the "garden of forking paths"). So here is my proposal
# Let's keep all women in the dataset so as to not lose treated observations. If we 
# have more women than men, we'll re-use male observations. However, if we observe
# more men than women, we'll only use as many men as we need for a 1-to-1 match
# and drop the others from our data. 

# 2c) We have 23 females and 46 males. Write a loop (or however else you choose)
# to create your matches. Hint: Create a loop that samples from an edited version 
# of our original `leg_df` df that contains only men [use sample_n()]. For example
# your loop should i) subset `leg_df` to men only and year[i], ii) sample n men
# with replacement so we create our matches. This df should have 23 male observations.

set.seed(123)
male_df <- tibble()

for(i in 1:nrow(f_drop2)){
  male_df <- bind_rows(male_df,
                          sample_n(leg_df %>% 
                                     filter(female == 0) %>% 
                                     filter(elected == f_drop2$elected[i]), 
                                   size = f_drop2$female[i], 
                                   replace = T))
}

# 2d) To finish creating our matched df, bind the rows of this all-male df with 
# a subset of our original `leg_df` df with only 23 women we're using (that is, 
# dropping those in years with no male matches). This should have 46 rows. 

female_df <- leg_df %>% filter(female == 1 & elected != '1992' & elected != '2019')

matched_df <- bind_rows(male_df, female_df)

# 3) Recreate your density plot/histogram from 1) with this matched df. Now what
# do you observe? 

ggplot(matched_df, aes(x = elected, fill = as.factor(female))) +
  geom_density(alpha = 0.5) + 
  labs(x = 'Year Elected', y = 'Density', fill = 'Female')

# Now these are perfectly overlapping. We have achieved covariate balance.

# Estimate the ATT. To do this...
# 4a) Subset your male df and female df to just the following variables: `icpsr`, 
# `elected`, and `les`.
# 4b) Arrange them in descending order by `elected` so they are in the same order
# 4c) Because these are now of equal size and the same order, you can bind the 
# columns. (Your colnames will be weird but that's ok)
# 4d) Estimate the ATT by subtracting the male `les` from the female `les` and 
# taking the average. What do you conclude?

wide_df <- bind_cols(male_df %>% select(icpsr, elected, les) %>% arrange(elected),
          female_df %>% select(icpsr, elected, les) %>% arrange(elected))

# Our matched data indicates that women are 0.1 points more effective than men 
# once we match on senatorial experience.

mean(wide_df$`les...6` - wide_df$`les...3`)

# 5) Recall that we just randomly sampled men when there are more than 1 possible 
# match. So, maybe our effect size is off because we randomly drew some ineffective
# men. One approach would be to use ALL male matches and average across. However, 
# we're going to bootstrap our result from 4) 500 times drawing different 1-to-1
# matches. What do you conclude?

est <- c()
set.seed(123)
for(j in 1:500){

  male_df_boot <- tibble()
  
  for(i in 1:nrow(f_drop2)){
    male_df_boot <- bind_rows(male_df_boot,
                         sample_n(leg_df %>% 
                                    filter(female == 0) %>% 
                                    filter(elected == f_drop2$elected[i]), 
                                  size = f_drop2$female[i], 
                                  replace = T))
  }
  
  wide_df <- bind_cols(male_df_boot %>% select(icpsr, elected, les) %>% arrange(elected),
                       female_df %>% select(icpsr, elected, les) %>% arrange(elected))

  est <- c(est, mean(wide_df$`les...6` - wide_df$`les...3`))
  
}

# Our effect size was a bit underestimated, but close to the mean. However, there
# is a lot of variation and it might be the case that women are actually less
# effective than men. It's hard to tell since we have a small sample size. 
hist(est)
quantile(est, c(0.025,.5,0.975))

# To conclude, we can get a similar (but slightly different) result using the 
# `Match()` function from the `Matching` package with the following settings. (Our
# estimate is different because the procedure to construct the matches is slightly
# different, but the same general principle applies...the key difference is that
# the code below will not reuse men once used, so we drop some women). 
library(Matching)

set.seed(123)
Match(Y = leg_df$les, 
      Tr = leg_df$female, 
      X = leg_df$elected, 
      exact = T, 
      replace = F,
      M = 1)$est

est_M <- c()
for(i in 1:500){
  est_M <- c(est_M, Match(Y = leg_df$les, 
                          Tr = leg_df$female,
                          X = leg_df$elected, 
                          exact = T, 
                          replace = F, 
                          M = 1)$est)
}

# From replicating this process 500 times, we can see that the distribution of 
# potential effects is pretty similar to what we did in the lab. 
hist(est_M)
quantile(est_M, c(0.025,.5,0.975))