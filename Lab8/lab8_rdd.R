# Causal Inference Lab 8: Regression Discontinuity
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

library(tidyverse)
library(haven)

# So, I had a great plan to create a plausibly realistic but fake RDD study.
# Turns out that didn't end up happening. I went a little off the deep end here
# but hey, you'll still learning something!

# As most RDD studies surely begin, someone looks at a program with a sharp 
# cutoff and says "hey, what questions can I answer with this using an RDD?" One
# thing I thought of was that in the spring of 2021, President Biden and Congress
# passed the American Rescue Plan, which, among other things, provided $1,400 
# stimulus payments to most Americans. Unlike some previous rounds of 
# COVID-related stimulus funding, this round of funding had an eligibility 
# requirement. Past a certain income threshold, Americans would no longer receive 
# a stimulus payment.^[In reality, there was a phase-out between $75K and $80K 
# for individuals and $150K to $160K for couples.] Income was determined by your
# 2020 tax return, so it was based on a previous income measure. Because the 
# phase out and individuals vs families thing can get complicated, let's suppose
# instead that there was a sharp cutoff at $75K. You got the payment if you
# reported less than $75K on your 2020 tax return and otherwise you did not. Also, 
# let's suppose that everyone filed individually. 

# Armed with an identification strategy, let's come up with our question: does 
# an increase in income cause one to be more likely to turn out to vote? There 
# is definitely a correlation between income and voting in the US (and maybe a 
# study about the causal effects?), but that relationship could simply be due to
# e.g., more knowledgeable people are both more likely to earn a higher income and 
# learn about political issues and thus turn out to vote on those issues. It's 
# not clear if the money itself has a causal effect. So, we will use an RDD to 
# investigate whether increased income has a causal effect on voting. 

# Sidebar: Actually, it makes more theoretical sense that having a higher income
# would enable one to e.g., access transportation to get to the polling place. 
# So, it's not necessarily that the money itself is causing voting, but money 
# facilitates access to mediators that enable more voting. Anyway...let's just
# move forward and investigate whether increasing your income in a given year (
# here, defined as receiving an unexpected $1,400) increases your probability of
# voting. We'll measure turnout as an individual's latent probability
# of turning out in the 2022 midterm elections that we somehow magically observe?

# To get started, let's read in the 2009 Current Population Survey data from 
# Bruce Hansen (we used this in Lab 2). This is an economics dataset that has
# some basic information about many US households. Of interest to us is the 
# earnings column, which will be our running (or forcing) variable. I have 
# imported this data below and made some slight adjustments to the earnings 
# variable to make it easier to work with. Specifically, I have i) divided by 
# 1,000 just to make the units smaller and easier to work with, ii) added some
# random noise to make the data smoother and more continuous, iii) removed some
# outliers to facilitate plotting the data. 

# Oh...also let's pretend this is data from 2020 and not 2009!

set.seed(0401)
cps <- read_dta('https://www.ssc.wisc.edu/~bhansen/econometrics/cps09mar.dta') %>% 
  # rename as pre-treatment earnings, divide by 1000 to make variable easier to 
  # use, and add noise to smooth out values which are reported as whole numbers
  mutate(earnings_pre = earnings/1000 + rnorm(n(), 1, 2)) %>% 
  filter(earnings_pre < 150 & earnings_pre > 0) # trim outliers

# 1a) By convention, we often transform our running variable so that the cut-point
# is at 0 (rather than, e.g., 75). Although it won't necessarily change your 
# findings, it will change the intercept. Let's follow convention. Create
# a new variable called `earnings_c` by subtracting 75 from the `earning_pre`
# variable. Then, assign treatment by creating a variable named `D` that takes
# on a value of 1 when a respondent's income is below the cut-point, and 0 when
# above. (Do use the specified names above, otherwise future code I've written
# for you below will not work).

cps <- cps %>% 
  mutate(earnings_c = earnings_pre - 75,
         D = if_else(earnings_c < 0, 1, 0))

# 1b) Create a plot with `earnings_c` on the x-axis and treatment (`D`) on the 
# y-axis. Also, use a different color for treated and untreated units. What does 
# this plot tell us? 

# This plot tells us that we are conducting a sharp RD design. Everyone below the
# cut-point is treated. Everyone above is untreated. It's important to make this
# plot because that way we can ensure that the RD is indeed sharp as we suspected.
ggplot(cps, aes(x = earnings_c, y = D, color = as.factor(D))) + 
  geom_point() + 
  labs(x = 'Income (Transformed)', y = 'Got Stimulus (Treatment)', 
       color = 'Got Stimulus')

# I am going to create the treatment effect below using the following data-
# generating process. The baseline probability of turnout is 25%. Those in the
# treatment have an increase of 3 percentage points. Each additional $1,000 in 
# earnings leads to a 0.2 percentage point increase in turnout. The error is 
# normally distributed with an sd of 5. (Credit to Scott Cunningham for this
# code; https://mixtape.scunning.com/regression-discontinuity.html)

set.seed(0401)
cps <- cps %>% 
  mutate(y = 25 + 3 * D + .2 * earnings_pre  + rnorm(n(), 0, 5))

# 2) Estimate the effect of the treatment using the full data. That is, regress
# `y` on `D` and `earnings_c`. What do you conclude? Make sure you understand how 
# to interpret what it is we are actually estimating here.

# Here, we estimate the LATE at the cut-point. That is, the coefficient on D is
# 3.08 (basically the true treatment effect), which tells us that the effect of 
# the stimulus increases the probability of voting by about 3% at the cut point 
# (people who make $75K). We do conclude that more income (or a cash infusion) 
# affects turnout probability, but again, we can only say it's true near the 
# cut-point.
rdd1 <- lm(y ~ D + earnings_c, cps)
summary(rdd1)

# 3a) Create a plot with `earnings_c` on the x-axis and `y` on the y-axis. Add
# the linear fitted lines (with the same slope on each side of the cut-point) to
# illustrate the effect of the treatment. (Hint: you can do this in ggplot by
# adding a column of the fitted values from your model to the `cps` data and
# calling `geom_line()`. Be sure to use different colors conditional on treatment
# so we can see the lines).

cps <- bind_cols(cps, fit = rdd1$fitted.values)

ggplot(cps, aes(x = earnings_c, y = y)) + 
  geom_point() + 
  geom_line(aes(x = earnings_c, y = fit, color = as.factor(D)), size = 2) + 
  labs(x = 'Income (Transformed)', y = 'Turnout Probability', 
       color = 'Got Stimulus' )

# 3b) This plot is kind of messy. As such, people typically plot binned values of
# the running variable. To do this, first round income to the nearest (thousandth)
# dollar. Then take the average turnout probability in each of those strata. Plot
# the binned averages and your fitted lines from (3a). The discontinuity should be
# much clearer.

cps_bin <- cps %>% 
  mutate(bins = round(earnings_c)) %>% 
  group_by(bins, D) %>% 
  summarise(mean_y = mean(y))

ggplot(cps_bin, aes(x = bins, y = mean_y)) + 
  geom_point() + 
  geom_line(data = cps, aes(x = earnings_c, y = fit, color = as.factor(D)),
            size = 1) + 
  labs(x = 'Income (Transformed)', y = 'Turnout Probability', 
       color = 'Got Stimulus' )

# 4a) We used the full data to estimate this treatment effect. Now, subset your 
# data to within $3,000 of the cut-point on each side. Re-estimate the linear 
# model with constant slope. What do you conclude

small_cps <- cps %>% # subset data
  filter(earnings_c > -3 & earnings_c < 3) 

# Now, we are slightly closer to the true treatment effect. But it is quite
# similar to our original specification. 
rdd2 <- lm(y ~ D + earnings_c, small_cps)
summary(rdd2)

# 4b) Create a plot similar to (3a) with this new specification. Include the best
# fit lines.
small_cps <- bind_cols(small_cps, fit2 = rdd2$fitted.values)

ggplot(small_cps, aes(x = earnings_c, y = y)) + 
  geom_point() + 
  geom_line(aes(x = earnings_c, y = fit2, color = as.factor(D)), size = 2) + 
  labs(x = 'Income (Transformed)', y = 'Turnout Probability', 
       color = 'Got Stimulus')

# 5) Re-estimate your model varying the bandwidth. Start with the largest possible 
# bandwidth using all the data (i.e., [-75,75]) and save the coefficient on `D`
# and its associated confidence interval. Then, reduce your bandwidth by 3 on 
# each side (i.e., [-72,72]), and so on until you arrive at [-3,3]. Make a plot
# with the bandwidth size on the x-axis and the coefficient estimate and associated
# confidence intervals on the y-axis. What do you conclude?

eff <- lwr <- upr <- band <- c() # vectors for values
# sequence from min (i.e, -75) to -3 by 3
for(i in seq(round(min(cps$earnings_c)), -3, by = 3)){
  # estimate the model within that bandwidth
  ols <- lm(y ~ D + earnings_c, cps %>% filter(earnings_c > i & earnings_c < -i))
  # record size of the bandwidth (double absolute value of i)
  band <- c(band, abs(i * 2))
  eff <- c(eff, coef(ols)[2]) # coefficient
  lwr <- c(lwr, confint(ols)[2,1]) # lower bar
  upr <- c(upr, confint(ols)[2,2]) # upper bar
}

coef_est <- tibble(eff, lwr, upr, band) # create df from data

# From the plot, it appears that across all specifications, our estimate of the
# LATE is pretty close to the true effect. Occasionally we overestimate or 
# underestimate, but this would make us feel good if we were going to publish
# this paper. Sensibly, the confidence interval is larger when we include less
# data, and things stabilize pretty quickly. Our confidence intervals always
# include the true value of 3. Of course, we wouldn't know the true value in real 
# life, so we have to look at this plot and do our best to determine the proper 
# bandwidth. For us, it probably doesn't matter too much since the estimates are
# all very similar. There are also algorithms to help with this.
ggplot(coef_est, aes(x = band, y = eff)) + 
  geom_point() + 
  geom_errorbar(aes(x = band, ymin = lwr, ymax = upr), width = 0) +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_hline(yintercept = 3, linetype = 'dashed') +
  labs(x = 'Bandwidth Size', y = 'Treatment Coefficient Estimate')

# 6) What key assumption do you need to satisfy if you want to say increased
# income _causes_ higher probability of turnout? Is it plausibly satisfied in this
# super plausible study?

# You need to satisfy the continuity assumption—that is, there is no herding or
# (a technical term) "funny business" going on near the cut-point. We could not
# conduct this study if somehow people were able to under-report their income or
# withhold labor to get under the $75K threshold. Given that the stimulus payment
# was based on the previous year's income and people did not know there would be
# a stimulus payment or threshold, it seems implausible that they could herd
# around the cut-point. We can also inspect this graphically with a histogram. 

# We do not see any evidence of a discontinuity in the running variable (but of 
# course we don't because this is data from 2009!). Nonetheless, this is the sort
# of exercise you'd conduct with real data.
ggplot(cps, aes(x = earnings_c)) + 
  geom_density() + 
  geom_vline(xintercept = 0) + 
  labs(x = 'Income (Transformed)', y = 'Density')

