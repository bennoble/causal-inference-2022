# Causal Inference Lab 6: Sensitivity Analysis
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

library(tidyverse)
options(dplyr.summarise.inform = FALSE)

# Sensitivity analysis is a useful tool to help us probe the assumptions we are
# making in an observational analysis. Any observational analysis is weak to the
# question, "but did you control for U?" Often, we cannot control for every
# possible U—whether we cannot measure it, do not think about it, do not know it
# exists, etc. But we can use sensitivity analysis to show how assumptions about 
# some potential U could effect our results. 

# (An important thing to note is that we only care about U that affects both 
# treatment and outcome. If U only affects one of these things, then it would not
# bias our estimates of D --> Y.)

# On your homework, you'll conduct sensitivity analysis for some hypothetical U
# and show how it affects the conclusions on an actual ATE using real data. 
# But because U does not exist, it can be hard to "see" what's going on behind
# the scenes and what you're actually showing. Today, we're going to conduct 
# sensitivity analysis with a U we CAN observe, to show how its exclusion
# biases our results.

# We'll simulate some data to answer the following question: how responsive is 
# the federal government to local crises? We read the literature and hypothesize 
# that the federal government is likely to give more money to cities that are in
# crisis, and we come up with the following clever (?) identification strategy:
# natural disasters (e.g., hurricanes, tornadoes, floods) are semi-exogenous
# events, so we use that as our "crisis" treatment (D) and measure the amount of 
# federal money states receive (Y) following a disaster (treatment group) to the 
# amount of federal money states receive that did not have a natural disaster.

# You are excited about this study and tell me all about it. But I tell you there's 
# a problem. In the US, the president is the federal official who decides what 
# qualifies as a "federal natural disaster." He can choose whether to give money 
# to a locality or not. I tell you to read Kriner and Reeves (2015)^[Yes, that 
# Reeves] in the APSR which is about this very topic. They show that presidents 
# are more likely to give federal money to competitive and co-partisan localities 
# (U) following natural disasters (that is, U --> D). You realize that we could have
# confounding if partisanship is also correlated with the amount of money states 
# receive. You realize that most US cities are democratic leaning, so perhaps the 
# current democratic Congress is more likely to send them more money too (U --> Y)! 
# So, in any observational analysis, we need to control for city partisanship. 

# Here is our data:
#  - U is city partisanship. It is 1 if 51% of the city voted for Biden in 2020 
#    and 0 otherwise.
#  - Y_i0 is the amount of money a state receives in 1000s under potential control, 
#    normalized to 0.
#  - Y_i1 is the amount of money a state receives in 1000s under potential treatment. 

# We add U (a 0 or 1) variable to represent that co-partisan states often get 
# 1000 extra dollars (U --> Y). Clearly, these numbers are made up! 

set.seed(031022)
# partisanship, takes on a value of 1 if the president won the city in the 
# previous election
U <- rbinom(1000, 1, .8)
Y_i0 <- rnorm(1000, 0, 1) + U # potential outcome under control
Y_i1 <- rnorm(1000, 0.5, 1) + U # potential outcome under treatment

#  - D is whether a natural disaster was declared, but notice, democratic cities
#    are more likely to be treated! 

D <- c() 
for(i in 1:length(U)){ 
  # assign treatment based on value of U, U == 1 more likely to be treated
 D <- c(D, ifelse(U[i] == 1, rbinom(1, 1, 0.7), rbinom(1, 1, 0.3)))
}

# 1) Assign realized outcomes from the potential outcomes conditional on 
# treatment status

Y_i <- ifelse(D == 1, Y_i1, Y_i0)

# 2) Calculate the naive difference in means (i.e., Y ~ D) not accounting for
# our confounder U. Use OLS and extract the relevant coefficient.

df <- tibble(Y_i0, Y_i1, U, D, Y_i)

naive_ols <- coef(lm(Y_i ~ D, df))[2]

dim <- df %>% 
  group_by(D) %>% 
  summarise(eff = mean(Y_i)) %>% 
  pull(eff) %>% 
  diff()

# Our naive difference in means, not accounting for U, indicates that following a
# natural disaster declaration, a state receives ~$650 extra dollars from the 
# federal government
dim
naive_ols

# 3) Compute $\delta$ (d) and $\gamma$ (g) from the formulas on slide 13 from 
# class, d = Pr(U_i = 1 | D_i = 1) - Pr(U_i = 1 | D_i = 0) and g = E[Y_i | U = 1] - 
# E[Y_i | U = 0]. What do these two terms represent? NB: Your answer here will
# be a little off, to get the more precise answer, you can use the longer formula
# on slide 12 that does not assume a constant average treatment effect across U.

# A very circuitous route to computing d...
d <- df %>% 
  group_by(D, U) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = 'U', values_from = 'n') %>% 
  rename(u1 = `1`, u0 = `0`) %>% 
  mutate(tot = u1 + u0,
         prob_D = u1/tot) %>% 
  pull(prob_D) %>% 
  diff()

# d represents the imbalance between groups. We see that in our data, the treatment
# group has about 27% more U than the control group. That is, our treatment group
# is 27% more democratic than the control group.
d

# A not as circuitous route to compute g
g <- df %>% 
  group_by(U) %>% 
  summarise(g = mean(Y_i)) %>% 
  pull(g) %>% 
  diff()

# g represents the effect of U on Y. That is, democratic cities get about $1000
# more dollars. That makes sense given that we add U to the potential outcomes.
g 

# compute actual bias without assuming constant ATE
a <- df %>% filter(D==1 & U==1) %>% pull(Y_i) %>% mean() - df %>% filter(D==1 & U==0) %>% pull(Y_i) %>% mean()
b <- nrow(df %>% filter(D==1 & U==1))/nrow(df %>% filter(D==1)) - nrow(df %>% filter(U==1))/nrow(df)
c <- df %>% filter(D==0 & U==1) %>% pull(Y_i) %>% mean() - df %>% filter(D==0 & U==0) %>% pull(Y_i) %>% mean()
d <- nrow(df %>% filter(D==0 & U==1))/nrow(df %>% filter(D==0)) - nrow(df %>% filter(U==1))/nrow(df)

# 4) Multiply these two terms together. What does this quantity represent?

# This quantity is the amount of bias present in the naive estimate of the ATE
# not accounting for U. In a sensitivity analysis, we would say that a confounder
# that had an effect of 1 on the outcome and was 27% imbalanced across groups would
# shrink our ATE estimate by 0.2.

d*g # bias, slightly off from the assumption of constant effects
a*b-c*d # actual bias, not assuming constant ATE


# 5) Using OLS, regress our outcome on treatment and our partisanship confounder.
# Then compute the difference between our naive estimate from (1) and our new
# estimate of D from this model. What is this quantity? Explain what we've done 
# here and how we can apply this more broadly.

# We can include U as a predictor and now we de-bias our estimate of D. Note that
# it is now pretty close to the true ATE of 0.5. Also note that U = 0.8 as we set it 
# above.
adjusted_coef <- coef(lm(Y_i ~ D + U, df))[2]
adjusted_coef

# The difference between our naive ATE and adjusted ATE is pretty close to our
# bias calculation before (and very very close to the one using the more precise
# formula)
dim - adjusted_coef

# We can see that by not controlling for a confounder (that positively influences
# both our treatment and outcome), we have introduced positive bias into the 
# estimate of the ATE. When we include U, our bias goes away, and the amount of
# reduction is roughly equal to the amount of imbalance across groups and the 
# effect of U on Y. We see that when we include U, we reduce our ATE by 0.2. Is
# that a lot? It depends on the size of the effects of other variables we think 
# are important in our model that we can observe/control for. If we had some other
# observed confounder, V, we might compare the size and effect of U to V to decide
# if U was problematic.

# In reality, we are doing sensitivity analysis without knowing what U might be.
# The point of this exercise is to see what sensitivity analysis shows us. It shows
# us how the ATE changes as a consequence of an omitted confounder. We vary the 
# level of imbalance and the amount of reduction we would be worried about 
# (as defined by you the researcher) and then figure out the values of (d, g) that 
# would make us worried, as compared to other variables in our model.

