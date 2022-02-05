# Causal Inference Lab 1, Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

library(tidyverse)

### ATE vs ATT

# To illustrate differences between the ATT and ATE, we're going to imagine a job
# training program for bankers in the year 2100 when AI-enhanced ATMs have taken 
# all of their jobs. In our imaginary world, there are just two jobs, bankers and 
# doctors. But we're going to test a new program where we teach former bankers
# to become computer programmers who will ensure these AI-enhanced ATMs don't 
# take over the world. 

# Because doctors have not (yet) been replaced by robots, they are not going to be
# eligible for the training program. Just bankers. So, our treated group will just
# be bankers. To estimate the ATE, though, doctors will be among the control group
# because that's how we define the ATE...the average treatment effect among the 
# whole population.

# (Sidenote: We should probably also include doctors in the treatment group to get 
# an estimate of the ATE, but they're not eligible for the training program, so 
# we won't include them in the treated group...just go with it.)

# To start, let's create our population:

set.seed(123)
doctor <- runif(1000, 160, 260) # vector of doctor salaries ~U[160,260]
bankers <- runif(1000, 43, 64) # vector of banker salaries ~U[43,64]

# our population of workers
workers <- tibble(job = c(rep('doctor', 1000), rep('banker', 1000)),
       salary = c(doctor, bankers),
       id = 1:2000)

# draw a sample of bankers for treatment
set.seed(123)
treated_rows <- sample(1001:2000, size = 500) 
drop_treats <- workers[-treated_rows,] # drop the treated units
# draw a sample of control units from full population (both bankers and doctors)
control <-  sample_n(drop_treats, size = 500) # control df
treated <- workers[treated_rows,] # treatment df

# Let's apply the treatment effect, which is more effective for those making less
# money. If one's salary is less than 60K, the true treatment effect is a salary 
# increase drawn uniformly between 30-50K. But if someone makes more than 60K, 
# the training is less effective—an increase drawn uniformly between 0 and 15K. 
# This means almost all bankers will get the "good" treatment effects, but no 
# doctors would were they treated.
set.seed(123)
treat_eff <- vector() 
for(i in 1:nrow(treated)){
  treat_eff <- append(treat_eff, 
                      unlist(ifelse(treated[i,'salary'] < 60, 
                                    treated[i,'salary'] + runif(1, 30, 50), 
                                    treated[i,'salary'] + runif(1, 0, 15))))
}

# everyone in the control group has no change in salary
control_sals <- control$salary 

exp_group <- bind_rows(treated, control) # put the groups together
exp_group$new_sal <- c(treat_eff, control_sals) # add the new salaries
exp_group$treated <- c(rep(1, 500), rep(0, 500)) # indicator for treatment

# How many of each type of worker is in our sample?
# Actually, a lot of doctors...
exp_group %>% group_by(job, treated) %>% summarise(n = n())
 
# difference in means
t_res <- t.test(new_sal ~ treated, exp_group) 
ate <- t_res$estimate[2] - t_res$estimate[1] # the ATE
ate # -69.20

# Uh oh! Our ATE indicates that our treatment has a NEGATIVE effect! People who take
# our program seem to LOSE almost 70K on average. But recall, the ACTUAL treatment
# effect for bankers (the population we care about) is +40K on average. So, what's 
# going on? Well, we have 329/500 doctors in our control group. Their salaries are
# WAY higher than the banker's salaries. So even though bankers are getting more 
# it looks like our training program is costing people money because their 
# salaries are still lower on average than the doctors' salaries.

# Maybe this is obvious, but let's look closer:
# Among our different groups, the treated bankers are getting more money than the 
# control bankers, but the control doctors are still way richer.
exp_group %>% group_by(job, treated) %>% summarise(eff_size = mean(new_sal))
# But if we just throw away the info about the jobs, then we can recover the 
# ATE estimate from above.
exp_group %>% group_by(treated) %>% summarise(eff_size = mean(new_sal))

# The solution is the ATT—we construct a control population not from EVERYONE
# but from people who look like those we expect to take the treatment. In fact,
# we'd RATHER have this than the ATE because doctors will never take this program.
# In the real world, we might use MATCHING to achieve this ATT estimate.

# create a new control group from the leftover bankers in our population 
# (but not doctors)
control_banker <- workers[-treated_rows,] %>% filter(job == 'banker')
banker_control_sal <- control_banker$salary # carry over their salaries

# original treated bankers + control bankers
exp_group2 <- bind_rows(treated, control_banker) 
exp_group2$new_sal <- c(treat_eff, banker_control_sal) # the salaries
exp_group2$treated <- c(rep(1, 500), rep(0, 500)) # treatment indicators
 
t_res2 <- t.test(new_sal ~ treated, exp_group2) # difference in means
att <- t_res2$estimate[2] - t_res2$estimate[1]
att

# The ATT is positive, a 34.6K increase for the treated which is close to the true
# treatment effect of 40K on average for the bankers. 

# Key point: ATE can be misleading, sometimes we need to construct a control
# group that looks like the people we expect to receive treatment and estimate
# the ATT.

# Important theoretical subpoint: ATE and ATT are TRUE quantities, but we can 
# never get the actual values because we don't have both potential outcomes. 
# We can only estimate both using estimators (and making some assumptions).

################################################################################
### Decomposing difference in means
### Adapted from Cunningham (2021) 4.1.3.
### https://mixtape.scunning.com/potential-outcomes.html#simple-difference-in-means-decomposition

# The difference in means is an estimator for the ATE (or ATT) but the estimates
# themselves may be biased because we don't have access to all potential outcomes.
# Cunningham spells out sources of bias in 4.1.3. These include selection bias
# and heterogeneous treatment effect bias. Let's make this concrete with an example.

# We're going to create a population of 10 patients with cancer who will either
# be assigned to experimental surgery (treatment) or chemo (control). However, 
# we are friends with a "perfect doctor" who knows everyone's potential outcomes
# under both treatment and control. They tell us each patient's potential outcome
# and we make this table:

po_patients <- tibble(
  patients = c(1:10),
  y_i1 = c(7,5,5,7,4,10,1,5,3,9), 
  y_i0 = c(1,6,1,8,2,1,10,6,7,8),
  po_diff = y_i1 - y_i0
)

po_patients

# Since we know all potential outcomes, we can calculate the TRUE ATE 
# using counterfactual outcomes.
ate <- mean(po_patients$po_diff)
ate
# Notice that not everyone benefits from treatment! These are heterogeneous treatment
# effects.
po_patients[7,]

# Since we know the potential outcomes, we can actually ensure everyone gets their
# best outcome. Let's sort everyone into their best potential outcome because we're
# good people and prefer everyone get better even if it messes up causal inference.
# Note though, by doing this, we are introducing a dependence between treatment 
# assignment and the potential outcomes.
po_patients2 <- po_patients %>% 
  # assign treatment if potential outcome positive under treatment, else control
  mutate(D = if_else(po_diff >= 0, 1, 0), 
         # choose the observed outcome Y for each person based on treatment assignment
         Y = y_i1 * D + y_i0 * (1 - D))

po_patients2

# We can compute the ATT, the average difference between observed outcomes for the
# treated group less their outcome had THEY been in the control group. This is the
# actual ATT, not an estimate.
att <- po_patients2 %>% 
  filter(D == 1) %>% 
  mutate(tt = Y - y_i0) %>% 
  pull(tt) %>% 
  mean()

att
# People we assigned to surgery live 4.4 more years on average

# We can similarly compute the ATC (average treatment effect among control)
atc <- po_patients2 %>% 
  filter(D == 0) %>% 
  # note, to match Cunningham, define ATC as \E[Y_i1 | D_i = 0] - \E[Y_i0 | D_i = 0] 
  mutate(tt = y_i1 - Y) %>% 
  pull(tt) %>% 
  mean()

atc
# Notice, this is a negative quantity. Had these people had the surgery, they'd
# lose 3.2 years of life on average! But that makes sense because we sorted 
# everyone into their best outcome.


# Now, let's pretend we don't have the counterfactual data and cannot estimate
# the true ATT/ATC. Let's use the simple difference in means as an estimator for
# these estimands.

po_patients2 %>% 
  group_by(D) %>% 
  summarise(diff = mean(Y)) 

diff_means <- po_patients2 %>% 
  group_by(D) %>% 
  summarise(diff = mean(Y)) %>% 
  pull(diff) %>% 
  diff()

diff_means
# Notice that the simple difference in means (an estimate of the ATE) shows that
# surgery is bad on average.

# But! Difference in means = ATE + selection bias + heterogeneous treatment effect.
# See Cunningham 4.1.3. This looks different from what we saw in class (no hte bias)
# because in class we were estimating the ATT, here we're estimating the ATE.

# Recall the true ATE we get from the potential outcomes
ate

# Selection bias: \E[Y_i0 | D = 1] - \E[Y_i0 | D = 0]
# First quantity is not observed in real life, second is
s_bias <- po_patients2 %>% filter(D == 1) %>% pull(y_i0) %>% mean() -
  po_patients2 %>% filter(D == 0) %>% pull(Y) %>% mean() 
s_bias

# HTE bias: (1 - \Pr(assigned to control)) x (ATT - ATC)  
hte <- nrow(po_patients2 %>% filter(D == 0))/nrow(po_patients2) * (att - atc)
hte

ate + s_bias + hte
diff_means

# As Cunningham notes, the TRUE ATE is hiding inside the difference in means
# estimate...but we can't always find it because of these sources of bias.
# Even if we assume HTE is 0 (that is, ATT == ATC), selection bias is still an 
# issue. 

# We can solve this issue through random assignment, breaking dependence between 
# potential outcomes and assignment to treatment. Recall, we sorted everyone
# into their best outcome. Let's not do that this time. Let's just randomly
# assign into treatment and control. But our sample is only 10 people...even 
# random assignment will probably not totally solve our problems. So, we'll replicate
# random assignment 1000 times and record 1000 estimates of the ATE from the 
# difference in means estimator.

set.seed(132)
dim_est <- vector()
for(i in 1:1000){
 dim_est <-  append(dim_est,
                    po_patients %>% 
                      mutate(D = rbinom(10, 1, .5),
                             Y = y_i1 * D + y_i0 * (1 - D)) %>% 
                      group_by(D) %>% 
                      summarise(dim = mean(Y)) %>% 
                      pull(dim) %>% 
                      diff()
                    )
}
mean(dim_est) # pretty close to true ATE of 0.6

# Let's see what our distribution looks like (see also, central limit theorem)
ggplot(tibble(d = dim_est), aes(x = d)) + 
  geom_histogram() + 
  geom_vline(xintercept = ate, linetype = 'dashed') + 
  labs(x = 'Difference in Means Estimate', y = 'Count') 

# Still, large 95% CIs. True treatment effect could be 0.
quantile(dim_est, c(.025,.975)) 

# Let's do this even better. We recruit 1000 patients with cancer and estimate the 
# treatment effect using random assignment. We still call our doctor who tells us
# each patient's potential outcome though. For science.
set.seed(321)
po_patients_big <- tibble(
  patients = c(1:1000),
  # Treatment provides 0.6 extra years of life on average, but a large std. dev.
  y_i1 = rnorm(1000, .6, .5),
  # Control is also noisy.
  y_i0 = rnorm(1000, 0, .5),
  po_diff = y_i1 - y_i0) %>% 
  mutate(D = rbinom(1000, 1, .5), # random assignment
         Y = y_i1 * D + y_i0 * (1 - D))

po_patients_big
ate_big <- mean(po_patients_big$po_diff)
ate_big # true ATE using potential outcomes

# big att
big_att <- po_patients_big %>% 
  filter(D == 1) %>% 
  mutate(tt = Y - y_i0) %>% 
  pull(tt) %>% 
  mean()

big_att # ATT ~ = ATE

# big atc
big_atc <- po_patients_big %>% 
  filter(D == 0) %>% 
  mutate(tt = y_i1 - Y) %>% 
  pull(tt) %>% 
  mean()

big_atc # ATC ~ = ATT (no HTE bias)

# new difference in means
big_diff_means <- po_patients_big %>% 
  group_by(D) %>% 
  summarise(diff = mean(Y)) %>% 
  pull(diff) %>% 
  diff()

# now difference in means is VERY close to true ATE
big_diff_means

# no selection bias
s_bias_big <- po_patients_big %>% filter(D == 1) %>% pull(y_i0) %>% mean() -
  po_patients_big %>% filter(D == 0) %>% pull(Y) %>% mean() 
s_bias_big

# no HTE bias
hte_big <- nrow(po_patients_big %>% filter(D == 0))/nrow(po_patients_big) * (big_att -big_atc)
hte_big

# Now our difference in means basically recovers the true ATE and we aren't 
# concerned about selection bias or hte bias.
ate_big + s_bias_big + hte_big
big_diff_means

# Key point: Without information about potential outcomes, we may come up with
# biased estimates of the ATE. Randomization helps break dependence between potential
# outcomes and treatment assignment.
