# Causal Inference Lab 7: Instrumental Variables
# Spring 2022
# Benjamin S. Noble
# benjaminnoble.org

library(dataverse)
library(AER)
library(tidyverse)

# White (2019) investigates the theory that short jail sentences can decrease one's 
# propensity to vote. Existing literature has shown that long jail sentences
# and mass incarceration reduce future turnout, but even a short jail sentence
# can dramatically impact one's life. She theorizes that a misdemeanor jail sentence
# can reduce turnout through i) "political socialization," a process where a bad
# experience with the government can reduce other interactions with the government
# and ii) time in jail can disrupt things like employment and housing, which may 
# make it harder to vote. She also hypothesizes that there is heterogeneity
# between white and black defendants on future voting behavior given differential
# exposure to prosecution and arrest.

# Her approach to answering this question causally is to use an IV strategy. In
# Harris County, TX, defendants are randomly assigned to one of 15 courtrooms with
# a judge who quickly decides their case and assigns prison time and/or fines. 
# Although jail time is not randomly assigned (and could be influenced by e.g.,
# seriousness of the crime, race, income, quality of the defense, etc) and could
# confound future voting behavior (e.g., perhaps more anti-social people are more
# likely to engage in crime and get jail time but also refrain from voting), White
# uses the exogenaity of the courtroom assignment (i.e., getting a harsher judge)
# as an instrument for jail time, which she uses to predict voting behavior.

# This summary is from White (2019), and I encourage you to read the paper 
# fully if you are interested in this topic or using IVs in your research. White
# does a superb job discussing the relevant assumptions needed, explaining her
# data and methods, and conducting various robustness checks. You can find the 
# article in the APSR here: https://doi.org/10.1017/S000305541800093X

# Today, we will replicate some of the main results from White (2019) to get a
# better understanding of how IV designs work.

# Helper function from `dataverse` documentation to read in data from dataverse
load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

# Read in White (2019) data, takes a few seconds to load; df will be called 'voter1'
voter1 <- get_dataframe_by_id(
  file = 'doi:10.7910/DVN/TWVXKZ/TMJQLL',
  server = "dataverse.harvard.edu",
  .f = load_object,
  original = TRUE)

# Of interest are the variables 
#   - `vote2012`. This is the DV. It is 1 if a defendant votes in 2012 and 0 o/w.
#   - `jail` is the endogenous treatment. It is 1 if someone is assigned any jail
#      time and 0 o/w.
#   - `crtjailavg1`. This is the instrument. It is the courtroom's incarceration
#      propensity—how likely is the courtroom (judge) to assign jail time. 
#      It is constructed as the mean incarceration rate in a given year (e.g., if 
#      55% of people going before this courtroom are assigned jail time, then 
#     `crtjailavg1` for that court-year is 0.55).
#   - `fyear` is a column of year fixed effects.
#   - `black` takes on a value of 1 if the defendant is black and 0 o/w.

# 1) First, let's do the observational analysis. Naively regress `vote2012` on
# `jail` using OLS.^[Logit might be more appropriate given the binary DV, but we
# will use OLS to keep things simple and consistent with the 2SLS approach we will
# take later] What do you conclude from this analysis, and why shouldn't we 
# necessarily trust the results?

# From this naive regression, we conclude that jail time has a statistically 
# significant effect—decreasing the probability of turning out by 10 percent. 
# However, it could be the case that there is some omitted variable that causes
# both jail time and lower voting rates. Perhaps defendants who get jail time 
# were more violent and also were less likely to vote already, so jail time has
# no causal effect.
summary(lm(vote2012 ~ jail, voter1))

# 2) Let's be slightly more sophisticated. We know that jail time is not randomly
# assigned. We can avoid the problem by focusing solely on the Intent to 
# Treat effect of getting assigned to a harsh courtroom on jail time. Compute
# the ITT. What do you observe. How would you interpret this quantity? Explain in
# terms of compliers, always takers, etc. Does this quantity make sense in the 
# context of our analysis?

# Getting assigned to a harsher court room seems to cause a decrease in future
# voting behavior of 0.05. This is smaller than the naive estimate at p < 0.1. I
# would interpret this quantity as the causal effect of getting "encouraged to 
# serve jail time" on future voting behavior. But this estimate does not actually 
# incorporate data about whether the defendant was assigned jail time (indeed not
# all defendants in harsh court rooms get jail time). Essentially, the people in
# harsher court rooms assigned jail time (and not jail time in more lenient
# court rooms) are the compliers, but we also have a mix of always takers (those 
# who would be assigned jail time regardless) and never takers (those who would 
# never get jail time regardless). Ultimately, this is kind of a weird quantity 
# in our case...it doesn't quite make sense—being "encouraged" to go to jail 
# and then complying or not! But notice it is smaller and not statistically 
# significant as the observational analysis was.
summary(lm(vote2012 ~ crtjailavg1, voter1))

# Now we can turn to our IV approach.

# 2a) Let's conduct 2SLS "by hand." First, conduct the first stage regression. 
# Regress our endogenous treatment, `jail`, on the instrument `crtjailavg1` and 
# the year fixed effects. With the results from this regression, which critical 
# IV assumption can we support?

# The first-stage results show that the court jail average has a strong, positive
# and statistically significant effect on the defendant being jailed. The first-
# stage F-statistic is large, much larger than the conventional F = 10 threshold,
# indicating that this is a strong instrument. 
first_stage <- lm(jail ~ crtjailavg1 + fyear, voter1)
summary(first_stage)

# 2b) Now we can conduct the second stage regression. Use your first stage 
# regression to get ^D, the fitted values that comprise our treatment. Then
# regress the dv `vote2012` on ^D and the year fixed effects—remember, your
# covariates need to be present in both stages!

D_hat <- first_stage$fitted.values
second_stage <- lm(vote2012 ~ D_hat + fyear, voter1)
summary(second_stage)

# 2c) Recall that our standard errors from this procedure 
# would not be correct. They are biased because we are using the second-stage
# residuals. To account for this, we can bootstrap the whole procedure. Bootstrap 
# your estimate of B_2SLS (both stages) 500 times. Then compute the revised 
# standard error. Interpret the results from (2b) and (2c).

set.seed(322)

se <- c()
for(i in 1:10000){
  df <- sample_n(voter1, size = nrow(voter1), replace = T)
  fsls <- lm(jail ~ crtjailavg1 + fyear, df)
  D_hat <- fsls$fitted.values
  ssls <- lm(vote2012 ~ D_hat + fyear, df)
  se <- c(se, coef(ssls)[2])
}

# The correct standard error is actually smaller. Note that if you compute the se
# using `ivreg` in the 'AER' package, it will differ slightly from our bootstrap
# estimate. If you increase your bootstrap runs, you'll converge to the
# se from `ivreg`.
sd(se)

# In terms of interpretation, we can see that the LATE is negative (and similar
# in magnitude to our ITT effect) but not statistically significant. We cannot
# reject the null that short jail sentences do not cause a decrease in turnout.

# 3) Now, conduct your IV analysis again. This time conduct two separate analyses
# with only white (black == 0) and only black (black == 1) defendants.^[When 
# conducting an IV analysis, you don't have to subset your data—you can include an 
# interaction term on the right and left hand sides. We are just subsetting here 
# to keep things simpler.] Use the package 'AER' which contains the function 
# `ivreg` this time. What do you conclude?
summary(ivreg(vote2012 ~ jail + fyear | crtjailavg1 + fyear, 
              data = voter1))

# From the IV results, there is some suggestive evidence that jail time causes a 
# decline in voting for black, but not white, defendants.
summary(ivreg(vote2012 ~ jail + fyear | crtjailavg1 + fyear, 
              data = voter1 %>% filter(black == 0)))

summary(ivreg(vote2012 ~ jail + fyear | crtjailavg1 + fyear, 
              data = voter1 %>% filter(black == 1)))

# Note that if you do the IV analysis with the interaction term, the result is
# now basically statistically significant for black defendants.
summary(ivreg(vote2012 ~ jail * black + fyear | crtjailavg1 * black + fyear, 
              data = voter1))

# 4) Beyond the assumption you tested for in (2a), name one other important
# assumption you need to identify the causal effect. How can you defend against 
# criticism around this assumption?

# You also need to ensure that the exclusion restriction is met. The exclusion 
# restriction says that the instrument can only affect the outcome through the
# treatment and in no other way. Here, that means that harsh courtroom sentences
# can only affect vote probability through the treatment. This assumption would
# be violated if, e.g., harsher judges ended each trial by saying "I don't 
# think people convicted of misdemeanors should be allowed to vote" thereby 
# potentially influencing future voting decisions. You cannot empirically 
# defend against these kinds of attacks (as you can with instrument strength).
# You can only argue on theoretical grounds and need to have a convincing design,
# otherwise, it will be very hard to publish your paper. See White (2019) as she
# does a great job with this. 
