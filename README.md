# Causal Inference 2022, Lab Exercises

Lab exercises I created in `R` as an assistant in instruction for graduate-level Causal Inference 2022 (Instructor of Record: Christopher Lucas).

If you find any errors, please let me know. Drop an issue here on GitHub or [DM me on Twitter](twitter.com/benjaminsnoble).

## Contents
- [Potential Outcomes Notation and Treatment Estimators](#week-1-potential-outcomes-notation-and-treatment-estimators)
- [CEF and Linear Regression](#week-2-the-conditional-expectation-function-and-linear-regression)
- [Power Analysis](#week-3-power-analysis-via-simulation)
- [Estimation via Subclassification](week-4-estimation-via-subclassification)
- [Matching](#week-5-matching)
- [Sensitivity Analysis](#week-6-sensitivity-analysis)
- [Instrumental Variables](#week-7-instrumental-variables)
- [Regression Discontinuity](week-8-regression-discontinuity)
- [Difference-in-Differences](week-9-difference-in-differences)

---

### Week 1: Potential Outcomes Notation and Treatment Estimators

- Demo: understand the difference between the ATE and ATT using a simulated job training program among a population of bankers and doctors.
- Demo: intuition for sources of bias in the simple difference in means estimate of the ATE (adapted from Section 4.1.3 of Scott Cunningham's [Causal Inference: The Mixtape](https://mixtape.scunning.com/potential-outcomes.html#simple-difference-in-means-decomposition)).

[Link to `R` code](/Lab1/lab1_att_po.R) with comments.

### Week 2: The Conditional Expectation Function and Linear Regression

- Exercise: estimate the gender gap in earnings using the CEF and linear regression. Why are these the same or different?
- Exercise: estimate the relationship between earnings and hours worked using the CEF and linear regression. Why are these the same or different?

[Link to `R` code](/Lab2/lab2_cef_regression.R) with comments and solutions.

### Week 3: Power Analysis via Simulation

- Demo: how to conduct power analysis via simulation.
- Exercise: conduct a power analysis for a hypothetical experiment in which we encourage people to display a flag and measure their feelings of patriotism. 
- Some useful `R` packages for power analysis.

[Link to `R` code](/Lab3/lab3_power.R) with comments and solutions.

### Week 4: Estimation via Subclassification

- Exercise: estimate the ATE of first class status on survival on the Titanic, conditional on age and sex (this exercise is heavily based on one in Scott Cunningham's [Causal Inference Mixtape](https://mixtape.scunning.com/matching-and-subclassification.html#subclassification-exercise-titanic-mathrmdata-set))

[Link to `R` code](/Lab4/lab4_sublcass.R) with comments and solutions.

### Week 5: Matching

- Exercise: using data from Volden and Wiseman (2010, 2014), estimate the effect of being female on legislative effectiveness (as compared to a matched set of male legislators). 

[Link to `R` code](/Lab5/lab5_matching.R) with comments and solutions.

### Week 6: Sensitivity Analysis

- Exercise: estimate the sensitivity of a model of natural disaster occurrence on state funding without controlling for city partisanship, using simulated data.

[Link to `R` code](/Lab6/lab6_sensitivity.R) with comments and solutions.

### Week 7: Instrumental Variables

- Exercise: estimate the effect of misdemeanor jail time on turnout (White 2019) using an instrumental variables design.

[Link to `R` code](/Lab7/lab7_iv.R) with comments and solutions.


### Week 8: Regression Discontinuity

- Exercise: estimate the effect of the 2021 stimulus payment on turnout using fake data and a regression discontinuity design.

[Link to `R` code](/Lab8/lab8_rdd.R) with comments and solutions.


### Week 9: Difference in Differences

- Exercise: estimate the effect of the pandemic on women academics' careers (as measured by Twitter behavior) from Kim and Patterson (2021).

[Link to `R` code](/Lab9/lab9_did.R) with comments and solutions.



