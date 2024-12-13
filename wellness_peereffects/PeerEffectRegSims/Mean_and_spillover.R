##Peer Effect Simulation
library(AER)
library(systemfit)
library(fastDummies)
library(ivreg)
library(tidyverse)
library(sandwich)
library(lmtest)
set.seed(683)


#Leave out means: I'm going to move onto the issues that Angrist discusses with leave out means,
#specifically how they can mimic group specific shocks as well. I am going to start by generating 
#a population with a normally distributed characteristic.
leave_pop = data.frame(characteristic = rnorm(600, 50, 1))

leave_out_mean = function(x) {
  n = length(x)
  sum_except_x = sum(x) - x
  mean_except_x = sum_except_x / (n - 1)
  return(mean_except_x)
}

grouper_no_dummy <- function(df, n) {
  
  # create a random number for each row
  set.seed(2262024)
  random <- sample(1:nrow(df), replace = FALSE, nrow(df))
  
  # divide the random number by the group size
  df$group_number <- ceiling(random / (nrow(df) / n))
  
  return(df)  
}

# I then group the data into 150 groups of 4. We could see this as standing in for dorm rooms or other 
# social groups. I then calculated a group shock that is random ( we can assume it is unobservable). 
# I then add the group shock to the characteristic value and calculate the 
# leave out mean of this value. I then regress characteristic_mod (characteristic plus the group effect) on the leave_out mean.


leave_groups = grouper_no_dummy(leave_pop, 150)
leave_groups = leave_groups %>%
  group_by(group_number) %>%
  mutate(group_effect = rnorm(1, 5, 0.5),
         mean = mean(characteristic))
leave_groups = arrange(leave_groups, group_number)
leave_groups = leave_groups %>%
    mutate(characteristic_mod = characteristic + group_effect,
           leave_out = leave_out_mean(characteristic_mod),
           outcome_group = characteristic + group_effect
              ) 

characteristic_model = coeftest(lm(characteristic_mod ~ leave_out, data = leave_groups), 
                                vcoc = vcovCL, 
                                type = "HC1", 
                                cluster = ~group_number)
characteristic_model

# Even though there are no peer effects in this, the regression estimates a statistically  
# significant coefficient for leave out mean. It is just capturing intraclass correlation
# described by Angrist in the paper.

# Lets move to social spillover. Social spillover has the following 
# econometric model: y = (B_1)*(mean of x for group z) + B_2(value of x) +  error term;
# Angrist shows that B_1 is proportional to the difference between the 2SLS estimate of x instrumented by 
# and the OLS estimate of x when regressing y on x. One cause of this excess can be 
# measurement error/variation that is averaged away in group data. 
# Another cause may be selection bias, discount rate bias, and nonlinearity.
# The social return  model can also be adapted to include controls and 
# still fit with the 2SLS approach, you condition x on the controls then 
# instrument x with z (rather than instrumenting x with z and then conditioning 
# on the controls). 

# For this one, I am going to replicate the social spillover model in section 4.1.
# Should try to replicate 2 different biases: omitted variable bias and measurement
# error. For now, stick with just state instruments (talk to David about needing
# to include instruments for year). For OVB, need instrumental variable to affect both x
# and y (specifically, higher x and y) directly, not just through how IV affects x.
# For measurement error, need to add "noise" to x variable. For now, going to start with
# baseline, can add controls later or after talking to David. Also talk to David
# about whether modeling education as  a polynomials is a good idea.

#Notes ffrom David: Good place to work in randomization framework (tells you whether or not you can reject 
# null of no peer effects). Can estimate direction/magnitued and direction of peer effect. Push now should be to merge
# regression with randomization. Taking work from reg pov to randomization pov.Read paper on David's website (first on his website
# he will send it). Don't worry about proving theorems, focus on what is going on. Need to throw data to test for peer effects, how 
# do you do that.

# First, we randomly generate education for our entire population
social_return_pop = data.frame(educ = rnorm(1000, 14, 1.5)) %>%
  mutate(noise = rnorm(1000, 0, 2))
# Then we group them into 10 groups of equal size (these are "states")
return_groups = grouper_no_dummy(social_return_pop, 10)
#Then we create a group specific  "state" effect  that is randomly generated
return_groups = return_groups %>%
  group_by(group_number) %>%
  mutate(state_effect = rnorm(1, 3,1))
# Next, I am going to capture the measurement error and omitted variable bias. 
# To capture the omitted variable bias, I created the variable educ_state, a 
# linear combination of education and the state_effect. I also created the variable
# wage, a linear combination of educ_state and state_effect.As the state effect 
# impacts both educ and wage, it would violate the exclusion restriction, creating
# omitted variable bias. To simulate measurement error, I created educ_noise, which is a linear 
# combination of educ and a random generated value taken from a distribution of mean 0. Wage_noise
# was generated using the same equation as wage but with educ_noise instead of educ_state.
return_groups = return_groups %>%
  mutate(educ_state = educ + 2*state_effect,
         educ_state_noise = educ_state + noise,
         wage_state = 200 + 25*(educ_state) + 3*state_effect,
         wage_state_noise = 200 + 25*(educ_state) + 3*state_effect)

# First, we're going to show the effect of omitted variable bias, comparing OLS and 2SLS
# regressions of educ_state on wage.
return_OLS = summary(lm(wage_state~educ_state, data=return_groups))
return_2SLS = summary(ivreg(wage_state~educ_state 
                    |group_number, data = return_groups))
return_OLS
return_2SLS

return_2SLS$coefficients[2,1] - return_OLS$coefficients[2,1]
# Then we can see the effect of measurement error.
return_noise_OLS = summary(lm(wage_state_noise ~ educ_state_noise, data = return_groups))
return_noise_2SLS = summary(ivreg(wage_state_noise~educ_state_noise 
                    |group_number, data = return_groups))
return_noise_OLS 
return_noise_2SLS

return_noise_2SLS$coefficients[2,1] - return_noise_OLS$coefficients[2,1] 

# We can see similar results to what Angrist estimated. The difference between OLS
# and 2SLS does not capture peer effects but rather the omitted variable bias of
# state effects. Further, measurement error increases the apparent size of the 
# spillover by attenuating the OLS estimate of education.


# Moving onto leave out mean versus mean in social return regression. 
# First, we'll examine how leave out mean matters less and less as group size
# rises. We'll take the social return data from above as a starter
return_groups = return_groups %>%
  group_by(group_number) %>%
  mutate(leave_mean_educ = leave_out_mean(educ_state))
leave_return_OLS = coeftest(lm(wage~leave_mean_educ, data=return_groups),
                      vcoc = vcovCL, 
                      type = "HC1", 
                      cluster = ~group_number)
leave_return_2SLS = ivreg(wage~leave_mean_educ 
                    |educ_state+ group_number_1 + group_number_2 + group_number_3 + group_number_4 + 
                      group_number_5 + group_number_6 + group_number_7 + group_number_8 + 
                      group_number_9 + group_number_10, data = return_groups)
# For a very large group, the difference between 2SLS leave out and leave in mean are 
# nonexistent

# Single group example. Since we're using the leave out mean, we can 
# still do this regression, but it produces a negative coefficient
# for leave out mean.
single_pop = data.frame(characteristic = rnorm(800, 3, 0.25))
single_pop = single_pop %>%
  mutate(leave_out = leave_out_mean(characteristic),
         result = characteristic*0.5 + 3,
         result_out = characteristic*0.4 +leave_out*0.05+ 3)

single_reg = summary(lm(result~leave_out, data=single_pop))
leave_reg = summary(lm(result_out~leave_out, data=single_pop))

# Save Kenya example for later, as it basically boils down to Angrist 
# talking about how measurement error could be contributing to the issue
# Let's move onto effective examples of identifying peer effects.
