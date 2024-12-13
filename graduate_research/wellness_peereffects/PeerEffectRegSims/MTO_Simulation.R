##MTO Simulation
library(AER)
library(systemfit)
library(fastDummies)
library(ivreg)
library(tidyverse)
library(sandwich)
library(lmtest)
set.seed(547)

grouper <- function(df, n) {
  
  # create a random number for each row
  set.seed(2262024)
  random <- sample(1:nrow(df), replace = FALSE, nrow(df))
  
  # divide the random number by the group size
  df$group_number <- ceiling(random / (nrow(df) / n))
  
  #Add dummy variables for group membership
  df = dummy_cols(df, select_columns = "group_number")
  
  return(df)  
}

# We can first look at the MTO example. To simulate this, we need assign a 
# population to larger groups.
# In this model "peer_char" is the average of the peer characteristics
# that a group is assigned too. There is also an individual characteristic that drives wages.
# I then generate 2 wages, peer_wage and no_peer_wage, to capture a situation where 
# peer effects exist and one where they do not. 


#Need to switch this next to 2SLS when I get the chance. This weekend?
mto_pop = data.frame(char = rnorm(10000, 250, 5))
mto_groups = grouper(mto_pop, 10)
mto_groups = mto_groups %>%
  group_by(group_number) %>%
  mutate(peer_char = rnorm(1, 10, 1.5),
         peer_wage = char + 0.5*peer_char,
         no_peer_wage = char)
no_peer_reg = coeftest(lm(no_peer_wage~peer_char, data = mto_groups),
                       vcoc = vcovCL, 
                       type = "HC1", 
                       cluster = ~group_number)
peer_reg = coeftest(lm(peer_wage~peer_char, data = mto_groups),
                    vcoc = vcovCL, 
                    type = "HC1", 
                    cluster = ~group_number)
# As can be seen, this model captures peer effects when they exist (as the results 
# is statistically significant and close to the actual effect in the model) and does
# not when they do not exist (as the coefficient on peer effects in)

# I also included some work related to leave out mean below.

leave_out_pop = data.frame(characteristic = rnorm(800, 3, 0.25))
leave_out_comps = grouper(leave_out_pop, 200)
leave_out_comps = leave_out_comps %>%
  group_by(group_number) %>%
  mutate(group_effect = rnorm(1, 5, 0.5),
         characteristic_mod = characteristic + rnorm(1, 0.5, 0.1),
         leave_out = leave_out_mean(characteristic),
         leave_in = mean(characteristic))
leave_out_comps = leave_out_comps %>% 
  mutate(outcome_in = 5*characteristic + 0.5*leave_out,
         outcome_out = 5*characteristic + 0.5*leave_in)

group_model =coeftest(lm(outcome_group ~ leave_out, data = leave_groups), 
                      vcoc = vcovCL, 
                      type = "HC1", 
                      cluster = ~group_number)
peer_model = coeftest(lm(outcome_peer ~ leave_out, data = leave_groups),
                      vcoc = vcovCL, 
                      type = "HC1", 
                      cluster = ~group_number)
combined_model = coeftest(lm(outcome_combined ~ leave_out, data = leave_groups),
                          vcoc = vcovCL, 
                          type = "HC1", 
                          cluster = ~group_number)
#Talk to David about this step, should effect be random?
# Come back to Kenya experiment when you get the chance
