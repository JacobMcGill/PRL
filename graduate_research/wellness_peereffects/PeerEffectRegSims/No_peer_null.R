##No peer null Simulation
library(AER)
library(systemfit)
library(fastDummies)
library(ivreg)
library(tidyverse)
library(sandwich)
library(lmtest)
grouper_no_dummy <- function(df, n) {
  
  # create a random number for each row
  random <- sample(1:nrow(df), replace = FALSE, nrow(df))
  
  # divide the random number by the group size
  df$group_number <- ceiling(random / (nrow(df) / n))
  
  return(df)  
}
# Now lets move onto to the job spillover example. In this model, we have
# job status = mu + treatment_prop + received job assistance + error. 
# In this RCT, treatment_prop was randomly assigned to 235 labor markets.
# Individuals received job search assistance at the rate of treatment_prop.
# In this model, we have 2 binary variables (job status and received job assistance)
# and a rate, treatment_prop. The goal of this model is to demonstrate how it can show
# the no-peer-effects null. In terms of the paper, this is 
# the regression of job (an indicator taking a value of 0 or 1) 
# on job_assist (an indicator for receiving aid) and 2SLS estimates equal each other when there is 
# no null.
# First, we will generate the data. I used GPT to help create this code. I generated the labor 
# 235 markets, randomly assigning them treatment rates of 0, 0.25. 0.5, 0.75, and 1. I then 
# randomly generated 250 laborers for each market, who were randomly assigned job_assist
# at the rate of their market.

treat_prop = c(0, 0.25, 0.5, 0.75, 1)
lab_mkt = 235
mkt_asg = sample(treat_prop, lab_mkt, replace = TRUE)
cities = data.frame(treat_prop = mkt_asg)
cities = grouper_no_dummy(cities, 235)
cities = arrange(cities, group_number)


# Define function to generate entries based on rate
generate_entries = function(rate, num_entries) {
  entries = sample(c(0, 1), num_entries, replace = TRUE, prob = c(1 - rate, rate))
  return(entries)
}


# Initialize vectors to store data
entries = c()
rate_assigned = c()
group = c()

# Generate entries for each value in mkt_asg
for (i in 1:length(mkt_asg)) {
  num_entries = 1000  # Number of entries for each value in mkt_asg
  rate = mkt_asg[i]
  generated_entries = generate_entries(rate, num_entries)
  entries = c(entries, generated_entries)
  rate_assigned = c(rate_assigned, rep(rate, num_entries))
  group = c(group, rep(i, num_entries))
}

# Create dataframe to store data
city_data = data.frame(
  assist_rate = rate_assigned,
  city = group,
  job_assist = entries
)
# I calculated some summaries of the data just to check that the data lined up with what
# I wanted to do. Next, I generated jobs in a situation with no peer effects/negative
# spillover and in a situation with spillover. job_no_peer captured the former, job_peer
# captured the latter. 
city_data_sum = city_data %>%
  group_by(city) %>%
  summarize(obs_rate = mean(job_assist),
            act_rate = mean(assist_rate))
city_data = city_data %>%
  mutate(job_no_peer = ifelse(job_assist == 1,
                              yes = rbinom(235000, 1, 0.35),
                              no = rbinom(235000, 1, 0.25)),
         job_peer = ifelse(job_assist == 1,
                           yes = job_no_peer,
                           no = rbinom(235000, 1, 0.25 - 0.15*assist_rate)))

assist_summary = city_data %>%
  group_by(job_assist) %>%
  summarize(peer_job_rate = mean(job_peer),
            nopeer_job_rate = mean(job_no_peer))
ggplot(city_data_sum) +
  geom_histogram(aes(x = act_rate))
ggplot(city_data_sum) +
  geom_histogram(binwidth = 0.05, aes(x = obs_rate))

# coeftest(lm(job_no_peer ~ job_assist, data = city_data), 
#          vcoc = vcovCL, 
#          type = "HC1", 
#          cluster = ~group_number)
# 
# coeftest(lm(job_no_peer ~ assist_rate , data = city_data), 
#          vcoc = vcovCL, 
#          type = "HC1", 
#          cluster = ~group_number)
assist_sum = summary(lm(job_no_peer ~ job_assist, data = city_data))

rate_sum = summary(lm(job_no_peer ~ assist_rate , data = city_data))

# This is where we are getting some counter-intuitive results again here. Under this model,
# the regression of job_no_peer on job_assist (psi_0 in the Angrist paper) should equal the 
# regression of job_no_peer on assist_rate (psi_1 in the Angrist) or we should fail to 
# reject the null hypothesis that the psi_0 and psi_1 equal other. Since that can be 
# rewritten as the difference of the 2 is 0, I calculated the z-score for that difference
# below using the formula (psi_1 - psi_0)/(sd_psi_1^2 + sd_psi_0^2)^(1/2) (shown below) and 
# got a z-score of 2.87, which is statistically significant.

coeff_diff = (rate_sum$coefficients[2,1] - assist_sum$coefficients[2,1])
sd_sum = (rate_sum$coefficients[2,2]^2 + assist_sum$coefficients[2,2]^2)
z_score = (coeff_diff)/(sd_sum)^(0.5)
z_score
# Next step: Looks like we got the monte carlo simulation to work.
# Next, we need to store the individual coefficients from the regression 
# to ensure that the simulations are working properly.

no_peer_simulation = function(num_simulations) {
  coeff_diff_values <- numeric(num_simulations)
  z_scores <- numeric(num_simulations)
  
    for (j in 1:num_simulations) {
      treat_prop = c(0, 0.25, 0.5, 0.75, 1)
      lab_mkt = 235
      mkt_asg = sample(treat_prop, lab_mkt, replace = TRUE)
      cities = data.frame(treat_prop = mkt_asg)
      cities = grouper_no_dummy(cities, 235)
      cities = arrange(cities, group_number)
      
      # Initialize vectors to store data
      entries = c()
      rate_assigned = c()
      group = c()
      for (i in 1:length(mkt_asg)) {
        num_entries = 1000  # Number of entries for each value in mkt_asg
        rate = mkt_asg[i]
        generated_entries = generate_entries(rate, num_entries)
        entries = c(entries, generated_entries)
        rate_assigned = c(rate_assigned, rep(rate, num_entries))
        group = c(group, rep(i, num_entries))
      }
      city_data = data.frame(
        assist_rate = rate_assigned,
        city = group,
        job_assist = entries
      )
    
      city_data = city_data %>%
        mutate(job_no_peer = ifelse(job_assist == 1,
                                    yes = rbinom(235000, 1, 0.35),
                                    no = rbinom(235000, 1, 0.25)),
               job_peer = ifelse(job_assist == 1,
                                 yes = job_no_peer,
                                 no = rbinom(235000, 1, 0.25 - 0.15*assist_rate)))
      
      assist_sum <- summary(lm(job_no_peer ~ job_assist, data = city_data))
      rate_sum <- summary(lm(job_no_peer ~ assist_rate , data = city_data))
      coeff_diff_values[j] <- rate_sum$coefficients[2,1] - assist_sum$coefficients[2,1]
      
      sd_sum <- (rate_sum$coefficients[2,2]^2 + assist_sum$coefficients[2,2]^2)
      z_scores[j] <- coeff_diff_values[j] / sqrt(sd_sum)
    }
  result = data.frame(coeff_diffs = coeff_diff_values, 
                      z_score = z_scores, 
                      rate_coeff =  rate_sum$coefficients[2,1], 
                      assist_coeff = assist_sum$coefficients[2,1])
  return(result)
}
sim_results = no_peer_simulation(1000)
# Come back to this
# Then a histogram of coefficient difference
ggplot(sim_results) + 
  geom_histogram(aes(x=coeff_diffs))
# Finally a histogram of the z-score of coefficient difference
ggplot(sim_results) + 
  geom_histogram(aes(x=z_score)) +
  geom_vline(xintercept = 1.96, color = "blue") +
  geom_vline(xintercept = -1.96, color = "blue") +
  geom_vline(xintercept = 1.645, color = "red") +
  geom_vline(xintercept = -1.645, color = "red")