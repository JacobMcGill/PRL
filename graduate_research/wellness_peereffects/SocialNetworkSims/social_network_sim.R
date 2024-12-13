#Note: Be sure to run code from social_network_gen.R before running this code
# Part 7: Simulating the regressions multiple times. Will need to run a for loop that generates a treatment vector, then 
# generate outcomes based off that treatment vector, run a regression on those outcomes. 
# Challenges: Biggest one is that in our current approach, the marginal effect varies in simulations. Since we are just
# trying to identify peer effects, I think the best approach is to "fix" the marginal effect of having a treated peer. 
#Psuedo-code
# for (i in 1:number of simulations) {
# generate treatment vector
# generate outcomes based off treatment vector
# Run regressions
# Extract estimated coefficients from regressions and store them in a dataframe
# Graph a histogram of coefficients generated from the loop along with a line indicating the true value and the 95% CI
# }
# Work on code
# Notes to self: Adding mean to graphs of results as a vertical line and testing
# how many standard deviations away from actual effect the estimated effect is. Left off at
# the continous process.
sim_graph = sim_graph %>%
  activate(nodes) %>%
  group_by(cluster) %>%
  mutate(group_shock = rnorm(1, 5, 2.5)) %>%
  ungroup()

thresh_test = function(tidy_graph, num_sims, treat_prob){
  set.seed(7292024)
  node_frame = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  nodes_numbers = nrow(node_frame)
  est_coeff = c()
  for (i in 1:num_sims) {
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(treated = rbinom(n(), 1, treat_prob),
             treated_status = as.factor(treated))
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(neighborhood_1 = local_members(order = 1),
             neighborhood_2 = local_members(order = 2),
             neighborhood_3 = local_members(order = 3)) 
    tidy_graph = Treated_neighbors(tidy_graph)
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(peer_first = ifelse(first_order >= 1 & treated == 0, 1, 0),
             outcome_peer = ifelse(treated == 1, rnorm(nodes_numbers, 10, 0.5), 
                                   ifelse(first_order >= 1, rnorm(nodes_numbers, 7.5, 0.5), rnorm(nodes_numbers, 5, 0.5))))
    dgp = tidy_graph %>%
      activate(nodes) %>%
      as_tibble()
    est_coeff[i] = summary(lm(outcome_peer ~ treated + peer_first, dgp))$coefficients[3,1]
  }
  coeff = data.frame(
    est_coeff = est_coeff
  )
  return(coeff)
} 
coeff = thresh_test(sim_graph, 1000, 0.05)

result_graph = ggplot(coeff) +
  geom_histogram(aes(x = est_coeff)) +
  geom_vline(xintercept = 2.5, color = "blue") +
  geom_vline(xintercept = quantile(coeff$est_coeff, c(0.05, 0.95))[1], color = "red") +
  geom_vline(xintercept = quantile(coeff$est_coeff, c(0.05, 0.95))[2], color = "red") +
  geom_vline(xintercept = mean(coeff$est_coeff), color = "purple")
result_graph

thresh_est_eff = mean(coeff$est_coeff)
thresh_est_sd = (var(coeff$est_coeff))^0.5
z_score = (thresh_est_eff - 2.5)/thresh_est_sd
thresh_bias = thresh_est_eff - 2.5

# Now we will move onto a continous DGP

con_test = function(tidy_graph, num_sims, treat_prob){
  set.seed(42029270)
  node_frame = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  nodes_numbers = nrow(node_frame)
  first_coeff = c()
  second_coeff = c()
  third_coeff = c()
  
  for (i in 1:num_sims) {
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(treated = rbinom(n(), 1, treat_prob),
             treated_status = as.factor(treated))
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(neighborhood_1 = local_members(order = 1),
             neighborhood_2 = local_members(order = 2),
             neighborhood_3 = local_members(order = 3))
    tidy_graph = Treated_neighbors(tidy_graph)
    #Trying something out; adding this to test how effective linear in-mean is
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(outcome_all_peers = rnorm(nodes_numbers, 5 + 5*treated + 3.5*(first_order) + 0.75*(second_order) + 0.25*(third_order), 0.5))
    dgp = tidy_graph %>%
      activate(nodes) %>%
      as_tibble()
    first_coeff[i] = summary(lm(outcome_all_peers ~ treated + first_order + second_order + third_order, dgp))$coefficients[3,1]
    second_coeff[i] = summary(lm(outcome_all_peers ~ treated + first_order + second_order + third_order, dgp))$coefficients[4,1]
    third_coeff[i] = summary(lm(outcome_all_peers ~ treated + first_order + second_order + third_order, dgp))$coefficients[5,1]

  }
  coeff = data.frame(
    first_coeff = first_coeff,
    second_coeff = second_coeff,
    third_coeff = third_coeff
  )
  return(coeff)
} 
# Should we also try to asses if the estimate of "treated" is biased as well?
test = con_test(sim_graph, 5000, 0.05)
test_1 = con_test(sim_graph, 50, 0.05)
result_graph = ggplot(test) +
  geom_histogram(aes(x = first_coeff)) +
  geom_vline(xintercept = 3.5, color = "blue") +
  geom_vline(xintercept = quantile(test$first_coeff,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(test$first_coeff,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(test$first_coeff) , color = "purple")

result_graph

first_est_eff = mean(test$first_coeff)
first_est_sd = (var(test$first_coeff))^0.5
hyp_test = (first_est_eff - 3.5)/first_est_sd
first_bias = first_est_eff - 3.5
node_frame = sim_graph %>%
  activate(nodes) %>%
  as_tibble()


result_graph_second = ggplot(test) +
  geom_histogram(aes(x = second_coeff)) +
  geom_vline(xintercept = .75, color = "blue") +
  geom_vline(xintercept = quantile(test$second_coeff,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(test$second_coeff,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(test$second_coeff) , color = "purple")
result_graph_second
second_est_eff = mean(test$second_coeff)
second_est_sd = (var(test$second_coeff))^0.5
second_hyp_test = (second_est_eff - .75)/second_est_sd
second_bias = second_est_eff - 0.75


result_graph_third = ggplot(test) +
  geom_histogram(aes(x = third_coeff)) +
  geom_vline(xintercept = .25, color = "blue") +
  geom_vline(xintercept = quantile(test$third_coeff,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(test$third_coeff,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(test$third_coeff) , color = "purple")
result_graph_third
third_est_eff = mean(test$third_coeff)
third_est_sd = (var(test$third_coeff))^0.5
third_hyp_test = (third_est_eff - .25)/third_est_sd
third_bias = third_est_eff - 0.25

# Now we will move onto the group rate approach. Be careful with this one, implementing it seems to be causing
# some issues with the other ones.

group_test = function(tidy_graph, num_sims, treat_prob){
  set.seed(2912024)
  node_frame = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  nodes_numbers = nrow(node_frame)
  rate_coeff = c()
  for (i in 1:num_sims) {
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(treated = rbinom(n(), 1, treat_prob),
             treated_status = as.factor(treated))
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      group_by(cluster) %>%
      mutate(mean_treat = mean(treated)) %>%
      ungroup()
    tidy_graph = tidy_graph %>%
      activate(nodes)  %>%
      mutate(cluster_peer_outcome = ifelse(treated == 1, 
                                           rnorm(nodes_numbers, 10, 0.5), 
                                           rnorm(nodes_numbers, 
                                                 5 + 4*(mean_treat), 0.5)))
    dgp = tidy_graph %>%
      activate(nodes) %>%
      as_tibble()
    rate_coeff[i] = summary(lm(cluster_peer_outcome ~ treated + mean_treat, dgp))$coefficients[3,1]
  }
  coeff = data.frame(
    rate_coeff = rate_coeff
  )
  return(coeff)
} 
rate_df = group_test(sim_graph, 5000, 0.05)
rate_result_graph = ggplot(rate_df) +
  geom_histogram(aes(x = rate_coeff)) +
  geom_vline(xintercept = 4, color = "blue") +
  geom_vline(xintercept = quantile(rate_df$rate_coeff,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(rate_df$rate_coeff,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(rate_df$rate_coeff), color = "purple")
rate_est_eff = mean(rate_df$rate_coeff)
rate_est_sd = (var(rate_df$rate_coeff))^0.5
rate_test_stat = (rate_est_eff - 4)/rate_est_sd
rate_bias = rate_est_eff - 4
rate_per_bias = (rate_bias/4)*100
rate_stand_bias = (rate_bias/rate_est_sd)*100
rate_mse = (rate_bias)^2 +(rate_est_sd)^2

# Group shock: Lets see if it can imitate peer effects 
shock_test = function(tidy_graph, num_sims, treat_prob){
  set.seed(2912024)
  node_frame = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  nodes_numbers = nrow(node_frame)
  first_coeff = c()
  second_coeff = c()
  third_coeff = c()
  rate_coeff = c()
  for (i in 1:num_sims) {
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(treated = rbinom(n(), 1, treat_prob),
             treated_status = as.factor(treated))
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(neighborhood_1 = local_members(order = 1),
             neighborhood_2 = local_members(order = 2),
             neighborhood_3 = local_members(order = 3))
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      group_by(cluster) %>%
      mutate(shock = rnorm(1, 5, 1),
             rate_treat = mean(treated)) %>%
      ungroup()
    tidy_graph = tidy_graph %>%
      activate(nodes)  %>%
      mutate(outcome_all_peers = rnorm(nodes_numbers, 5 + 5*treated + 0.5*shock, 0.5))
    dgp = tidy_graph %>%
      activate(nodes) %>%
      as_tibble()
    reg = summary(lm(outcome_all_peers ~ treated + first_order + second_order + third_order, dgp))
    mean_reg = summary(lm(outcome_all_peers ~ treated + rate_treat, dgp))
    first_coeff[i] = reg$coefficients[3,1]
    second_coeff[i] = reg$coefficients[4,1]
    third_coeff[i] = reg$coefficients[5,1]
    rate_coeff[i] = mean_reg$coefficient[3,1]
  }
  coeff = data.frame(
    rate_coeff = rate_coeff,
    first_coeff = first_coeff,
    second_coeff = second_coeff,
    third_coeff = third_coeff
  )
  return(coeff)
} 
shock_test(sim_graph, 10, 0.05)

#Social networks exist but we cannot observe them but we can observe group membership.
# Way to get around the group_by issue: use 2SLS, instrumenting by group membership
con_group_test = function(tidy_graph, num_sims, treat_prob){
  set.seed(42029270)
  node_frame = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  nodes_numbers = nrow(node_frame)
  first_coeff = c()
  second_coeff = c()
  third_coeff = c()
  OLS_coeff = c()
  SLS_coeff = c()
  peer_effect = c()
  r_sq = c()
  ATE = c()
  rate_coeff = c()
  
  for (i in 1:num_sims) {
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(treated = rbinom(n(), 1, treat_prob),
             treated_status = as.factor(treated))
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(neighborhood_1 = local_members(order = 1),
             neighborhood_2 = local_members(order = 2),
             neighborhood_3 = local_members(order = 3))
    tidy_graph = Treated_neighbors(tidy_graph)
    tidy_graph = tidy_graph %>%
      group_by(cluster) %>%
      mutate(rate_treated = mean(treated)) %>%
      ungroup()
    #Trying something out; adding this to test how effective linear in-mean is
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(outcome_all = rnorm(nodes_numbers, 5 + 5*treated + 3.5*(first_order) + 0.75*(second_order) + 0.25*(third_order), 0.5),
             act_peer_effect = 3.5*(first_order) + 0.75*(second_order) + 0.25*(third_order))
    dgp = tidy_graph %>%
      activate(nodes) %>%
      as_tibble()
    OLS = summary(lm(outcome_all ~ treated, data = dgp))
    SLS = summary(ivreg(outcome_all ~ treated|cluster, data = dgp))
    rate_reg = summary(lm(outcome_all ~ treated + rate_treated, data = dgp))
    True = summary(lm(outcome_all ~ treated + first_order + second_order + third_order, data = dgp))
    r_sq = summary(lm(treated ~ cluster, data = dgp))$r.squared
    diff = SLS$coefficients[2,1] - OLS$coefficients[2,1]
    OLS_coeff[i] = OLS$coefficients[2,1]
    SLS_coeff[i] = SLS$coefficients[2,1]
    peer_effect[i] = (diff)*(1/(1 - r_sq)) 
    first_coeff[i] = True$coefficients[3,1]
    second_coeff[i] = True$coefficients[4,1]
    third_coeff[i] = True$coefficients[5,1]
    ATE[i] = mean(dgp$act_peer_effect)
    rate_coeff[i] = rate_reg$coefficients[3,1]
  }
  coeff = data.frame(
    OLS_coeff = OLS_coeff,
    SLS_coeff = SLS_coeff,
    first_coeff = first_coeff,
    second_coeff = second_coeff,
    third_coeff = third_coeff,
    ATE = ATE,
    peer_effect = peer_effect,
    rate_coeff = rate_coeff
  )
  return(coeff)
} 

# Lets look at the 2SLS and OLS difference
testwork = con_group_test(sim_graph, 500, 0.05)
test_result_graph = ggplot(testwork) +
  geom_histogram(aes(x = SLS_coeff)) +
  geom_vline(xintercept = quantile(testwork$SLS_coeff,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(testwork$SLS_coeff,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(testwork$SLS_coeff), color = "blue") +
  geom_vline(xintercept = mean(testwork$ATE), color = "purple")
# Lets look at the peer effect calculated through Angrist's method
test_result_graph = ggplot(testwork) +
  geom_histogram(aes(x = peer_effect)) +
  geom_vline(xintercept = quantile(testwork$peer_effect,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(testwork$peer_effect,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(testwork$peer_effect), color = "blue") +
  geom_vline(xintercept = mean(testwork$ATE), color = "purple")
# Lets look at the mean coefficient
test_result_graph_mean = ggplot(testwork) +
  geom_histogram(aes(x = rate_coeff)) +
  geom_vline(xintercept = quantile(testwork$rate_coeff,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(testwork$rate_coeff,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(testwork$rate_coeff), color = "blue") +
  geom_vline(xintercept = mean(testwork$ATE), color = "purple")
# In this situation, using the equation recommended by Angrist (finding the difference between OLS and 2SLS)
# overestimates peer effects by a significant margin


# Now lets try adding some non-treatment covariates
covar_test = function(tidy_graph, num_sims, treat_prob){
  set.seed(42029270)
  node_frame = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  nodes_numbers = nrow(node_frame)
  first_coeff = c()
  second_coeff = c()
  third_coeff = c()
  first_noise_coeff = c()
  second_noise_coeff = c()
  third_noise_coeff = c()
  first_coeff_diff = c()
  second_coeff_diff = c()
  third_coeff_diff = c()
  
  for (i in 1:num_sims) {
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(treated = rbinom(n(), 1, treat_prob),
             treated_status = as.factor(treated))
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(neighborhood_1 = local_members(order = 1),
             neighborhood_2 = local_members(order = 2),
             neighborhood_3 = local_members(order = 3))
    tidy_graph = Treated_neighbors(tidy_graph)
    tidy_graph = tidy_graph %>%
      activate(nodes) %>%
      mutate(educ = rnorm(nodes_numbers, 5, 1),
             noise = rnorm(nodes_numbers, 0, 1.5),
             ability = rnorm(nodes_numbers, 5 + 5*treated + 3.5*(first_order) + 0.75*(second_order) + 0.25*(third_order), 0.5),
             outcome = educ + ability,
             outcome_noise = educ + ability + noise)
    dgp = tidy_graph %>%
      activate(nodes) %>%
      as_tibble()
    reg_sum = summary(lm(outcome ~ treated + first_order + second_order + third_order, dgp))
    reg_noise_sum = summary(lm(outcome_noise ~ treated + first_order + second_order + third_order, dgp))
    first_coeff[i] = reg_sum$coefficients[3,1]
    second_coeff[i] = reg_sum$coefficients[4,1]
    third_coeff[i] = reg_sum$coefficients[5,1]
    first_noise_coeff[i] = reg_noise_sum$coefficients[3,1]
    second_noise_coeff[i] = reg_noise_sum$coefficients[4,1]
    third_noise_coeff[i] = reg_noise_sum$coefficients[5,1]
    first_coeff_diff[i] = reg_noise_sum$coefficients[3,1] - reg_sum$coefficients[3,1]
    second_coeff_diff[i] = reg_noise_sum$coefficients[4,1] - reg_sum$coefficients[4,1]
    third_coeff_diff[i] = reg_noise_sum$coefficients[5,1] - reg_sum$coefficients[5,1]
    
  }
  coeff = data.frame(
    first_coeff = first_coeff,
    second_coeff = second_coeff,
    third_coeff = third_coeff,
    first_noise_coeff = first_noise_coeff,
    second_noise_coeff = second_noise_coeff,
    third_noise_coeff = third_noise_coeff,
    first_coeff_diff = first_coeff_diff,
    second_coeff_diff = second_coeff_diff,
    third_coeff_diff = third_coeff_diff
  )
  return(coeff)
} 
noise_test = covar_test(sim_graph, 1000, 0.05)
noise_graph_mean = ggplot(noise_test) +
  geom_histogram(aes(x = first_coeff_diff)) +
  geom_vline(xintercept = quantile(noise_test$first_coeff_diff,c(0.05, 0.95))[1] , color = "red") +
  geom_vline(xintercept = quantile(noise_test$first_coeff_diff,c(0.05, 0.95))[2] , color = "red") +
  geom_vline(xintercept = mean(noise_test$first_coeff_diff), color = "purple")
noise_graph_mean


#Review ReadMe file of conditional randomization test to start running it 
# Also: Find a new set of graduate students. MA Econ program, talk to Stephanie for the posting
# Will probably be 2, but just say we're looking for a batch
