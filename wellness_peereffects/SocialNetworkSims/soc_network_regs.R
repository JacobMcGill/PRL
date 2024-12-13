#Linear Regressions
# next steps
## (1) generate outcome Y | Z,X,peers, e.g., Y ~ normal_1 if Z=1,peers>threshold and Y ~normal_2 if Z=0 peers<=threshold.
## (2) create a data set, similar to "node_data", where one of the columns is Y, with X, Z, peers, ....
## (3) fit regression models with lm() function

# Questions for David: Have a couple different data generating process:
# 1. Threshold: The distribution sampled from depends on treated status and if the # of treated peers is above a certain threshold
# 2. Continuous: The mean of the distribution sampled for non-treated individuals is a function of the number of treated peers.
# 3. Group Spillover: The mean of the distribution sampled for non-treated individuals is a function of the rate of treated peers in a 
# cluster
# 4. Group shocks: I also added cluster specific shocks just to see if they imitated peer effects.


# Part 6: Data Generating Process: Next, I will move onto generating data from these nodes. I will start with a simple approach,
# where we have no peer effects and then peer effects triggered by having at least 1 first order treated peer.
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(peer_first = ifelse(first_order >= 1 & treated == 0, 1, 0),
         outcome_no_peer = ifelse(treated == 1, rnorm(nodes_numbers, 10, 0.5), rnorm(nodes_numbers, 5, 0.5)),
         outcome_peer = ifelse(treated == 1, rnorm(nodes_numbers, 10, 0.5), 
                               ifelse(first_order >= 1, rnorm(nodes_numbers, 7.5, 0.5), rnorm(nodes_numbers, 5, 0.5))))
dgp_test = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()

# In this example, we have a no peer DGP and a DGP where the presence of at at least 1 first order peer increases
# the mean of the untreated sample distribution by 2.5. The regression of outcome on treated and first order
# peer produces the following results:

summary(lm(outcome_no_peer ~ treated + first_order, dgp_test))
summary(lm(outcome_peer ~ treated + first_order, dgp_test))
summary(lm(outcome_peer ~ treated + peer_first, dgp_test))

# As we can see, the linear regression identifies the presence of peer effects when they exist. We can expand this data generating process by increasing the 
# threshold for peer effects to "kick in" and having different thresholds have different effects. In this case, the thresholds are 
# determined by a fraction of maximum number of first order treated peers (for example, the greatest peer effect is for individuals
# with treated peers equal to 3/4 or more of the maximum number of treated peers).
# We can also visualize this. Nodes that are triangles are treated, circle nodes are untreated.

threshold_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = outcome_peer, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")


max_tbl = node_data %>%
  summarize(first_peer = max(first_order),
            second_peer = max(second_order),
            third_peer = max(second_order))
max_peer_first = max_tbl$first_peer
max_peer_second = max_tbl$second_peer
max_peer_third = max_tbl$third_peer
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(outcome_no_peer = ifelse(treated == 1, rnorm(nodes_numbers, 10, 0.5), rnorm(nodes_numbers, 5, 0.5)),
         first_indicator = ifelse(first_order >= max_peer_first/4 & treated == 0, 1, 0),
         second_indicator = ifelse(first_order >= max_peer_first/2 & treated == 0, 1, 0),
         third_indicator = ifelse(first_order >= 3*max_peer_first/4 & treated == 0, 1, 0),
         outcome_peer_thresh = ifelse(treated == 1, rnorm(nodes_numbers, 10, 0.5), 
                                      ifelse(first_order >= 3*max_peer_first/4, 
                                             rnorm(nodes_numbers, 8.75, 0.5),  
                                             ifelse(first_order >= max_peer_first/2, rnorm(nodes_numbers, 7.5, 0.5),
                                                    ifelse(first_order >= max_peer_first/4, rnorm(nodes_numbers, 6.25, 0.5), 
                                                           rnorm(nodes_numbers, 5, 0.5))))))


# In this DGP, I estimate the effect of first order peers by finding max_peer_first/4,
# the length of the interval we see the jumps in the mean of the distribution, then 
# dividing 1.25 by max_peer_first/4.

dgp_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
summary(lm(outcome_no_peer ~ treated + first_order, dgp_data))
summary(lm(outcome_peer_thresh ~ treated + first_order, dgp_data))
summary(lm(outcome_peer_thresh ~ treated + first_indicator + second_indicator + third_indicator, dgp_data))

first_thresh = summary(lm(outcome_peer_thresh ~ treated + first_indicator + second_indicator + third_indicator, dgp_data))$coefficients[3,1]
second_thresh = summary(lm(outcome_peer_thresh ~ treated + first_indicator + second_indicator + third_indicator, dgp_data))$coefficients[4,1]
third_thresh = summary(lm(outcome_peer_thresh ~ treated + first_indicator + second_indicator + third_indicator, dgp_data))$coefficients[5,1]
thresh_results = matrix(c(1.5, 
                          first_thresh,
                          second_thresh,
                          third_thresh),
                        nrow = 1,
                        ncol = 4,
                        byrow = TRUE)
rownames(thresh_results) = (c("Effect on Outcome"))
colnames(thresh_results) = c("Actual", "First Threshold", "Second Threshold", "Third Threshold")


# Let's visualize this.  


thresh_max_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = outcome_peer_thresh, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")


# I am next going to work in the 2SLS and OLS difference method described by Angrist to test the effectiveness of that at identifying
# peer effects. Group membership will be the clusters assigned by R during the social network generation.

no_peer_OLS = summary(lm(outcome_no_peer ~ treated, dgp_data))
no_peer_IV = summary(ivreg(outcome_no_peer ~ treated|cluster, data = dgp_data))
r_sq = summary(lm(treated ~ cluster, dgp_data))$r.squared

#Now I'm going to test the size of the difference for the regressions for the no peer DGP

no_peer_diff = (no_peer_IV$coefficients[2,1] - no_peer_OLS$coefficients[2,1])
no_peer_sd = (no_peer_IV$coefficients[2,2]^2 + no_peer_OLS$coefficients[2,2]^2)
no_peer_z = (no_peer_diff)/(no_peer_sd)^(0.5)
no_peer_z

# Now for the threshold DGP and the size of the peer effect estimated by the regressions. The equation for estimating the size of 
# peer effects comes from equation 17 of the Angrist paper

thresh_OLS = summary(lm(outcome_peer_thresh ~ treated, dgp_data))
thresh_IV = summary(ivreg(outcome_peer_thresh ~ treated|cluster, data = dgp_data))
thresh_diff = (thresh_IV$coefficients[2,1] - thresh_OLS$coefficients[2,1])
thresh_sd = (thresh_IV$coefficients[2,2]^2 + thresh_OLS$coefficients[2,2]^2)
thresh_z = (thresh_diff)/(thresh_sd)^(0.5)
thresh_z
thresh_eff = (1/(1 - r_sq))*thresh_diff
thresh_eff

# Next, I am going to move a bit more continuous data generating
# process. In this one, the mean of the distribution the outcome variable for untreated observations depends on the 
# number of treated peers. I will start with a simple model where only first order peers affect the outcome:

tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(outcome_peer_con = rnorm(nodes_numbers, 5 + 5*treated + 4.5*(first_order/max_peer_first), 0.5))
dgp_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
peer_effect = 4.5/max_peer_first

# We can also visualize this.

dgp_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = outcome_peer_con, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")
dgp_graph


# In the above data generating process, the mean for the non-treated nodes is a function of the number of first order neighbors they
# have, first_order. To ensure that non-treated nodes have a mean less than treated nodes, first_order is divided by max_peers_first,
# the largest amount of first order peers a node has in the data. The mean effect of first order peers should then be 4.5/max_peer_first.

con_reg = summary(lm(outcome_peer_con ~ treated + first_order, data = dgp_data))
con_OLS = summary(lm(outcome_peer_con ~ treated, data = dgp_data))
con_IV = summary(ivreg(outcome_peer_con ~ treated|cluster, data = dgp_data))
peer_effect

# Now let's test the difference and find the estimated peer effect using Angrist's method

con_diff = (con_IV$coefficients[2,1] - con_OLS$coefficients[2,1])
con_sd = (con_IV$coefficients[2,2]^2 + con_OLS$coefficients[2,2]^2)
con_z = (con_diff)/(con_sd)^(0.5)
con_z
(1/(1-r_sq))*con_diff

# Here, the linear regression estimates a coefficient for first order that is close to the actual value although in
# some previous simulations, the actual effect of peer effects is outside of the 95% CI, so we are already
# starting to see some issues associated with using linear regressions to estimate spillover.


# Next, we will expand the data generating process to include 2nd and 3rd degree neighbors, using a similar approach when we just had first degree neighbors.
# We will also include some visualizations.

max_tbl = node_data %>%
  summarize(first_peer = max(first_order),
            second_peer = max(second_order),
            third_peer = max(third_order))
max_peer_first = max_tbl$first_peer
max_peer_second = max_tbl$second_peer
max_peer_third = max_tbl$third_peer
tbl_graph = tbl_graph %>%
  mutate(outcome_all_peers = rnorm(nodes_numbers, 5 + 5*treated + 3.5*(first_order/max_peer_first) + 0.75*(second_order/max_peer_second) + 0.25*(third_order/max_peer_third), 0.5))
peer_effects = c((3.5)/(max_peer_first), (0.75)/(max_peer_second), (.25)/(max_peer_third))

dgp_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()

degree_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = outcome_all_peers, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")

degree_graph

peer_con = summary(lm(outcome_all_peers ~ treated + first_order + second_order + third_order, dgp_data))


con_results = matrix(c((3.5)/(max_peer_first), 
                       (0.75)/(max_peer_second), 
                       (.25)/(max_peer_third), 
                       peer_con$coefficients[3,1], 
                       peer_con$coefficients[4,1], 
                       peer_con$coefficients[5,1]),
                     nrow = 2,
                     ncol = 3,
                     byrow = TRUE)

colnames(con_results) = c( "First Order", 
                           "Second Order", 
                           "Third Order")

rownames(con_results) = c("Actual Effect", "Estimated Effect")

con_results


# Now, I am going to do a slightly different data generating process. As part of creating the social network, nodes were grouped
# into clusters by an R algorithm. I am going to use those clusters to represent group membership and have the untreated outcomes
# depend on the rate of treated nodes in a cluster

tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  group_by(cluster) %>%
  mutate(mean_treat = mean(treated))
tbl_graph = tbl_graph %>%
  activate(nodes)  %>%
  mutate(cluster_peer_outcome = ifelse(treated == 1, 
                                       rnorm(nodes_numbers, 10, 0.5), 
                                       rnorm(nodes_numbers, 
                                             5 + 4*(mean_treat), 0.5)))
dgp_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
summary(lm(outcome_no_peer ~ treated + mean_treat, dgp_data))
summary(lm(cluster_peer_outcome ~ treated + mean_treat, dgp_data))
summary(lm(cluster_peer_outcome ~ treated + first_order + second_order + third_order, dgp_data))

# This gives us some interesting results (and counter intuitive) results. 
# Also, when peer effects do not exist, this linear regression does identify a statistically significant effect.
# Lets add a visualization. First, we can see the clusters by color. 

cluster_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, shape = shape_type, color = cluster))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) 
cluster_graph

# Now we can color code by rate of treated nodes in a cluster

rate_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = mean_treat, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")
rate_graph

# Finally, the outcomes

rate_outcome_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = cluster_peer_outcome, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")
rate_outcome_graph

# Again, we'll add the 2SLS vs OLS approach

mean_OLS = summary(lm(cluster_peer_outcome ~ treated, dgp_data))
mean_IV = summary(ivreg(cluster_peer_outcome ~ treated|cluster, data = dgp_data))

# Let's find the difference

mean_diff = (mean_IV$coefficients[2,1] - mean_OLS$coefficients[2,1])
mean_sd = (mean_IV$coefficients[2,2]^2 + mean_OLS$coefficients[2,2]^2)
mean_z = (mean_diff)/(mean_sd)^(0.5)
mean_z
est_mean = (1/(1-r_sq))*mean_diff

# This estimate effect is close to the true effect of the rate
# Next, we are going to add cluster specific shocks.

tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  group_by(cluster) %>%
  mutate(cluster_shock = rnorm(n(), 2, 0.5))
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(outcome_cluster = outcome_no_peer + cluster_shock)
dgp_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
summary(lm(outcome_cluster ~ treated + first_order + second_order + third_order, dgp_data))
summary(lm(outcome_cluster ~ treated + mean_treat, dgp_data))

# OLS is finding no example of peer effects
# We can also visualize this, along with a graph of the clusters

cluster_graph_no_legend = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, shape = shape_type, color = cluster))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() + 
  theme(legend.position = "false") +
  guides(size = FALSE) 
shock_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = cluster_shock, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")
shock_outcome_graph = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 1, color = outcome_cluster, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")

# Here, we have the cluster graph, shock graph, and shock outcome graph side by side

cluster_graph + shock_graph + shock_outcome_graph

# Lets add measurement error for number of treated peers. For all orders of peer, the 
# the error will be mean 0 but we will increase the standard deviation of the noise.
# This could be useful for simulating self reported social networks (such as coworkers
# reporting people who they have worked with on teams). for generating noise, I randomly
# generated numbers using a normal distribution then rounding off the digits. Noise was
# then calculated by adding the noise to first, second, and third order measures of 
# treated peers, with the value set by 0 if the resulting sum was less then 0. This was
# done to avoid negative values for treated peers.
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(first_noise = round(rnorm(n(), 0, 1), digits = 0),
         second_noise = round(rnorm(n(), 0, 4), digits = 0),
         third_noise = round(rnorm(n(), 0, 20), digits = 0),
         first_order_noise = ifelse(first_order + first_noise > 0, 
                                    first_order + first_noise,
                                    0),
         second_order_noise = ifelse(second_order + second_noise > 0,
                                     second_order + second_noise,
                                     0),
         third_order_noise = ifelse(third_order + third_noise > 0,
                                    third_order + third_noise,
                                    0)
  )
dgp_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
Error_reg = summary(lm(outcome_all_peers ~ treated + first_order_noise + second_order_noise + third_order_noise, 
                       data = dgp_data))
measurement_error_comp = matrix(c(peer_con$coefficients[3,1],
                                  peer_con$coefficients[4,1],
                                  peer_con$coefficients[5,1],
                                  Error_reg$coefficients[3,1],
                                  Error_reg$coefficients[4,1],
                                  Error_reg$coefficients[5,1]),
                                nrow = 3,
                                ncol = 2)
colnames(measurement_error_comp) = c("No Measurement Error", "Measurement Error")
rownames(measurement_error_comp) = c("First Order", "Second Order", "Third Order")
# Comparing the estimated coefficients between no error and error shows that measurement
# error attenuates the estimated effect of treated peers, although in the case of the third order, the estimate is 
# negatively biased instead of being simply attenuated.
measurement_error_comp






# The regressions appears to be robust to these issues when we include first order connections. Lets also find peer effects by 
# calculating the difference between 2SLS and OLS (the method discussed in Angrist's paper)
cluster_OLS = summary(lm(outcome_cluster ~ treated, dgp_data))
cluster_IV = summary(ivreg(outcome_cluster~treated|cluster, data = dgp_data))
# Testing the difference between the two
cluster_diff = (cluster_IV$coefficients[2,1] - cluster_OLS$coefficients[2,1])
cluster_sd = (cluster_IV$coefficients[2,2]^2 + cluster_OLS$coefficients[2,2]^2)
cluster_z = (cluster_diff)/(cluster_sd)^(0.5)
cluster_z
