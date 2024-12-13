
library(tidyverse)
library(fields)
library(ggraph)
library(tidygraph)
library(igraph)
library(Matrix)
library(purrr)
library(patchwork)
library(ivreg)
library(rdist)

#Part 1: Creating and visualizing the network
#Note: Unless stated otherwise, the basic structure of this section of code (generating the social network) came from the webpage
#https://www.r-bloggers.com/2020/07/a-social-network-simulation-in-the-tidyverse/. Also, this step will randomly generate a new
# network, so be careful about running this step.
# Create a vector with plane dimensions. The random nodes will be placed on the plane. The size of the plane and the poisson parameter
# determines the number of nodes. I recommend adjusting the poisson parameter to increase the nodes, adjusting the plane size can create
# some weird results and may mess with the memory/time to calculate the network.
set.seed(903)
plane <- c(1000, 1000)

poisson_para <- .3 * 10^(-3) # Poisson intensity parameter
# The beta parameter controls the impact of distance on a connection forming between nodes. If you lower the beta 
# distance matters more, meaning it is less likely for a connection to form between nodes that are farther away.
beta <- .45 * 10^3
# The gamma parameter controls the impact of randomly assigned weights to nodes. A lower gamma results in a lower impact of a node's 
# weight on the probability of forming connections 
gamma <- .52

# Number of nodes is Poisson(gamma)*AREA - distributed
n_nodes <- rpois(1, poisson_para * plane[1] * plane[2])
# Uniformly distributed weights. I have not played with this as a parameter, but we could discuss the role we want weights to play in 
# in generating the social network (should nodes have a wider or narrower band of weights, should they be equally weighted?).
weights <- runif(n_nodes)

# The Poisson process locally yields node positions that are completely random.
x = plane[1] * runif(n_nodes)
y = plane[2] * runif(n_nodes)


# Connection function. I have not modified this, but should we?
phi <- function(z) {
  pmin(z^(-1.8), 1)
} 
# Distance matrix needed as input. Creates a matrix from a tibble dataframe that calculates the distance between
# each point
dist_matrix <-rdist(tibble(x,y))

# Outer function creates a matrix by applying a function to every combination of data in "weights", in this case multiplication.
weight_matrix <- outer(weights, weights, FUN="*") # Weight matrix

#This takes the phi function from above and has the connection function be a function of the beta and gamma
con_matrix_prob <- phi(1/beta * weight_matrix^gamma*dist_matrix^2)# Evaluation

con_matrix <- Matrix(rbernoulli(1,con_matrix_prob), sparse=TRUE) # Sampling
con_matrix <- con_matrix * upper.tri(con_matrix) # Transform to symmetric matrix
adjacency_matrix <- con_matrix + t(con_matrix)
# Create Igraph object
graph <- graph_from_adjacency_matrix(adjacency_matrix, mode="undirected")

# Make a tidygraph object from it. Igraph methods can still be called on it.
tbl_graph <- as_tbl_graph(graph)

hub_id <- which.max(degree(graph))

# Add spacial positions, hub distance and degree information to the nodes.
tbl_graph <- tbl_graph %>%
  #activate(nodes) tells tidygraph to apply the following steps to only nodes
  activate(nodes) %>%
  mutate(
    x = x,
    y = y,
    hub_dist = replace_na(bfs_dist(root = hub_id), Inf),
    degree = degree(graph),
    friends_of_friends = replace_na(local_ave_degree(), 0),
    cluster = as.factor(group_infomap())
  )

#Adding the below code so tbl_graph can be incorporated into social_network_sim.R
sim_graph = tbl_graph
# Add coord_fixed() for fixed axis ratio!
basic <- tbl_graph %>%
  # Note that is uses ggraph instead of ggplot. Creates a dataframe from x coordinate of the point (extracted by V(.)$x) and the
  # y coordinate (extracted by V(.)$y)
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = degree, color = degree))  +
  scale_color_gradient(low = "dodgerblue2", high = "firebrick4") +
  coord_fixed() +
  guides(size = FALSE)
#To see if a network is "interesting", I usually look at the visualization and make sure there are not too many nodes that are
#isolated or too heavily connected.
basic
# This is the end of code that was explicitly taken from the webpage on generating social networks.


# Part 2: Extracting the node and edge dataframes
# Now I'm going to try to extract the node and edge data as separate dataframes in case we want to modify/summarize those
# dataframes directly.
node_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
nodes_numbers = nrow(node_data)
edge_data = tbl_graph %>%
  activate(edges) %>%
  as_tibble()
# Part 3: Assigning a random treatment to each node
# Next steps: Now we need to simulate each node receiving a "treatment", start with a random assignment of treatment In this model,We have randomly assigned 
# "treatment" status to about 5% of units in the model. This assignment is visualized in the treated ggraph object.
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(treated = rbinom(n(), 1, 0.05),
         treated_status = as.factor(treated),
         shape_type = as_factor(ifelse(treated == 1, 2, 1)))

treated = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 0.5, color = treated_status, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_manual(values = c("1" = "firebrick4",
                                "0" = "dodgerblue2"))
# We can also other characteristics to the nodes, such as gender, education, etc.
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(Male = as.factor(rbinom(n(), 1, 0.5)),
         educ = rnorm(n(), 13, 2.5))

#Part 4: Measuring how many "peers" of each observation are treated.
# Now we need to move onto to creating a variable that measures the number of treated connected nodes.
# We start by finding the number of 1st, 2nd, and 3rd order neighbors a node has. (so the neighbors it is connected to
# by at most 1 edge, then by at most 2 edges, and then by at most 3 edges)
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(neighborhood_1 = local_members(order = 1),
         neighborhood_2 = local_members(order = 2),
         neighborhood_3 = local_members(order = 3)) 


# alternative way to calculate number of peers treated
#Z = node_data$treated
#num_treated_peers_first = adjacency_matrix %*% Z
#cbind(num_treated_peers_first, node_data$first_order)


#Then the following functions find out how many of the 1st, 2nd, and 3rd order neighbors received treatment and add it 
#as a value in the node dataframe of the tidygraph.


#The following function finds the number of first order neighbors that are treated (excluding the node itself).
Treated_neighbors_first = function(tidy_graph) {
  neighbor_treat = c()
  node = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  node_rows = nrow(node)
  for (i in 1:node_rows){
    treated_count = 0
    extract = node$neighborhood_1[i]
    extracted_vector = extract[[1]]
    for (j in extracted_vector){
      if(node$treated[j] == 1) {
        treated_count = treated_count + 1
      }
    }
    neighbor_treat[i] = treated_count
  }
  tidy_graph = tidy_graph %>%
    activate(nodes) %>%
    mutate(first_order= neighbor_treat - treated)
  return(tidy_graph)
}

#Now we will do the number of 2nd order neighbors (excluding the node itself and 1st order neighbors).
Treated_neighbors_second = function(tidy_graph) {
  neighbor_treat = c()
  node = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  node_rows = nrow(node)
  for (i in 1:node_rows){
    treated_count = 0
    extract = node$neighborhood_2[i]
    extracted_vector = extract[[1]]
    for (j in extracted_vector){
      if(node$treated[j] == 1) {
        treated_count = treated_count + 1
      }
    }
    neighbor_treat[i] = treated_count
  }
  tidy_graph = tidy_graph %>%
    activate(nodes) %>%
    mutate(second_order= neighbor_treat - treated - first_order)
  return(tidy_graph)
}

#Now we will do the number of 3rd order neighbors (excluding the node itself, 1st order neighbors, and 2nd order neighbor). 
Treated_neighbors_third = function(tidy_graph) {
  neighbor_treat = c()
  node = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  node_rows = nrow(node)
  for (i in 1:node_rows){
    treated_count = 0
    extract = node$neighborhood_3[i]
    extracted_vector = extract[[1]]
    for (j in extracted_vector){
      if(node$treated[j] == 1) {
        treated_count = treated_count + 1
      }
    }
    neighbor_treat[i] = treated_count
  }
  tidy_graph = tidy_graph %>%
    activate(nodes) %>%
    mutate(third_order= neighbor_treat - treated - first_order - second_order)
  return(tidy_graph)
}

#Finally, include a self check to make sure the function works that finds the total number of treated neighbors within
# 3 degrees. This should equal the sum of the 1st, 2nd, and 3rd order columns.
Treated_neighbors_Total = function(tidy_graph) {
  neighbor_treat = c()
  node = tidy_graph %>%
    activate(nodes) %>%
    as_tibble()
  node_rows = nrow(node)
  for (i in 1:node_rows){
    treated_count = 0
    extract = node$neighborhood_3[i]
    extracted_vector = extract[[1]]
    for (j in extracted_vector){
      if(node$treated[j] == 1) {
        treated_count = treated_count + 1
      }
    }
    neighbor_treat[i] = treated_count
  }
  tidy_graph = tidy_graph %>%
    activate(nodes) %>%
    mutate(total= neighbor_treat - treated)
  return(tidy_graph)
}
#Combine them all into a single function
Treated_neighbors = function(tidy_graph) {
  first_order = Treated_neighbors_first(tidy_graph)
  second_order = Treated_neighbors_second(first_order)
  third_order = Treated_neighbors_third(second_order)
  final = Treated_neighbors_Total(third_order)
  return(final)
}
tbl_graph = Treated_neighbors(tbl_graph)
# Now we have information about peers recorded in the tidygraph object.
node_data = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
# We can also include visuals of different peers
peer_first = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 0.5, color = first_order, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high = "firebrick4", low = "dodgerblue2")
peer_second = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 0.5, color = second_order, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high = "firebrick4", low = "dodgerblue2")
peer_third = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 0.5, color = third_order, shape = shape_type))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high = "firebrick4", low = "dodgerblue2")
peer_first + peer_second + peer_third
#Part 5: Creating an outcome for each node dependent on characteristics. For now this just depends on "treated" and 
#number of "treated" peers.
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(common_treated = ifelse(treated == 1, rbinom(nodes_numbers, 1, 0.25), NA_integer_),
         outcome_no_peer = as.factor(ifelse(treated == 1, 
                                  common_treated,
                                  rbinom(nodes_numbers, 1, 0.05)))) %>%
         mutate(outcome_peer = as.factor(ifelse(treated == 1,
                               yes = common_treated,
                               no = ifelse(first_order >= 1, rbinom(nodes_numbers, 1, 0.1),rbinom(nodes_numbers, 1, 0.05))))) %>%
  select(-common_treated)
tbl_graph = tbl_graph %>%
  activate(nodes) %>%
  mutate(spillover = ifelse(treated == 1, 10, first_order*(0.99)))
# We can add a visualization of this
outcomes_no_peer = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 0.5, color = outcome_no_peer))  +
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_manual(values = c("1" = "firebrick4",
                                "0" = "dodgerblue2"))
outcomes_peer = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 0.5, color = outcome_peer))  +
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_manual(values = c("1" = "firebrick4",
                                "0" = "dodgerblue2"))
spillover = tbl_graph %>%
  ggraph(layout = tibble(V(.)$x, V(.)$y)) +
  geom_edge_link(width = .1) +
  geom_node_point(aes(size = 0.5, color = spillover))  +
  scale_size_continuous(range = c(0.5, 2.5))+
  coord_fixed() +
  guides(size = FALSE) +
  scale_color_gradient(high= "firebrick4", low = "dodgerblue2")
#Now we can save the results.
save(tbl_graph, file = "C://Users/jacob/Dropbox/Wellness Study/Simulation Scripts/soc_net.Rdata")


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


