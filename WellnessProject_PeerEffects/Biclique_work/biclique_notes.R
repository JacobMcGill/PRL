install.packages("devtools")
install.packages("vctrs")
library(devtools)
install_github("dpuelz/BicliqueRT")
library(BicliqueRT)
biclique.decompose(Z,
                   #Z is observed treatment assignment vector with length N (number of units)
                   hypothesis, 
                   controls=list(method="greedy", mina=10, num_randomizations=2000), 
                   stop_Zobs=F)
# Hypothesis is a list with 3 functions: 
#     design_fn: Experiment design, function that returns a realization of treatment assignment for sample
#     It can also depend on other global variables, like the number of units.
#     Ex: design_fn = function() { rbinom(num_units, 1, prob=0.2) }
#     Sets up design s.t. each unit has a 0.2 prob of receiving treatment
#     exposure_i: function that returns exposure f_i(z) of unit i under treatment z. Inputs are index i and
#     vector z(ex: exposure_i = function(z, i) { z[i] }, in this case exposure being treatment)
#     null_equiv: Equivalence btw 2 exposures, function that takes 2 inputs from exposure i and determines
#     if they are equivalent.
# Controls is a list that specifies 2 parameters for biclique decomposition: 
#   method: Needs to be either "bimax" (requires min number of units and assignments (minr and minc) or
#   "greedy", requires "mina"
#   num_randomizations: Number of randomizations to perform (larger takes more time but also gives more 
#   power)
#stop_Zobs: True or False, tells the function to stop decomposition when you find a biclique that contains
#         the observed treatment assignment vector. Can speed up test at cost of not getting the whole
#         biclique decomposition
# Take results of biclique.decompose, as biclique_decom and pass to 
# clique_test(Y, Z, teststat, biclique_decom, alpha=0.05, one_sided=T) for randomizations test
# teststat: function, specifies test state to be used, has inputs y (outcome vector), z (treatment
# vector), and focal_unit_indicator, a 0-1 vector indicating if a unit is focal (1) or not (0).
# Test run
# generated network - 3 clusters of 2D Gaussians
# loads in the 500x500 matrix Dmat
# Dmat just encodes all pairwise Euclidean distances between network nodes, and
# this is used to define the spillover hypothesis below.

library(BicliqueRT)
set.seed(1)

N = 500 # number of units
thenetwork = out_example_network(N)
D = thenetwork$D

# simulating an outcome vector and a treatment realization
Y = rnorm(N)
Z = rbinom(N, 1, prob=0.2)

# simulation parameters
num_randomizations = 5000
radius = 0.02

# To use the package:
#   1. The design function: here the experimental design is Bernoulli with prob=0.2
design_fn = function() { rbinom(N, 1, prob=0.2) }

#   2. The exposure function: exposure for each unit is (w_i, z_i) where
#       w_i = 1{\sum_{j\neq i} g_{ij}^r z_j > 0 } and g_{ij}^r = 1{d(i,j)<r}
Gr = (D<radius) * 1; diag(Gr) = 0
exposure_i = function(z, i) { c(as.numeric(sum(Gr[i,]*z) > 0), z[i]) }

#   3. The null
null_hypothesis = list(c(0,0), c(1,0))
null_equiv = function(exposure_z1, exposure_z2) {
  (list(exposure_z1) %in% null_hypothesis) & (list(exposure_z2) %in% null_hypothesis)
}

# Then we can decompose the null exposure graph:
H0 = list(design_fn=design_fn, exposure_i=exposure_i, null_equiv=null_equiv)
bd = biclique.decompose(Z, H0, controls= list(method="greedy", mina=20, num_randomizations = 2e3))
m = bd$MNE # this gives the biclique decomposition

# To do randomization test, firstly generate a test statistic. Here we use the absolute value of differences in means between units with exposure (0,0) and exposure (1,0)
Tstat = function(y, z, is_focal) {
  exposures = rep(0, N)
  for (unit in 1:N) {
    exposures[unit] = exposure_i(z, unit)[1]
  }
  stopifnot("all focals have same exposures" = (length(unique(exposures[is_focal]))>1) )
  abs(mean(y[is_focal & (exposures == 1)]) - mean(y[is_focal & (exposures == 0)]))
}

# Then run the test
testout = clique_test(Y, Z, Tstat, bd)

# Clustered interference

library(BicliqueRT)
set.seed(1)
N = 2000 # total number of units
K = 500  # total number of households, i.e., number of clusters
housestruct = out_house_structure(N, K, T)

# The design function:
design_fn = function(){
  treatment_list = out_treat_household(housestruct, K1 = K/2) # one unit from half of the households would be treated.
  treatment = out_Z_household(N, K, treatment_list, housestruct)
  return(treatment[,'treat'])
}

# The exposure function: exposure for each unit i is z_i + \sum_{j \in [i]} z_j where [i] represents the cluster i is in.
exposure_i = function(z, i) {
  # find the household that i is in
  house_ind = cumsum(housestruct)
  which_house_i = which.min(house_ind < i)
  # find lower and upper index of [i] in z
  if (which_house_i == 1) {lower_ind = 1} else {lower_ind = house_ind[which_house_i-1] + 1}
  upper_ind = house_ind[which_house_i]
  # calculate exposure
  exposure_z = z[i] + sum(z[lower_ind:upper_ind])
  exposure_z
}

# The null
null_equiv = function(exposure_z1, exposure_z2) {
  ((exposure_z1 == 1) | (exposure_z1 == 0)) & ((exposure_z2 == 1) | (exposure_z2 == 0))
}

# Generate a treatment realization and outcome
Z = design_fn() # randomly get one realization
# Generate exposure under the realized Z
Z_exposure = rep(0, N); for (i in 1:N) { Z_exposure[i] = exposure_i(Z, i) }
# Generate observed outcomes based on exposures under Z
Y = out_bassefeller(N, K, Z_exposure, tau_main = 0.4, housestruct = housestruct)$Yobs 
# here we assume that potential outcomes are 0.4 higher if an untreated unit is in a cluster
# with a treated unit compared to in a cluster without any treated unit, 
# i.e., a spillover effect of 0.4 is assumed

# Do biclique decomposition on the null exposure graph
H0 = list(design_fn=design_fn, exposure_i=exposure_i, null_equiv=null_equiv)
bd = biclique.decompose(Z, H0, controls= list(method="greedy", mina=30, num_randomizations = 5e3))

# Define a test statistic, here we use the absolute value of differences in means between units with exposure 1 (untreated in treated cluster) and exposure 0 (untreated in untreated cluster)
Tstat = function(y, z, is_focal) {
  exposures = rep(0, N)
  for (unit in 1:N) {
    exposures[unit] = exposure_i(z, unit)[1]
  }
  stopifnot("all focals have same exposures" = (length(unique(exposures[is_focal]))>1) )
  abs(mean(y[is_focal & (exposures == 1)]) - mean(y[is_focal & (exposures == 0)]))
}

# Then run the test
testout = clique_test(Y, Z, Tstat, bd)
testout$p.value # p-value of the test


# Social network
library(BicliqueRT)
library(igraph)
set.seed(1)

N = 100
g = erdos.renyi.game(N, 0.1)
#generates social network g with probability 0.1 of a connection being formed between nodes

Z = sample(c(rep(1, N/2), rep(0, N/2))) # treatment, half of nodes are treated
A = get.adjacency(g) #finds the adjacency matrix of g
G = as.matrix(A); diag(G) = 0 # sets the adjacency mtx of to have diagonal 0 (ie no node is its own peer)

W = as.numeric(G %*% Z) # Finds the matrix product of G and Z (finds which node has a 1st degree treated peer)
Y = 0.1 + 1*Z  + 5*W+ rnorm(N) # here we have a positive spillover effect from immediate neighbours

# The design function
design_fn = function(){rbinom(289, 1, 0.5)} 

# The exposure function
exposure_i = function(z, i){
  stopifnot(length(z)==N)
  z[i]
}
#Will need to adjust this exposure function: exposure function outlines peer effects
# The null
null_equiv = function(e1, e2){
  identical(e1, e2)
}

# decompose
H0 = list(design_fn=design_fn, exposure_i=exposure_i, null_equiv=null_equiv)
bd = biclique.decompose(Z, H0, controls= list(method="greedy", mina=50, num_randomizations = 2e3))
Tstat = gen_tstat(adjacency_matrix, "elc")
testout = clique_test(Y,Z, Tstat, bd)
testout$p.value # p-value of the test


#Starting my own work: First, we will extract the test vector from tbl_graph as treatment
frame = tbl_graph %>%
  activate(nodes) %>%
  as_tibble()
treatment = frame$treated
first_peer = frame$first_order
second_peer = frame$second_order
third_peer = frame$third_order

N = 289
Y_test = 0.1 + 5*treatment  + 7*first_peer  + rnorm(N)
Z_test = rbinom(N, 1, 0.05)
#My test. To fix this, I need to get the adjacency matrix of our treatment.
H0_test = list(design_fn=design_fn, exposure_i=exposure_i, null_equiv=null_equiv)
bd_test = biclique.decompose(Z_test, H0_test, controls= list(method="greedy", mina=3, num_randomizations = 4e3))
Tstat_test = gen_tstat(adjacency_matrix, "elc")
testout_test = clique_test(Y_test,Z_test, Tstat_test, bd_test)
testout_test$p.value # p-value of the test
#Notes: We have very strong peer effects present but p-value is high, even when set a large amount of 
#randomizations and min biclique size. 
#Questions for David: What should we tune to make this more accurate?
#design, exposure, null? Probably need to check those.
#Can 