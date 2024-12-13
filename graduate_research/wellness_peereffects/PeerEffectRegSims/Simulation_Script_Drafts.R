#I then moved to simulate the Greek membership and high school drinking example.
#In this example, I randomly generated a dummy variable "high_school", indicating whether
#that individual drank in high school. I used "high_school" to determine the probability
#of whether someone joined Greek life to capture the influence of high school drinking
# on joining Greek life. In this simulation, there are no peer effects on high school drinking.
#I did this to show Angrist's weak instrument critique of how peer effects can show up in 
#in regressions even when they do not exist. 
greek_data = data.frame(high_school = rbinom(n = 100000, size = 1, prob = 0.4))
greek_data=greek_data %>% 
  mutate(greek = ifelse(high_school == 1, 
                        yes = rbinom(n = 100000, size = 1, prob = 0.8), 
                        no=rbinom(n = 100000, size = 1, prob = 0.45)))
lm(greek~high_school, data=greek_data)

#After the 

#Setting up partition into separate groups
student_assignment = function(num_group, pop){
  student_pop = pop
  # student_pop = data.frame(high_school = pop)
  n_observations = nrow(student_pop)
  indices = seq_len(n_observations)
  shuffled_indices = sample(indices)
  group_indices = split(shuffled_indices, cut(seq_along(shuffled_indices), breaks = num_group, labels = FALSE)) 
  
  group_list = list()
  for (i in 1:num_group) {
    group_name = paste("Group",i, sep = "_")
    group_df = data.frame(high_school = student_pop[group_indices[[i]],])
    group_df = group_df %>%
      mutate(Group = group_name,
             hs_rate = mean(high_school),
             # greek = ifelse(high_school == 1, 
             #                yes = rbinom(n = 100000, size = 1, prob = 0.6), 
             #                no=rbinom(n = 100000, size = 1, prob = 0.4)),
             greek_rate = mean(greek))
    group_list[[group_name]] <- group_df
  } 
  
  Groups = do.call(rbind, group_list)
  Groups = dummy_cols(Groups, select_columns = "Group")
  return(Groups)
}

pop = rbinom(n = 10000, size = 1, prob = 0.5)
student_body = data.frame(high_school = pop)
student_body = student_body %>%
  mutate(greek = ifelse(high_school == 1, 
                        yes = rbinom(n = 100000, size = 1, prob = 0.6), 
                        no=rbinom(n = 100000, size = 1, prob = 0.4)),)
Test_Group = student_assignment(5, student_body
                                
                                # Dorm_Group = student_assignment(25)
                                # Floor_Group = student_assignment(100)
                                # Room_Group = student_assignment(1000)
                                # 
                                # hs_OLS = lm(greek~high_school, data = Dorm_Group)
                                
                                
                                #For this peer effects regression, we want to estimate with regression 
                                #E(y|z, x) = B*mean of x given z + C*x. In peer effects, this means finding the expected value of y
                                #given the individual's value of x and the expected value of x given their group, the mean of x.
                                #In OLS, this is a simple regression, as seen below. With 2SLS regression, we instrument x with
                                #all dummies for z. 
                                
                                #In this model, we place E(y|z) on one side of the equation and E(x|z) on the other. I simulated 
                                #the GSS experiment on high school drinking and Greek life participation by generating 2800 values that had value 
                                #0 or 1, reflecting whether a student drank in high school, named hs_rate. Using the student assignment function I made above, I sorted students 
                                #into groups of equal size. Furthermore, I generated variable Greek for each observation to indicate if the student
                                #participated in greek life. If a student drank in high school, they had a 0.6 probability of pledging while if they did not 
                                #drink, they had a 0.4 probability of pledging. Greek life participation could then be modeled with the equation
                                #Greek_Life = 0.4 + (0.2)*hs_rate. The expected coefficient on the high school drinking in a OLs regression would be 0.2  
                                
                                #I also created the variable "Greek rate", the average rate of Greek life participation for the individuals group. 
                                #In this model, to demonstrate how peer effects are approximately equivalent to the ratio of 2SLS extimates
                                #and OLS estimates of individual covariates, I ran 3 regressions for each group. The first regression was equivalent to equation
                                #(4) in the Angrist paper, regressing greek life participation on high school drinking and the average rate of greek life participating 
                                #of the student's group (in this case the variable greek_rate). I then regressed Greek participating on high school drinking
                                #using OLS and again using 2SLS. In the 2SLS case, students groups were used as instruments for high school drinking.
                                

# student_pop = data.frame(high_school = rbinom(n = 10000, size = 1, prob = 0.5))
# n_observations = nrow(student_pop)
# indices = seq_len(n_observations)
# shuffled_indices = sample(indices)
# dorm_indices = split(shuffled_indices, cut(seq_along(shuffled_indices), breaks = 4, labels = FALSE))
# Dorm_A = data.frame(high_school = student_pop[dorm_indices[[1]], ]) %>% 
#   mutate(hs_rate = mean(high_school), 
#          Dorm = "A", 
#          greek = ifelse(high_school == 1, 
#                         yes = rbinom(n = 100000, size = 1, prob = 0.6), 
#                         no=rbinom(n = 100000, size = 1, prob = 0.4)),
#          greek_rate = mean(greek))
# Dorm_B = data.frame(high_school = student_pop[dorm_indices[[2]], ]) %>% 
#   mutate(hs_rate = mean(high_school), 
#          Dorm = "B",
#          greek = ifelse(high_school == 1, 
#                         yes = rbinom(n = 100000, size = 1, prob = 0.6), 
#                         no=rbinom(n = 100000, size = 1, prob = 0.4)),
#          greek_rate = mean(greek))
# Dorm_C = data.frame(high_school = student_pop[dorm_indices[[3]], ]) %>%
#   mutate(hs_rate = mean(high_school), 
#          Dorm = "C",
#          greek = ifelse(high_school == 1, 
#                         yes = rbinom(n = 100000, size = 1, prob = 0.6), 
#                         no=rbinom(n = 100000, size = 1, prob = 0.4)),
#          greek_rate = mean(greek))
# Dorm_D = data.frame(high_school = student_pop[dorm_indices[[4]], ]) %>%
#   mutate(hs_rate = mean(high_school), 
#          Dorm = "D",
#          greek = ifelse(high_school == 1, 
#                         yes = rbinom(n = 100000, size = 1, prob = 0.6), 
#                         no=rbinom(n = 100000, size = 1, prob = 0.4)),
#          greek_rate = mean(greek))
# Dorms = rbind(Dorm_A, Dorm_B, Dorm_C, Dorm_D)
# Dorms = dummy_cols(Dorms, select_columns = "Dorm")
Floor_check = Dorm_Group %>%
  group_by(group_number, Dorm) %>%
  summarize
Dorm_Group = Dorm_Group %>%
  group_by()
Dorm_split = split(Dorm_Group, f = Dorm_Group$group_number)


Dorm_Instruments = as.formula(paste("~", paste(colnames(Dorm_Group)[4:10], collapse = "+")))