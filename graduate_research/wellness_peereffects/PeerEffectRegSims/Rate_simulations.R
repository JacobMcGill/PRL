##Peer Effect Simulation
library(AER)
library(systemfit)
library(fastDummies)
library(ivreg)
library(tidyverse)
library(sandwich)
library(lmtest)
set.seed(547)
#Coefficient of unity
#First, I am going to simulate the "smoke a lot of dope" example from Angrist's paper.
#I created 3 groups of equal size with a indicator variable (yes") randomly generated for each group
#at a different rate. Group membership and the probability of the indicator variable being 1
# was also recorded in each data frame. I then combined the dataframes into one frame.
Group_A = data.frame(yes = rbinom(n = 100000, size = 1, prob = 0.5), Group = "A", rate = 0.5)
Group_A = Group_A %>% 
  mutate(rate = mean(yes))
Group_B = data.frame(yes = rbinom(n = 100000, size = 1, prob = 0.2), Group = "B", rate = 0.2)
Group_B = Group_B %>% 
  mutate(rate = mean(yes))
Group_C = data.frame(yes = rbinom(n = 100000, size = 1, prob = 0.8), Group = "C", rate = 0.8)
Group_C = Group_C %>% 
  mutate(rate = mean(yes))
combined_groups = rbind(Group_A, Group_B, Group_C)
#After combining the dataframes, regressed "yes" onto the rate for the group 
#an observation was part of
combined_reg = lm(yes ~ rate, data = combined_groups)
summary(combined_reg)
#As can be seen, this produces a coefficient of 1, as Angrist predicted.


#I then moved to simulate the Greek membership and high school drinking example.
#In this example, I randomly generated a dummy variable "high_school", indicating whether
#that individual drank in high school. I used "high_school" to determine the probability
#of whether someone joined Greek life to capture the influence of high school drinking
# on joining Greek life. In this simulation, there are no peer effects on high school drinking.
#I did this to show Angrist's weak instrument critique of how peer effects can show up in 
#in regressions even when they do not exist. 

#I started by creating the population, referred to as student body. I generated "high_school",
# indicating if someone drinked in high school. I then created "greek", which indicated if 
# some joined greek life. There is a different rate for joining greek life for high school
# drinkers and non-high school drinkers. However, peer effects play no role here.
set.seed(853)
population = rbinom(n = 10000, size = 1, prob = 0.5)
student_body = data.frame(high_school = population)
student_body = student_body %>%
  mutate(greek = ifelse(high_school == 1, 
                        yes = rbinom(n = 10000, size = 1, prob = 0.6), 
                        no=rbinom(n = 10000, size = 1, prob = 0.4)),)
body_summary = student_body %>%
  group_by(high_school) %>%
  summarize(mean_greek = mean(greek))
lm(greek~high_school, data = student_body)
# Notes from David: Combine this code with the randomization code. Come up with a similar peer effect test 
# Answering the same question but different statistical paradigm. Next steps would be looking at randomization.
#After the population had been created, I used the following grouper function, which 
#I found on Stack Exchange, to split the population into groups of different levels. First
# I split the population into 7 groups, to represent 7 dorms. After that, I split
# each of the seven groups into 5 subgroups, to capture the floor of each dorm. Finally,
# I split each floor into 20 subgroups, capturing a dorm room. This model ensures that we can
# compare estimates of peer effects across more coarse groupings.
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
grouper_no_dummy <- function(df, n) {
  
  # create a random number for each row
  set.seed(2262024)
  random <- sample(1:nrow(df), replace = FALSE, nrow(df))
  
  # divide the random number by the group size
  df$group_number <- ceiling(random / (nrow(df) / n))
  
  return(df)  
}
test = grouper(student_body, 5)
#xtabs(~group_number, test)


#Instrument on Dorm rooms
Dorm_Group = grouper_no_dummy(student_body, 7)
colnames(Dorm_Group)[which(names(Dorm_Group) == "group_number")] <- "Dorm"
Dorm_Group = Dorm_Group %>%
  mutate(dorm_assignment = paste("Dorm", Dorm))
  #I realize this naming scheming is a bit clunky, but I added it to ensure that
  # we can differentiate between floors on different dorms. I took this approach for
  # rooms too.
Dorm_Group = Dorm_Group %>%
  group_by(Dorm) %>%
  mutate(group_number = ceiling(row_number() / (n() / 5)))
# I added this to check that floors are equally distributed across dorms
#xtabs(~room_assignment + Dorm, Dorm_Group)
# They appear to be so, so we can move on
colnames(Dorm_Group)[which(names(Dorm_Group) == "group_number")] <- "Floor"
Dorm_Group = Dorm_Group %>%
  mutate(floor_assignment = paste(dorm_assignment, "Floor", Floor)) %>%
  group_by(floor_assignment) %>%
  mutate(room_number = ceiling(row_number() / (n() / 20)),
         room_assignment = paste(floor_assignment, "Room", room_number))
# I calculate the following summary statistics to check that students were distributed
# randomly. Doing just a spot check, it looks they were.
Dorm_check = Dorm_Group %>%
  group_by(dorm_assignment) %>%
  summarize(high_school_rate = mean(high_school),
            greek_rate = mean(greek))
Floor_check = Dorm_Group %>%
  group_by(floor_assignment) %>%
  summarize(high_school_rate = mean(high_school),
          greek_rate = mean(greek))
Room_check = Dorm_Group %>%
  group_by(room_assignment) %>%
  summarize(high_school_rate = mean(high_school),
            greek_rate = mean(greek))
# I then regressed greek on high school with an OLS model and with a 2SLS model
# instrumenting high school with dorm_assignment.
Dorm_OLS = lm(greek~high_school, data = Dorm_Group)
Dorm_SLS = ivreg(greek~high_school|dorm_assignment, data = Dorm_Group)
Dorm_OLS_Summary = summary(Dorm_OLS)
Dorm_2SLS_Summary = summary(Dorm_SLS)

dorm_OLS_coeff = Dorm_OLS_Summary$coefficients[2,1]
dorm_OLS_se = Dorm_OLS_Summary$coefficients[2,2]
dorm_2SLS_coeff = Dorm_2SLS_Summary$coefficients[2,1]
dorm_2SLS_se = Dorm_2SLS_Summary$coefficients[2,2]
dorm_SLS_OLS_ratio = dorm_2SLS_coeff/dorm_OLS_coeff

#I repeated the above regression but instead instrumented high_school with floor_assignment
Floor_OLS = lm(greek~high_school, data=Dorm_Group)
Floor_2SLS = ivreg(greek~high_school 
                   |floor_assignment, data = Dorm_Group)

Floor_OLS_Summary = summary(Floor_OLS)
Floor_2SLS_Summary = summary(Floor_2SLS)

floor_OLS_coeff = Floor_OLS_Summary$coefficients[2,1]
floor_OLS_se = Floor_OLS_Summary$coefficients[2,2]
floor_2SLS_coeff = Floor_2SLS_Summary$coefficients[2,1]
floor_2SLS_se = Floor_2SLS_Summary$coefficients[2,2]
floor_SLS_OLS_ratio = floor_2SLS_coeff/floor_OLS_coeff

#Finally, I repeated regressions using room_assignment as an instrument.
Room_OLS = lm(greek~high_school, data = Dorm_Group)
Room_SLS = ivreg(greek~high_school
                 |room_assignment,data = Dorm_Group)

Room_OLS_Summary = summary(Room_OLS)
Room_2SLS_Summary = summary(Room_SLS)

room_OLS_coeff = Room_OLS_Summary$coefficients[2,1]
room_OLS_se = Room_OLS_Summary$coefficients[2,2]
room_2SLS_coeff = Room_2SLS_Summary$coefficients[2,1]
room_2SLS_se = Room_2SLS_Summary$coefficients[2,2]
room_SLS_OLS_ratio = room_2SLS_coeff/room_OLS_coeff

#Result Summary

Results = matrix(
  c(dorm_OLS_coeff, dorm_OLS_se, dorm_2SLS_coeff, dorm_2SLS_se, dorm_SLS_OLS_ratio,
    floor_OLS_coeff, floor_OLS_se, floor_2SLS_coeff, floor_2SLS_se, floor_SLS_OLS_ratio,
    room_OLS_coeff, room_OLS_se, room_2SLS_coeff, room_2SLS_se, room_SLS_OLS_ratio),
  nrow = 3,
  ncol = 5,
  byrow = TRUE
)
colnames(Results) = c( "OLS Reg", "OLS SE", "2SLS Reg","2SLS SE", "Ratio")
rownames(Results) = c("Dorms", "Floors", "Rooms")

Dorm_2SLS_Summary
Floor_2SLS_Summary
Room_2SLS_Summary
print(Results)
# These results align with what Angrist predicted. As the partitions become more coarse,
# the standard errors of the 2SLS regression rise. Also, more coarse partitions see a 
# decrease in the ratio of the 2SLS regression to OLS regression. The weak first stage of the
# instrument results in the model appearing to capture peer effects, even though none existed
# in the data generating process.

