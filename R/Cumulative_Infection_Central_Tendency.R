### Code for analyzing cumulative infections

#Packages
library(dplyr)
library(tidyverse)

## STATS FUNCTIONS ##
summary_stats <- function(x) {
  c(
    Mean = mean(x),
    STD = sd(x, na.rm = F),
    Median = median(x),
    Q1 = quantile(x, 0.25),
    Q3 = quantile(x, 0.75),
    Max = max(x)
  )
}

# BASELINE -----
setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')
baseline <- read.csv('Cumulative_infection_SEAIR.CSV', header = T)
baseline$sim <- 1:nrow(baseline)
baseline$Swine <- baseline$Swine +5
baseline$prev <- baseline$Swine / 208 * 100
baseline$human <- baseline$Member + baseline$Attendee

results <- apply(baseline, 2, summary_stats)

write.csv(results, file = 'Baseline_Summary_Stats_SEAIR.csv')

# code for checking the output of the SAIRQ model relative to the SAIR
baseline2 <- read.csv('Cumulative_infection_q_base.csv', header=T)
baseline2$sim <- 1:nrow(baseline2)
baseline2$Swine <- baseline2$Swine +5
baseline2$prev <- baseline2$Swine / 208 * 100
baseline2$human <- baseline2$Member + baseline2$Attendee

comp_base_df <- baseline %>%
  right_join(baseline2, by = 'sim')

Base_prev <- cbind(baseline$prev, baseline2$prev)
Base_prev <- as.data.frame(Base_prev)
names(Base_prev) <- c('No Quarentine', 'Quarentine')

long_swine <- Base_prev %>% pivot_longer(cols = everything(),
                                         names_to = 'Base',
                                         values_to = 'Infected')

Base_human <- cbind(baseline2$human, baseline2$human)
Base_human <- as.data.frame(Base_human)
names(Base_human) <- c('No Quarentine', 'Quarentine')

long_human <- Base_human %>% pivot_longer(cols = everything(),
                                         names_to = 'Base',
                                         values_to = 'Infected')
ggplot(long_swine, aes(x=Base, y=Infected, fill=Base))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_viridis(discrete = T)+
  theme_ipsum()+
  theme(legend.position = "none")+
  labs(title = "Swine", x = "Model", y = "Infection Prevalence (%)") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggplot(long_human, aes(x=Base, y= Infected, fill=Base))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_viridis(discrete = T)+
  theme_ipsum()+
  theme(legend.position = "none")+
  labs(title = "Human", x = "Model", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

# FAIR DURATION ----- 

#file pathway
setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')

#importing CSV & creating simulation ID
Day_3 <- read.csv('Cumulative_Duration_3.csv', header = T)
names(Day_3) <- c('swine3', 'mem3', 'atten3')
Day_3$sim <- 1:nrow(Day_3)
Day_3$swine3 <- Day_3$swine3 + 5 # corrects the number by adding the initial infections that weren't counted
Day_3$prev <- Day_3$swine3 / 208 * 100
Day_3$human <- Day_3$mem3 + Day_3$atten3
 
Day_4 <- read.csv('Cumulative_Duration_4.csv', header = T)
names(Day_4) <- c('swine4', 'mem4', 'atten4')
Day_4$sim <- 1:nrow(Day_4)
Day_4$swine4 <- Day_4$swine4 + 5
Day_4$prev <- Day_4$swine4 / 208 * 100
Day_4$human <- Day_4$mem4 + Day_4$atten4

Day_5 <- read.csv('Cumulative_Duration_5.csv', header = T)
names(Day_5) <- c('swine5', 'mem5', 'atten5')
Day_5$sim <- 1:nrow(Day_5)
Day_5$swine5 <- Day_5$swine5 + 5
Day_5$prev <- Day_5$swine5 / 208 * 100
Day_5$human <- Day_5$mem5 + Day_5$atten5

Day_6 <- read.csv('Cumulative_Duration_6.csv', header = T)
names(Day_6) <- c('swine6', 'mem6', 'atten6')
Day_6$sim <- 1:nrow(Day_6)
Day_6$swine6 <- Day_6$swine6 + 5
Day_6$prev <- Day_6$swine6 / 208 * 100
Day_6$human <- Day_6$mem6 + Day_6$atten6

Day_7 <- read.csv('Cumulative_Duration_7.csv', header = T)
names(Day_7) <- c('swine7', 'mem7', 'atten7')
Day_7$sim <- 1:nrow(Day_7)
Day_7$swine7 <- Day_7$swine7 + 5
Day_7$prev <- Day_7$swine7 / 208 * 100
Day_7$human <- Day_7$mem7 + Day_7$atten7

Day_8 <- read.csv('Cumulative_Duration_8.csv', header = T)
names(Day_8) <- c('swine8', 'mem8', 'atten8')
Day_8$sim <- 1:nrow(Day_8)
Day_8$swine8 <- Day_8$swine8 + 5
Day_8$prev <- Day_8$swine8 / 208 * 100
Day_8$human <- Day_8$mem8 + Day_8$atten8


## Generating a single data frame
duration_df <- Day_3 %>%
  right_join(Day_4, by = 'sim') %>%
  right_join(Day_5, by = 'sim') %>%
  right_join(Day_6, by = 'sim') %>%
  right_join(Day_7, by = 'sim') %>%
  right_join(Day_8, by = 'sim') 

duration_df <- duration_df %>% select(-sim)

#running the data through the stats function
results <- apply(duration_df, 2, summary_stats)

write.csv(results, file = 'Fair_Duration_Summary_Stats.csv')

## Redistributing by population
# Case Count in swine
Swine <- cbind(Day_3$swine3, Day_4$swine4, Day_5$swine5, Day_6$swine6, Day_7$swine7, Day_8$swine8, baseline$Swine)
Swine <- as.data.frame(Swine)
names(Swine) <- c('3', '4', '5', '6', '7', '8', 'Baseline')

long_swined <- Swine %>% pivot_longer(cols = everything(),
                                      names_to = "Day",
                                      values_to = "Infected")


Human <-cbind(Day_3$human, Day_4$human, Day_5$human, Day_6$human, Day_7$human, Day_8$human, baseline$human)
Human <- as.data.frame(Human)
names(Human) <- c('3', '4', '5', '6', '7', '8', 'Baseline')

long_humand <- Human %>% pivot_longer(cols = everything(),
                                    names_to = "Day",
                                    values_to = "Infected")


# BIOSECURITY R0  -----

setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')

R03 <-read.csv('Cumulative_infection_R0_3.csv', header = T)
names(R03) <- c('swine3', 'mem3', 'atten3')
R03$sim <- 1:nrow(R03)
R03$swine3 <- R03$swine3 + 5
R03$prev <- R03$swine3 / 208 * 100
R03$human3 <- R03$mem3 + R03$atten3

R04 <-read.csv('Cumulative_infection_R0_4.csv', header = T)
names(R04) <- c('swine4', 'mem4', 'atten4')
R04$sim <- 1:nrow(R04)
R04$swine4 <- R04$swine4 + 5
R04$prev <- R04$swine4 / 208 * 100
R04$human4 <- R04$mem4 + R04$atten4

R08 <- read.csv('Cumulative_infection_R0_8.csv', header = T)
names(R08) <- c('swine8', 'mem8', 'atten8')
R08$sim <- 1:nrow(R08)
R08$swine8 <- R08$swine8 + 5
R08$prev <- R08$swine8 / 208 * 100
R08$human8 <-R08$mem8 + R08$atten8

R09 <- read.csv('Cumulative_infection_R0_9.csv', header = T)
names(R09) <- c('swine9', 'mem9', 'atten9')
R09$sim <- 1:nrow(R09)
R09$swine9 <- R09$swine9 + 5
R09$prev <- R09$swine9 / 208 * 100
R09$human9 <-R09$mem9 + R09$atten9

biosecurity_df <- R03 %>%
  right_join(R04, by = 'sim') %>%
  right_join(R08, by = 'sim') %>%
  right_join(R09, by = 'sim')

biosecurity_df <- biosecurity_df %>% select(-sim)


#running the data through the stats function
results <- apply(biosecurity_df, 2, summary_stats)

write.csv(results, file = 'Fair_Biosecurity_Summary_Stats.csv')

# Redistributing by population
Swine <- cbind( R03$swine3, R04$swine4, R08$swine8, R09$swine9)
Swine <- as.data.frame(Swine)
names(Swine) <- c('3', '4', '8', '9')

long_swineR0 <- Swine %>% pivot_longer(cols = everything(),
                                     names_to = "R0",
                                     values_to = "Infected")

Human <- cbind(R03$human3, R04$human4, R08$human8, R09$human9)
Human <- as.data.frame(Human)
names(Human) <- c('3', '4', '8', '9')

long_humanR0 <- Human %>% pivot_longer(cols = everything(),
                                      names_to = "R0",
                                      values_to = "Infected")
# reordering columns for plotting
long_swineR0$R0 <- factor(long_swineR0$R0, levels = c("3", "4", "8", "9"))
long_humanR0$R0 <- factor(long_humanR0$R0, levels = c("3", "4", "8", "9"))



# INITALLY INFECTED / PRE-FAIR TESTING ----
setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')

As2 <- read.csv('Cumulative_Initailly_Infected_2.csv', header = T)
names(As2) <- c('swine2', 'mem2', 'atten2')
As2$sim <- 1:nrow(As2)
As2$prev <- As2$swine2 / 208 * 100
As2$human <- As2$mem2 + As2$atten2

As1 <- read.csv('Cumulative_Initailly_Infected_1.csv', header = T)
names(As1) <- c('swine1', 'mem1', 'atten1')
As1$sim <- 1:nrow(As1)
As1$prev <- As1$swine1 / 208 * 100
As1$human <- As1$mem1 + As1$atten1


Inital_inf_df <- As1 %>%
  right_join(As2, by = 'sim')

Inital_inf_df <- Inital_inf_df %>% select(-sim)

#Summary Stats
results <- apply(Inital_inf_df, 2, summary_stats)

write.csv(results, file = 'PreFair_Testing_Summary_Stats.csv')

# Count the number of 0 outbreaks
count_zeros_As1_swine <- sum(As1$swine1 == 0)
count_zeros_As1_human <- sum(As1$human == 0)

print(paste("Number of 0s in the 'swine1' column:", count_zeros_As1_swine))
print(paste("Number of 0s in the 'human' column:", count_zeros_As1_human))

count_zeros_As2_swine <- sum(As2$swine2 == 0)
count_zeros_As2_human <- sum(As2$human == 0)

print(paste("Number of 0s in the 'swine2' column:", count_zeros_As2_swine))
print(paste("Number of 0s in the 'human' column:", count_zeros_As2_human))

# NO 0 entry statistics
As1 <- As1[As1$swine1 != 0, ]

As2 <- As2[As2$swine2 != 0, ]

As1 <- As1[As1$human != 0, ]

As2 <- As2[As2$human != 0, ]

results1 <- apply(As1, 2, summary_stats)
results2 <- apply(As2, 2, summary_stats)

write.csv(results1, file = 'PreFair_Testing_Summary_Stats_NO_0_As1.csv')
write.csv(results2, file = 'PreFair_Testing_Summary_Stats_NO_0_As2.csv')


# Redistributing by population
Swine <- cbind(As1$swine1, As2$swine2)
Swine <- as.data.frame(Swine)
names(Swine) <- c('1', '2')

long_swineAs <- Swine %>% pivot_longer(cols = everything(),
                                       names_to = "Initial",
                                       values_to = "Infected")

Human <- cbind(As1$human, As2$human)
Human <- as.data.frame(Human)
names(Human) <- c('1', '2')

long_humanAs <- Human %>% pivot_longer(cols = everything(),
                                     names_to = "Initial",
                                     values_to = "Infected")


# QUARENTINE & DETECTION -----

#Theta =1 day of detection 17% symptomatic
setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')

theta1.2 <- read.csv('Cumulative_infection_symptom_ID_0.2_17.csv', header = T)
names(theta1.2) <- c('swine.2', 'mem.2', 'atten.2')
theta1.2$sim <- 1:nrow(theta1.2)
theta1.2$swine.2 <- theta1.2$swine.2 + 5
theta1.2$prev <- theta1.2$swine.2 / 208 * 100
theta1.2$human.2 <- theta1.2$mem.2 + theta1.2$atten.2

theta1.4 <- read.csv('Cumulative_infection_symptom_ID_0.4_17.csv', header = T)
names(theta1.4) <- c('swine.4', 'mem.4', 'atten.4')
theta1.4$sim <- 1:nrow(theta1.4)
theta1.4$swine.4 <- theta1.4$swine.4 + 5
theta1.4$prev <- theta1.4$swine.4 / 208 * 100
theta1.4$human.4 <- theta1.4$mem.4 + theta1.4$atten.4

theta1.6 <- read.csv('Cumulative_infection_symptom_ID_0.6_17.csv', header = T)
names(theta1.6) <- c('swine.6', 'mem.6', 'atten.6')
theta1.6$sim <- 1:nrow(theta1.6)
theta1.6$swine.6 <- theta1.6$swine.6 + 5
theta1.6$prev <- theta1.6$swine.6 / 208 * 100
theta1.6$human.6 <- theta1.6$mem.6 + theta1.6$atten.6

theta1.8 <- read.csv('Cumulative_infection_symptom_ID_0.8_17.csv', header = T)
names(theta1.8) <- c('swine.8', 'mem.8', 'atten.8')
theta1.8$sim <- 1:nrow(theta1.8)
theta1.8$swine.8 <- theta1.8$swine.8 + 5
theta1.8$prev <- theta1.8$swine / 208 * 100
theta1.8$human.8 <- theta1.8$mem.8 + theta1.8$atten.8

theta1.10 <- read.csv('Cumulative_infection_symptom_ID_1.0_17.csv', header = T)
names(theta1.10) <- c('swine.1', 'mem.1', 'atten.1')
theta1.10$sim <- 1:nrow(theta1.10)
theta1.10$swine.1 <- theta1.10$swine.1 + 5
theta1.10$prev <- theta1.10$swine / 208 * 100
theta1.10$human.10 <- theta1.10$mem.1 + theta1.10$atten.1

#full data set
Quarentine17 <- theta1.2 %>%
  right_join(theta1.4, by = 'sim') %>%
  right_join(theta1.6, by = 'sim') %>%
  right_join(theta1.8, by = 'sim') %>%
  right_join(theta1.10, by = 'sim')

Quarentine17 <- Quarentine17 %>% select(-sim)


#Summary Stats
results <- apply(Quarentine17, 2, summary_stats)

write.csv(results, file = "Quarentine_17_day_of_Summary_Stat.csv",)


# Redistributing populations
Swine <- cbind(theta1.2$prev, theta1.4$prev, theta1.6$prev, theta1.8$prev, theta1.10$prev)
Swine <- as.data.frame(Swine)
names(Swine) <- c('20%', '40%', '60%', '80%', '100%')

long_swineT17 <- Swine %>% pivot_longer(cols = everything(),
                                       names_to = 'Chi',
                                       values_to = 'Infected')

Human <-cbind(theta1.2$human, theta1.4$human, theta1.6$human, theta1.8$human, theta1.10$human)
Human <- as.data.frame(Human)
names(Human) <- c('20%', '40%', '60%', '80%', '100%')

long_humanT17 <- Human %>% pivot_longer(cols = everything(),
                                      names_to = 'Chi',
                                      values_to = 'Infected')
# reordering columns for plotting
long_swineT17$Chi <- factor(long_swineT17$Chi, levels = c("20%", "40%", "60%", "80%", "100%"))
long_humanT17$Chi <- factor(long_humanT17$Chi, levels = c("20%", "40%", "60%", "80%", "100%"))



##SYMPTOMATIC 35%
setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')

theta2 <- read.csv('Cumulative_infection_symptom_ID_0.2_35.csv', header = T)
names(theta2) <- c('swine.2', 'mem.2', 'atten.2')
theta2$sim <- 1:nrow(theta2)
theta2$swine.2 <- theta2$swine.2 + 5
theta2$prev <- theta2$swine.2 / 208 * 100
theta2$human <- theta2$mem.2 + theta2$atten.2

theta4 <- read.csv('Cumulative_infection_symptom_ID_0.4_35.csv', header = T)
names(theta4) <- c('swine.4', 'mem.4', 'atten.4')
theta4$sim <- 1:nrow(theta4)
theta4$swine.4 <- theta4$swine.4 + 5
theta4$prev <- theta4$swine.4 / 208 * 100
theta4$human <- theta4$mem.4 + theta4$atten.4

theta6 <- read.csv('Cumulative_infection_symptom_ID_0.6_35.csv', header = T)
names(theta6) <- c('swine.6', 'mem.6', 'atten.6')
theta6$sim <- 1:nrow(theta6)
theta6$swine.6 <- theta6$swine.6 + 5
theta6$prev <- theta6$swine.6 / 208 * 100
theta6$human <- theta6$mem.6 + theta6$atten.6

theta8 <- read.csv('Cumulative_infection_symptom_ID_0.8_35.csv', header = T)
names(theta8) <- c('swine.8', 'mem.8', 'atten.8')
theta8$sim <- 1:nrow(theta8)
theta8$swine.8 <- theta8$swine.8 + 5
theta8$prev <- theta8$swine / 208 * 100
theta8$human <- theta8$mem.8 + theta8$atten.8

theta9 <- read.csv('Cumulative_infection_symptom_ID_1.0_35.csv', header = T)
names(theta9) <- c('swine.1', 'mem.1', 'atten.1')
theta9$sim <- 1:nrow(theta9)
theta9$swine.1 <- theta9$swine.1 + 5
theta9$prev <- theta9$swine.1 / 208 * 100
theta9$human <- theta9$mem.1 + theta9$atten.1

#full data set
Quarentine35 <- theta2 %>%
  right_join(theta4, by = 'sim') %>%
  right_join(theta6, by = 'sim') %>%
  right_join(theta8, by = 'sim') %>%
  right_join(theta9, by = 'sim')

Quarentine35 <- Quarentine35 %>% select(-sim)


#Summary Stats
results <- apply(Quarentine35, 2, summary_stats)

write.csv(results, file = "Quarentine_35_day_of_Summary_Stat.csv",)


# Redistributing populations
Swine <- cbind(theta2$prev, theta4$prev, theta6$prev, theta8$prev, theta9$prev)
Swine <- as.data.frame(Swine)
names(Swine) <- c('20%', '40%', '60%', '80%', '100%')

long_swineT35 <- Swine %>% pivot_longer(cols = everything(),
                                        names_to = 'Chi',
                                        values_to = 'Infected')

Human <-cbind(theta2$human, theta4$human, theta6$human, theta8$human, theta9$human)
Human <- as.data.frame(Human)
names(Human) <- c('20%', '40%', '60%', '80%', '100%')

long_humanT35 <- Human %>% pivot_longer(cols = everything(),
                                        names_to = 'Chi',
                                        values_to = 'Infected')


# reordering columns for plotting
long_swineT35$Chi <- factor(long_swineT35$Chi, levels = c("20%", "40%", "60%", "80%", "100%"))
long_humanT35$Chi <- factor(long_humanT35$Chi, levels = c("20%", "40%", "60%", "80%", "100%"))


# BIOSECURITY & FAIR DURATION -----
setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')

scen1 <- read.csv('Cumulative_R0_3_Duration_3.csv', header =  T)
scen1$sim <- 1:nrow(scen1)
scen1$Swine1 <- scen1$Swine + 5
scen1$prev <- scen1$Swine / 208 * 100
scen1$human <- scen1$Member + scen1$Attendee

scen2 <- read.csv('Cumulative_R0_3_Duration_4.csv', header = T)
scen2$sim <- 1:nrow(scen2)
scen2$Swine2 <- scen2$Swine  + 5
scen2$prev <- scen2$Swine / 208 * 100
scen2$human <- scen2$Member + scen2$Attendee

scen3 <- read.csv('Cumulative_R0_3_Duration_5.csv', header = T)
scen3$sim <- 1:nrow(scen3)
scen3$Swine3 <- scen3$Swine + 5
scen3$prev <- scen3$Swine / 208 * 100
scen3$human <- scen3$Member + scen3$Attendee

scen4 <- read.csv('Cumulative_R0_3_Duration_7.csv', header = T)
scen4$sim <- 1:nrow(scen4)
scen4$Swine4 <- scen4$Swine + 5
scen4$prev <- scen4$Swine / 208 * 100
scen4$human <- scen4$Member + scen4$Attendee

scen5 <- read.csv('Cumulative_R0_4_Duration_3.csv', header = T)
scen5$sim <- 1:nrow(scen5)
scen5$Swine5 <- scen5$Swine + 5
scen5$prev <- scen5$Swine / 208 * 100
scen5$human <- scen5$Member + scen5$Attendee

scen6 <- read.csv('Cumulative_R0_4_Duration_4.csv', header = T)
scen6$sim <- 1:nrow(scen6)
scen6$Swine6 <- scen6$Swine + 5
scen6$prev <- scen6$Swine / 208 * 100
scen6$human <- scen6$Member + scen6$Attendee

scen7 <- read.csv('Cumulative_R0_4_Duration_5.csv', header = T)
scen7$sim <- 1:nrow(scen7)
scen7$Swine7 <- scen7$Swine + 5
scen7$prev <- scen7$Swine / 208 * 100
scen7$human <- scen7$Member + scen7$Attendee

scen8 <- read.csv('Cumulative_R0_4_Duration_7.csv', header = T)
scen8$sim <- 1:nrow(scen8)
scen8$Swine8 <- scen8$Swine + 5
scen8$prev <- scen8$Swine / 208 * 100
scen8$human <- scen8$Member + scen8$Attendee

# Full data set
R0_Duration <- scen1 %>%
  right_join(scen2, by = 'sim') %>%
  right_join(scen3, by = 'sim') %>%
  right_join(scen4, by = 'sim') %>%
  right_join(scen5, by = 'sim') %>%
  right_join(scen6, by = 'sim') %>%
  right_join(scen7, by = 'sim') %>%
  right_join(scen8, by = 'sim')

R0_Duration <- R0_Duration %>% select(-sim)

#Summary Statistics
results <- apply(R0_Duration, 2, summary_stats)

write.csv(results, file = "R0_Duration_Summary_Stats.csv")

# Separating by population
Swine <- cbind(scen1$Swine1, scen2$Swine2, scen3$Swine3, scen4$Swine4, scen5$Swine5, scen6$Swine6, scen7$Swine7, scen8$Swine8)
Swine <- as.data.frame(Swine)
names(Swine) <- c("#1", "#2", "#3", "#4", "#5", "#6", "#7", "#8")

long_swineR0D <- Swine %>% pivot_longer(cols = everything(),
                                    names_to = "R0.D",
                                    values_to = "Infected")

Human<- cbind(scen1$human, scen2$human, scen3$human, scen4$human, scen5$human, scen6$human, scen7$human, scen8$human)
Human <- as.data.frame(Human)
names(Human) <- c("#1", "#2", "#3", "#4", "#5", "#6", "#7", "#8")

long_HumanR0D <- Human %>% pivot_longer(cols = everything(),
                                       names_to = "R0.D",
                                       values_to = "Infected")

# PREFAIR TESTING, BIOSECURITY, & QUARENTINE -----
setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Tau leap Sims/')

scen9 <- read.csv('Cumulative_infection_Theata_1_Intial_Inf_1_R03.csv', header = T)
scen9$sim <- 1:nrow(scen9)
scen9$prev <- scen9$Swine / 208 * 100
scen9$human <- scen9$Member + scen9$Attendee

scen10 <- read.csv('Cumulative_infection_Theata_1_Intial_Inf_1_R04.csv', header = T)
scen10$sim <- 1:nrow(scen10)
scen10$prev <- scen10$Swine / 208 * 100
scen10$human <- scen10$Member + scen10$Attendee

scen11 <- read.csv('Cumulative_infection_Theata_1_Intial_Inf_2_R03.csv', header = T)
scen11$sim <- 1:nrow(scen11)
scen11$prev <- scen11$Swine / 208 * 100
scen11$human <- scen11$Member + scen11$Attendee

scen12 <- read.csv('Cumulative_infection_Theata_1_Intial_Inf_2_R04.csv', header = T)
scen12$sim <- 1:nrow(scen12)
scen12$prev <- scen12$Swine / 208 * 100
scen12$human <- scen12$Member + scen12$Attendee

# Full data set
Testing_Biosecurity <- scen9 %>%
  right_join(scen10, by = "sim") %>%
  right_join(scen11, by = "sim") %>%
  right_join(scen12, by = "sim") 
  
Testing_Biosecurity <- Testing_Biosecurity %>% select(-sim)

#Summary Stats
results <- apply(Testing_Biosecurity, 2, summary_stats)

write.csv(results, file = "Testing_Biosecurity_summary_stats.csv")

# Count the number of 0 outbreaks
count_zeros_scen9_swine <- sum(scen9$Swine == 0)
count_zeros_scen9_human <- sum(scen9$human == 0)

print(paste("Number of 0s in the 'swine' column:", count_zeros_scen9_swine))
print(paste("Number of 0s in the 'human' column:", count_zeros_scen9_human))

count_zeros_scen10_swine <- sum(scen10$Swine == 0)
count_zeros_scen10_human <- sum(scen10$human == 0)

print(paste("Number of 0s in the 'swine' column:", count_zeros_scen10_swine))
print(paste("Number of 0s in the 'human' column:", count_zeros_scen10_human))

count_zeros_scen11_swine <- sum(scen11$Swine == 0)
count_zeros_scen11_human <- sum(scen11$human == 0)

print(paste("Number of 0s in the 'swine' column:", count_zeros_scen11_swine))
print(paste("Number of 0s in the 'human' column:", count_zeros_scen11_human))

count_zeros_scen12_swine <- sum(scen12$Swine == 0)
count_zeros_scen12_human <- sum(scen12$human == 0)

print(paste("Number of 0s in the 'swine' column:", count_zeros_scen12_swine))
print(paste("Number of 0s in the 'human' column:", count_zeros_scen12_human))

# NO 0 entry statistics
scen9 <- scen9[scen9$Swine != 0, ]

scen9 <- scen9[scen9$Swine != 0, ]

scen10 <- scen10[scen10$human != 0, ]

scen10 <- scen10[scen10$human != 0, ]

scen11 <- scen11[scen11$Swine != 0, ]

scen11 <- scen11[scen11$Swine != 0, ]

scen12 <- scen12[scen12$human != 0, ]

scen12 <- scen12[scen12$human != 0, ]

#Redistributing the populations
Swine <- cbind(scen9$Swine, scen10$Swine, scen11$Swine, scen12$Swine)
Swine <- as.data.frame(Swine)
names(Swine) <- c("#9", "#10", "#11", "#12")

long_swineBioTest <- Swine %>% pivot_longer(cols = everything(),
                                            names_to = "Scenario",
                                            values_to = "Infected")

Human <- cbind(scen9$human, scen10$human, scen11$human, scen12$human)
Human<- as.data.frame(Human)
names(Human) <- c("#9", "#10", "#11", "#12")


long_HumanBioTest <- Human %>% pivot_longer(cols = everything(),
                                           names_to = "Scenario",
                                           values_to = "Infected")
# reordering columns for plotting
long_swineBioTest$Scenario<- factor(long_swineBioTest$Scenario, levels = c("#9", "#10", "#11", "#12"))
long_HumanBioTest$Scenario <- factor(long_HumanBioTest$Scenario , levels = c("#9", "#10", "#11", "#12"))
