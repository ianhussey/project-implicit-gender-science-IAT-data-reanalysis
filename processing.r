# title: exploring project implicit gender-science IAT data 
# author: Ian Hussey (ian.hussey@ugent.be)
# license: 
#   code: GPLv3+    https://www.gnu.org/licenses/quick-guide-gplv3.en.html
#   data: CC-0      https://creativecommons.org/publicdomain/zero/1.0/


# dependencies ------------------------------------------------------------


library(tidyverse)
library(schoRsch)


# acquire data ------------------------------------------------------------


#gender_science_data_2003 <- read.delim("/Users/Ian/Dropbox overflow/Data/Gender-Science IAT raw data 2003-2015/iat2003.txt")

#save(gender_science_data_2003, file = "/Users/Ian/Dropbox overflow/Data/Gender-Science IAT raw data 2003.RData")
load(file = "/Users/Ian/Dropbox overflow/Data/Gender-Science IAT raw data 2003.RData")


# Ss with complete data ---------------------------------------------------


# list participants with the right amount of trials
subs_with_complete_data <- 
  gender_science_data_2003 %>%
  group_by(session_id) %>%
  summarize(trials = n()) %>%
  filter(trials == 200)

# select out these participants
screened_data_1 <-
  left_join(subs_with_complete_data, gender_science_data_2003, by = "session_id") %>%
  filter(block_name == "BLOCK2" |     # select only their test blocks
           block_name == "BLOCK3" | 
           block_name == "BLOCK5" | 
           block_name == "BLOCK6") %>%
  select(-trials)

# IAT block order
parameters <- 
  screened_data_1 %>%
  distinct(session_id, .keep_all = TRUE) %>%
  select(session_id, 
         task_name)


# D1 scores ---------------------------------------------------------------


D1_scores <- 
  screened_data_1 %>%
  filter(trial_latency <= 10000) %>%
  group_by(session_id) %>%
  summarise(block2_mean = mean(trial_latency[block_name == "BLOCK2"]),
            block3_mean = mean(trial_latency[block_name == "BLOCK3"]),
            block5_mean = mean(trial_latency[block_name == "BLOCK5"]),
            block6_mean = mean(trial_latency[block_name == "BLOCK6"]),
            block2_5_sd = sd(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK5"]),
            block3_6_sd = sd(trial_latency[block_name == "BLOCK3" | block_name == "BLOCK6"]),
            D1a = (block5_mean - block2_mean) / block2_5_sd,
            D1b = (block6_mean - block3_mean) / block3_6_sd,
            D1 = mean(D1a, D1b)) %>% 
  select(session_id, D1)

D1_scores_all <- 
  screened_data_1 %>%
  filter(trial_latency <= 10000) %>%
  group_by(session_id) %>%
  summarise(block2_3_mean = mean(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK3"]),
            block5_6_mean = mean(trial_latency[block_name == "BLOCK5" | block_name == "BLOCK6"]),
            block_sd = sd(trial_latency),
            D1_all = (block5_6_mean - block2_3_mean) / block_sd) %>%
  select(session_id, D1_all)

D1_scores_no_trimming <- 
  screened_data_1 %>%
  #filter(trial_latency <= 10000) %>%
  group_by(session_id) %>%
  summarise(block2_mean = mean(trial_latency[block_name == "BLOCK2"]),
            block3_mean = mean(trial_latency[block_name == "BLOCK3"]),
            block5_mean = mean(trial_latency[block_name == "BLOCK5"]),
            block6_mean = mean(trial_latency[block_name == "BLOCK6"]),
            block2_5_sd = sd(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK5"]),
            block3_6_sd = sd(trial_latency[block_name == "BLOCK3" | block_name == "BLOCK6"]),
            D1a = (block5_mean - block2_mean) / block2_5_sd,
            D1b = (block6_mean - block3_mean) / block3_6_sd,
            D1_no_trimming = mean(D1a, D1b)) %>% 
  select(session_id, D1_no_trimming)

D1_scores_sd_trimming <- 
  screened_data_1 %>%
  outlier(dv = "trial_latency", 
          todo = "elim", 
          upper.z = 2.5, 
          lower.z = -2.5) %>%
  group_by(session_id) %>%
  summarise(block2_mean = mean(trial_latency[block_name == "BLOCK2"]),
            block3_mean = mean(trial_latency[block_name == "BLOCK3"]),
            block5_mean = mean(trial_latency[block_name == "BLOCK5"]),
            block6_mean = mean(trial_latency[block_name == "BLOCK6"]),
            block2_5_sd = sd(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK5"]),
            block3_6_sd = sd(trial_latency[block_name == "BLOCK3" | block_name == "BLOCK6"]),
            D1a = (block5_mean - block2_mean) / block2_5_sd,
            D1b = (block6_mean - block3_mean) / block3_6_sd,
            D1_sd_trimming = mean(D1a, D1b)) %>% 
  select(session_id, D1_sd_trimming)



# PI scores ---------------------------------------------------------------


library(pim)

N <- length(unique(screened_data_1$session_id))
PI <- numeric(N)

for(i in 1:N){
  x <- subset(screened_data_1, screened_data_1$session_id == unique(screened_data_1$session_id)[i])
  id1 <-which(x$block_name == "BLOCK5" | x$block_name == "BLOCK6")
  id0 <-which(x$block_name == "BLOCK2" | x$block_name == "BLOCK3")
  poset <- rbind(expand.grid(id1,id0))
  pos <- list(L = poset$Var1, R = poset$Var2)
  pim.fit <- pim(trial_latency ~ 1, data = x, compare = pos, link = "logit", estim = "estimator.glm")
  PI[i] <- as.numeric(pim.fit@coef)
}
PI <- plogis(PI)

# Returns:
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: algorithm did not converge 


# combine data frames -----------------------------------------------------


comparisons <- 
  left_join(D1_scores, D1_scores_no_trimming, by = "session_id") %>%
  left_join(D1_scores_sd_trimming, by = "session_id") %>%
  left_join(parameters, by = "session_id") %>%
  left_join(D1_scores_all, by = "session_id")


# plots -------------------------------------------------------------------


# 1d plots
ggplot(comparisons, 
         aes(x = D1)) + 
  geom_density(alpha = 0.3)

ggplot(comparisons, 
         aes(x = D1)) + 
  geom_density(aes(group = task_name, 
                   colour = task_name, 
                   fill = task_name), 
               alpha = 0.3)

# 2d plots
ggplot(comparisons,
         aes(x = D1, 
             y = D1_sd_trimming)) +
  geom_point(aes(colour = factor(task_name)),
             alpha = .5,
             shape = 16,
             size = 1)

ggplot(comparisons,
         aes(x = D1, 
             y = D1_no_trimming)) +
  geom_point(aes(colour = factor(task_name)),
             alpha = .5,
             shape = 16,
             size = 1)

ggplot(comparisons,
         aes(x = D1_no_trimming, 
             y = D1_sd_trimming)) +
  geom_point(aes(colour = factor(task_name)),
             alpha = .5,
             shape = 16,
             size = 1)

# traditional D1 vs D1 using all con vs all incon
ggplot(comparisons,
       aes(x = D1, 
           y = D1_all)) +
  geom_point(alpha = .1,
             shape = 16,
             size = 1)


# 3d plots
attach(comparisons)

library(rgl)
plot3d(D1, D1_no_trimming, D1_sd_trimming, col="red", size=3)

library(Rcmdr)
scatter3d(D1, D1_no_trimming, D1_sd_trimming)


# D1 scores by tt ---------------------------------------------------------


# trial_types <- 
#   screened_data_1 %>%
#   distinct(trial_name)
# write.csv(trial_types, "trial types 2003.csv")

trial_types <- 
  screened_data_1 %>%
  mutate(trial_type = ifelse(trial_name == "Son" | 
                               trial_name == "Boy" |
                               trial_name == "Father" |
                               trial_name == "Husband" |
                               trial_name == "Grandpa" |
                               trial_name == "Man" |
                               trial_name == "Uncle" |
                               trial_name == "Male", "men",
                             ifelse(trial_name == "Grandma" |
                                      trial_name == "Female" |
                                      trial_name == "Mother" |
                                      trial_name == "Girl" |
                                      trial_name == "Daughter" |
                                      trial_name == "Woman" |
                                      trial_name == "Aunt" |
                                      trial_name == "Wife", "women",
                                    ifelse(trial_name == "LATIN" |
                                             trial_name == "ENGLISH" |
                                             trial_name == "ARTS" |
                                             trial_name == "HUMANITIES" |
                                             trial_name == "SPANISH" |
                                             trial_name == "MUSIC" |
                                             trial_name == "PHILOSOPHY" |
                                             trial_name == "HISTORY" |
                                             trial_name == "LIBERAL ARTS", "arts", 
                                           ifelse(trial_name == "BIOLOGY" |
                                                    trial_name == "CHEMISTRY" |
                                                    trial_name == "NEUROSCIENCE" |
                                                    trial_name == "BIOCHEMISTRY" |
                                                    trial_name == "ASTRONOMY" |
                                                    trial_name == "ENGINEERING" |
                                                    trial_name == "PHYSICS" |
                                                    trial_name == "BIOPHYSICS" |
                                                    trial_name == "SCIENCE", "science", "na")))))

distinct(trial_types, trial_type)

D1_scores_by_tt <- 
  trial_types %>%
  filter(trial_latency <= 10000) %>%
  group_by(session_id, trial_type) %>%
  summarise(block2_mean = mean(trial_latency[block_name == "BLOCK2"]),
            block3_mean = mean(trial_latency[block_name == "BLOCK3"]),
            block5_mean = mean(trial_latency[block_name == "BLOCK5"]),
            block6_mean = mean(trial_latency[block_name == "BLOCK6"]),
            block2_3_mean = mean(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK3"]),
            block5_6_mean = mean(trial_latency[block_name == "BLOCK5" | block_name == "BLOCK6"]),
            block2_5_sd = sd(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK5"]),
            block3_6_sd = sd(trial_latency[block_name == "BLOCK3" | block_name == "BLOCK6"]),
            block_all_sd = sd(trial_latency),
            D1a = (block5_mean - block2_mean) / block2_5_sd,
            D1b = (block6_mean - block3_mean) / block3_6_sd,
            IAT_D1 = round(mean(D1a, D1b), 3),
            IAT_D1_overall = round((block5_6_mean - block2_3_mean) / block_all_sd, 3)) %>% 
  select(session_id, trial_type, IAT_D1, IAT_D1_overall) %>%
  left_join(parameters, by = "session_id") 

# convert from long to wide
D1_scores_by_tt_wide_1 <-
  D1_scores_by_tt %>%
  select(session_id, trial_type, IAT_D1, task_name) %>%
  spread(trial_type, IAT_D1)

D1_scores_by_tt_wide_2 <-
  D1_scores_by_tt %>%
  select(session_id, trial_type, IAT_D1_overall) %>%
  spread(trial_type, IAT_D1_overall) %>%
  rename(men_overall = men,
         women_overall = women,
         arts_overall = arts,
         science_overall = science)

D1_scores_by_tt_wide <- 
  left_join(D1_scores_by_tt_wide_1, D1_scores_by_tt_wide_2, by = "session_id")


# get demographics data ---------------------------------------------------


library(foreign)
#other_tasks_data <- read.spss("/Users/Ian/Dropbox overflow/Data/Gender-Science IAT.public.2003-2015.sav")
#other_tasks_df <- as.data.frame(other_tasks_data)

#save(other_tasks_df, file = "/Users/Ian/git/project-implicit-gender-science-IAT-data-reanalysis/Gender-Science IAT.public.2003-2015.RData")
load(file = "/Users/Ian/git/project-implicit-gender-science-IAT-data-reanalysis/Gender-Science IAT.public.2003-2015.RData")

demographics <-
  other_tasks_df %>%
  select(session_id, 
         year,
         age, 
         sex, 
         country) %>%
  filter(year == 2003,
         sex == "f" | sex == "m")

IAT_and_demographics <-
  left_join(D1_scores_by_tt_wide, demographics, by = "session_id") %>%
  filter(!is.na(sex)) %>%
  rename(IAT_block_order = task_name)

#save(IAT_and_demographics, file = "/Users/Ian/git/project-implicit-gender-science-IAT-data-reanalysis/Gender-Science IAT 2003 trial type D1 scores.RData")

