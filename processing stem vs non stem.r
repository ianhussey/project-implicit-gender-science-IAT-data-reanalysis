# title: exploring project implicit gender-science IAT data 
# author: Ian Hussey (ian.hussey@ugent.be)
# license: 
#   code: GPLv3+    https://www.gnu.org/licenses/quick-guide-gplv3.en.html
#   data: CC-0      https://creativecommons.org/publicdomain/zero/1.0/

# notes:
# Currently processes IAT data from 2003, which I chose randomly. 
# The IATs change across years, so need some code changes to extract other years. 
# Occupation data was not available until 06 or 07 according to the demographics data.
# So, chose a year (based on IAT content face validity?) and tweak code to extract 
# this data instead.
# NB gender identity data is also available! The could be cool cross sectional 
# or timepoint comparison opportunities here.


# dependencies ------------------------------------------------------------


library(tidyverse)
library(schoRsch)


# acquire data ------------------------------------------------------------


#gender_science_data_2015 <- read.delim("/Users/Ian/Dropbox overflow/Data/Gender-Science IAT raw data 2003-2015/iat2015.txt")

#save(gender_science_data_2015, file = "/Users/Ian/Dropbox overflow/Data/Gender-Science IAT raw data 2015.RData")
load(file = "/Users/Ian/Dropbox overflow/Data/Gender-Science IAT raw data 2015.RData")


# Ss with complete data ---------------------------------------------------


# list participants with the right stimuli in their IAT trials 

# trial_types <-
#   subs_with_correct_trials %>%
#   distinct(trial_name)
# write.csv(trial_types, "~/Downloads/trial types 2015.csv")

subs_with_correct_trials <- 
  gender_science_data_2015 %>%
  mutate(trial_type = ifelse(trial_name == "Man" |
                               trial_name == "Son"	|
                               trial_name == "Grandpa"	|
                               trial_name == "Uncle"	|
                               trial_name == "Husband"	|
                               trial_name == "Father" |
                               trial_name == "Boy"	|
                               trial_name == "Male", "men",
                             ifelse(trial_name == "Woman"	|
                                      trial_name == "Wife" |
                                      trial_name == "Girl" |
                                      trial_name == "Mother" |
                                      trial_name == "Daughter" |
                                      trial_name == "Aunt" |
                                      trial_name == "Female" |
                                      trial_name == "Grandma", "women",
                                    ifelse(trial_name == "Literature" |
                                             trial_name == "Humanities" |
                                             trial_name == "Music"	|
                                             trial_name == "Philosophy" |
                                             trial_name == "English"	|
                                             trial_name == "History"	|
                                             trial_name == "Arts", "arts", 
                                           ifelse(trial_name == "Physics" |
                                                    trial_name == "Math" |
                                                    trial_name == "Geology" |
                                                    trial_name == "Biology" |
                                                    trial_name == "Chemistry" |
                                                    trial_name == "Astronomy" |
                                                    trial_name == "Engineering", "science", NA))))) %>%
  filter(!is.na(trial_type))

# list participants with the right amount of trials 
subs_with_complete_data <- 
  subs_with_correct_trials %>%
  group_by(session_id) %>%
  summarize(trials = n()) %>%
  filter(trials == 200) %>%
  ungroup()

# select out these participants
screened_data <-
  left_join(subs_with_complete_data, subs_with_correct_trials, by = "session_id") %>%
  filter(block_name == "BLOCK2" |     # select only their test blocks
           block_name == "BLOCK3" | 
           block_name == "BLOCK5" | 
           block_name == "BLOCK6") %>%
  select(-trials)


# IAT block order parameter -----------------------------------------------


parameters <- 
  screened_data %>%
  distinct(session_id, .keep_all = TRUE) %>%
  select(session_id, 
         task_name) %>%
  rename(IAT_block_order = task_name)


# D1 scores ---------------------------------------------------------------


D1_scores <- 
  screened_data %>%
  filter(trial_latency <= 10000) %>%
  group_by(session_id) %>%
  summarise(block2_mean = mean(trial_latency[block_name == "BLOCK2"]),
            block3_mean = mean(trial_latency[block_name == "BLOCK3"]),
            block5_mean = mean(trial_latency[block_name == "BLOCK5"]),
            block6_mean = mean(trial_latency[block_name == "BLOCK6"]),
            block2_5_sd = sd(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK5"]),
            block3_6_sd = sd(trial_latency[block_name == "BLOCK3" | block_name == "BLOCK6"])) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(D1a = (block5_mean - block2_mean) / block2_5_sd,
         D1b = (block6_mean - block3_mean) / block3_6_sd,
         D1 = round(mean(c(D1a, D1b)), 3)) %>% 
  ungroup() %>%
  select(session_id, D1)


# D1 scores by tt ---------------------------------------------------------


D1_scores_by_tt <- 
  screened_data %>%
  filter(trial_latency <= 10000) %>%
  group_by(session_id, trial_type) %>%
  summarise(block2_mean = mean(trial_latency[block_name == "BLOCK2"]),
            block3_mean = mean(trial_latency[block_name == "BLOCK3"]),
            block5_mean = mean(trial_latency[block_name == "BLOCK5"]),
            block6_mean = mean(trial_latency[block_name == "BLOCK6"]),
            block2_5_sd = sd(trial_latency[block_name == "BLOCK2" | block_name == "BLOCK5"]),
            block3_6_sd = sd(trial_latency[block_name == "BLOCK3" | block_name == "BLOCK6"])) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(D1a = (block5_mean - block2_mean) / block2_5_sd,
         D1b = (block6_mean - block3_mean) / block3_6_sd,
         D1 = round(mean(c(D1a, D1b)), 3)) %>% 
  select(session_id, trial_type, D1) %>%
  select(session_id, trial_type, D1) %>%
  spread(trial_type, D1) %>%
  rename(D1_men = men,
         D1_women = women,
         D1_arts = arts,
         D1_science = science)

IAT_data <- 
  D1_scores %>%
  left_join(D1_scores_by_tt, by = "session_id") %>%
  left_join(parameters, by = "session_id") 


# get self report data ----------------------------------------------------


library(foreign)
#other_tasks_data <- read.spss("/Users/Ian/Dropbox overflow/Data/Gender-Science IAT.public.2003-2015.sav")
#other_tasks_df <- as.data.frame(other_tasks_data)

#save(other_tasks_df, file = "/Users/Ian/git/project-implicit-gender-science-IAT-data-reanalysis/Gender-Science IAT.public.2003-2015.RData")
load(file = "/Users/Ian/Dropbox overflow/Data/Gender-Science IAT.public.2003-2015.RData")


self_reports <-
  other_tasks_df %>%
  select(session_id, 
         year,
         age, 
         sex, 
         occupation,
         genident) %>%
  filter(year == 2015, 
         age >= 18,
         sex == "m" | sex == "f")


# seperate participants by sex and stem/nonstem occupation

# occupation
# 15-1000	Computer/Math - Computer Specialists
# 15-2000	Computer/Math - Math Scientists
# 15-3000	Computer/Math - Math Technicians
# 17-1000	Engineers/Architects - Architects, Surveyors, Cartographers
# 17-2000	Engineers/Architects - Engineers
# 17-3000	Engineers/Architects - Drafters, Engineering and Mapping Technicians
# 19-1000	Science - Life Scientists
# 19-2000	Science - Physical scientists
# 19-3000	Science - Social Scientists
# 19-4000	Science - Life, Physical, Social Science Technicians
# 29-1000	Healthcare - Diagnosing and Treating Practitioners (MD, Dentist, etc.)
# 29-2000	Healthcare - Technologists and Technicians

stem_participants <-
  self_reports %>%
  filter(occupation == "15-1000" | 
           occupation == "15-1000" | 
           occupation == "15-2000" | 
           occupation == "15-3000" | 
           occupation == "17-1000" | 
           occupation == "17-2000" | 
           occupation == "17-3000" | 
           occupation == "19-1000" | 
           occupation == "19-2000" | 
           occupation == "19-3000" | 
           occupation == "19-4000" |
           occupation == "29-1000" | 
           occupation == "29-2000") %>%
  mutate(group = "stem")

nonstem_participants <-
  self_reports %>%
  filter(occupation == "43-1000" |     # all other non stem career categories,
           occupation == "43-3000" |   # with the exception of student, retired, and unemployed.
           occupation == "43-4000" |   # these are removed because they don't inform us to training.
           occupation == "43-5000" |
           occupation == "43-6000" |
           occupation == "43-9000" |
           occupation == "27-1000" |
           occupation == "27-2000" |
           occupation == "27-3000" |
           occupation == "27-4000" |
           occupation == "13-1000" |
           occupation == "13-2000" |
           occupation == "47-1000" |
           occupation == "47-2000" |
           occupation == "47-3000" |
           occupation == "47-5000" |
           occupation == "47-4000" |
           occupation == "25-1000" |
           occupation == "25-2000" |
           occupation == "25-3000" |
           occupation == "25-4000" |
           occupation == "25-9000" |
           occupation == "45-1000" |
           occupation == "45-2000" |
           occupation == "45-3000" |
           occupation == "45-4000" |
           occupation == "45-9000" |
           occupation == "35-1000" |
           occupation == "35-2000" |
           occupation == "35-3000" |
           occupation == "35-9000" |
           occupation == "31-1000" |
           occupation == "31-2000" |
           occupation == "31-9000" |
           occupation == "00-0000" |
           occupation == "23-1000" |
           occupation == "23-2000" |
           occupation == "37-1000" |
           occupation == "37-2000" |
           occupation == "37-3000" |
           occupation == "11-0000" |
           occupation == "11-2000" |
           occupation == "11-3000" |
           occupation == "11-9000" |
           occupation == "55-1000" |
           occupation == "55-2000" |
           occupation == "55-3000" |
           occupation == "51-1000" |
           occupation == "51-2000" |
           occupation == "51-3000" |
           occupation == "51-4000" |
           occupation == "51-5000" |
           occupation == "51-6000" |
           occupation == "51-7000" |
           occupation == "51-8000" |
           occupation == "51-9000" |
           occupation == "33-1000" |
           occupation == "33-2000" |
           occupation == "33-3000" |
           occupation == "33-9000" |
           occupation == "49-1000" |
           occupation == "49-2000" |
           occupation == "49-3000" |
           occupation == "49-9000" |
           occupation == "41-1000" |
           occupation == "41-2000" |
           occupation == "41-3000" |
           occupation == "41-4000" |
           occupation == "41-9000" |
           occupation == "39-1000" |
           occupation == "39-2000" |
           occupation == "39-3000" |
           occupation == "39-4000" |
           occupation == "39-5000" |
           occupation == "39-6000" |
           occupation == "39-9000" |
           occupation == "21-1000" |
           occupation == "21-2000" |
           occupation == "53-1000" |
           occupation == "53-2000" |
           occupation == "53-3000" |
           occupation == "53-4000" |
           occupation == "53-5000" |
           occupation == "53-7000" |
           occupation == "53-6000") %>%
  mutate(group = "nonstem")

combined_self_reports <- 
  rbind(stem_participants, 
        nonstem_participants)


# join iat and demographics -----------------------------------------------


IAT_and_self_reports <-
  inner_join(IAT_data, combined_self_reports, by = "session_id") 

IAT_and_self_reports %>% save(file = "/Users/Ian/git/project-implicit-gender-science-IAT-data-reanalysis/2015 data tt D1 scores.RData")
IAT_and_self_reports %>% write.csv(file = "/Users/Ian/git/project-implicit-gender-science-IAT-data-reanalysis/2015 data tt D1 scores.csv", row.names = FALSE)

