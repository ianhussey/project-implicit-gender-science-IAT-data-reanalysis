# title: exploring project implicit gender-science IAT data 
# author: Ian Hussey (ian.hussey@ugent.be)
# license: 
#   code: GPLv3+    https://www.gnu.org/licenses/quick-guide-gplv3.en.html
#   data: CC-0      https://creativecommons.org/publicdomain/zero/1.0/


# data acquisition --------------------------------------------------------


load("/Users/Ian/git/project-implicit-gender-science-IAT-data-reanalysis/Gender-Science IAT 2003 trial type D1 scores.RData")


# measurement invariance --------------------------------------------------


# dependencies
library(lavaan)
library(semTools)
library(reshape2)

# model structures
model_a <- 
"implicit_stereotypes =~  
lam1 * science + 
lam2 * arts + 
lam3 * men + 
lam4 * women"

model_b <- 
"gender_stereotypes =~ 
lam1 * men + 
lam2 * women
subject_stereotypes =~ 
lam3 * science + 
lam4 * arts
gender_stereotypes ~ subject_stereotypes"

model_a_2 <- 
"implicit_stereotypes =~  
lam1 * science_overall + 
lam2 * arts_overall + 
lam3 * men_overall + 
lam4 * women_overall"


# trial type correlations
IAT_and_demographics %>% 
  select(arts, science, men, women, 
         arts_overall, science_overall, 
         men_overall, women_overall) %>%
  cor()

## 1. trial type correlations are higher for the overall scorings (c0.7 vs c0.6)


# IAT block order
measurementInvariance(model = model_a, group = "IAT_block_order", data = IAT_and_demographics)
measurementInvariance(model = model_a_2, group = "IAT_block_order", data = IAT_and_demographics)
measurementInvariance(model = model_b, group = "IAT_block_order", data = IAT_and_demographics)
# B has negative error variances, therefore we examine As

# Chi squared may need to be ignored given that it is sample size dependent and
# we have a very large sample size. If we examine other model fit indicators 
# (AIC, BIC, CFI and RMSEA) we see some change across models between 
# IAT block order, especially a difference in means (as to be expected).

# 2. AIC and BIC both far smaller when using overall D1 scoring (0.59 of the larger) than traditional D1 scoring

# participant gender
measurementInvariance(model = model_a, group = "sex", data = IAT_and_demographics)
measurementInvariance(model = model_a_2, group = "sex", data = IAT_and_demographics)
measurementInvariance(model = model_b, group = "sex", data = IAT_and_demographics)
# B has negative error variances, therefore we examine A 

# 3. AIC and BIC both far smaller when using overall D1 scoring (0.64 of the larger) than traditional D1 scoring

# 4. Model fits under measurement invariance constraints are fine in participant gender comparions,
# but poor in block order comparisons. 



# verbose measurement invariance ------------------------------------------


# # block order
# # configural
# model_0a <- cfa(model_a, group = "IAT_block_order", data = IAT_and_demographics) # free parameters
# summary(model_0a, fit.measures = TRUE, standardized = TRUE)
# 
# model_0b <- cfa(model_b, group = "IAT_block_order", data = IAT_and_demographics) # free parameters
# summary(model_0a, fit.measures = TRUE, standardized = TRUE)
# # B has negative error variances, therefore we employ A in additionally constrained models
# 
# # metric
# model_1a <- cfa(model_a, data = IAT_and_demographics, group = "IAT_block_order", group.equal = c("loadings")) # fixed factor  loadings
# summary(model_1a, fit.measures = TRUE)
# anova(model_1a, model_0a)
# 
# # scalar
# model_2a <- cfa(model_a, data = IAT_and_demographics, group = "IAT_block_order", group.equal = c("loadings", "intercepts")) # fixed loadings and intercepts
# summary(model_2a, fit.measures = TRUE)
# anova(model_2a, model_1a)  
# 
# 
# # participant gender
# # configural
# model_0a <- cfa(model_a, group = "sex", data = IAT_and_demographics) # free parameters
# summary(model_0a, fit.measures = TRUE, standardized = TRUE)
# 
# model_0b <- cfa(model_b, group = "sex", data = IAT_and_demographics) # free parameters
# summary(model_0a, fit.measures = TRUE, standardized = TRUE)
# # B has negative error variances, therefore we employ A in additionally constrained models
# 
# # metric
# model_1a <- cfa(model_a, data = IAT_and_demographics, group = "sex", group.equal = c("loadings")) # fixed factor  loadings
# summary(model_1a, fit.measures = TRUE)
# anova(model_1a, model_0a)
# 
# # scalar
# model_2a <- cfa(model_a, data = IAT_and_demographics, group = "sex", group.equal = c("loadings", "intercepts")) # fixed loadings and intercepts
# summary(model_2a, fit.measures = TRUE)
# anova(model_2a, model_1a)  
# 






