library(dplyr)
library(readr)
library(DescTools)
library(caret)      # For confusion matrix
library(ROCR)
library(pscl)
library(precrec)

library(ggplot2)
library(wesanderson)

tricyclic_metscore_ad <- read.table("tricyclic_metscore_ad.tsv", sep = "\t", header = TRUE)
tricyclic_testing_data <- readRDS("tricyclic_testing_data.rds")
tricyclic_testing_data <- rename(tricyclic_testing_data, ID = "f.eid")

tricyclic_metscore_ad <- tricyclic_metscore_ad %>% left_join(tricyclic_testing_data %>% select(ID, tricyclic_current_never), by = "ID")
tricyclic_metscore_ad$total.MetS <- as.numeric(tricyclic_metscore_ad$total.MetS)
str(tricyclic_metscore_ad)

tricyclic_never_filtered <- tricyclic_metscore_ad %>% filter(!is.na(tricyclic_current_never) & !is.na(total.MetS))

logis_reg_never <- glm(tricyclic_current_never ~ scale(total.MetS), family=binomial (link=logit), data = tricyclic_never_filtered)
summary(logis_reg_never) 
exp(coef(logis_reg_never)) 

pred1 <- predict(logis_reg_never, data = tricyclic_never_filtered, type="response")
prob <- prediction(pred1, tricyclic_never_filtered$tricyclic_current_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 

tricyclic_metscore_ad <- tricyclic_metscore_ad %>% left_join(tricyclic_testing_data %>% select(ID, tricyclic_current_past), by = "ID")
tricyclic_past_filtered <- tricyclic_metscore_ad %>% filter(!is.na(tricyclic_current_past) & !is.na(total.MetS))
logis_reg_past <- glm(tricyclic_current_past ~ scale(total.MetS), family=binomial (link=logit), data = tricyclic_past_filtered)
summary(logis_reg_past) 
exp(coef(logis_reg_past)) 
pred2 <- predict(logis_reg_past, data = tricyclic_past_filtered, type="response")
prob <- prediction(pred2, tricyclic_past_filtered$tricyclic_current_past)
roc_auc <- performance(prob, measure = "auc") 
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 

tricyclic_metscore_ad <- tricyclic_metscore_ad %>% left_join(tricyclic_testing_data %>% select(ID, tricyclic_current_future), by = "ID")
tricyclic_future_filtered <- tricyclic_metscore_ad %>% filter(!is.na(tricyclic_current_future) & !is.na(total.MetS))
logis_reg_future <- glm(tricyclic_current_future ~ scale(total.MetS), family=binomial (link=logit), data = tricyclic_future_filtered)
summary(logis_reg_future) 
exp(coef(logis_reg_future))
pred3 <- predict(logis_reg_future, data = tricyclic_future_filtered, type="response")
prob <- prediction(pred3, tricyclic_future_filtered$tricyclic_current_future)
roc_auc <- performance(prob, measure = "auc") 
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 
saveRDS(tricyclic_metscore_ad, "tricyclic_metscore_ad.rds")

###check if ssri scores predict tricyclic antidepressants
ssri_metscore_ad <- read_tsv("ssri_metscore_ad.tsv")
ssri_metscore_ad <- left_join(ssri_metscore_ad, tricyclic_metscore_ad %>% select(ID, tricyclic_current_never), by = "ID")
ssri_filter <- ssri_metscore_ad %>% filter(!is.na(tricyclic_current_never), !is.na(total.MetS))
ssri_model <- glm(tricyclic_current_never ~ scale(total.MetS), family=binomial (link=logit), data = ssri_filter)
exp(coef(ssri_model))
summary(ssri_model)
pred1 <- predict(ssri_model, data = ssri_filter, type="response")
prob <- prediction(pred1, ssri_filter$tricyclic_current_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 


ssri_testing_data <- readRDS("ssri_testing_data.rds")
ssri_testing_data <- rename(ssri_testing_data, ID = "f.eid")
ssri_metscore_ad <- ssri_metscore_ad %>% left_join(ssri_testing_data %>% select(ID, ssri_current_never), by = "ID")

###check if tca scores predict ssri antidepressants
tricyclic_metscore_ad <- left_join(tricyclic_metscore_ad, ssri_metscore_ad %>% select(ID, ssri_current_never), by = "ID")
tca_filter <- tricyclic_metscore_ad %>% filter(!is.na(ssri_current_never), !is.na(total.MetS))
tca_model <- glm(ssri_current_never ~ scale(total.MetS), family=binomial (link=logit), data = tca_filter)
exp(coef(tca_model))
summary(tca_model)
pred2 <- predict(tca_model, data = tca_filter, type="response")
prob <- prediction(pred2, tca_filter$ssri_current_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 
