library(dplyr)
library(readr)
library(DescTools)
library(caret)      # For confusion matrix
library(ROCR)
library(pscl)
library(precrec)

library(ggplot2)
library(wesanderson)

ssri_metscore_ad <- read.table("ssri_metscore_ad.tsv", sep = "\t", header = TRUE)
ssri_testing_data <- readRDS("ssri_testing_data.rds")
ssri_testing_data <- rename(ssri_testing_data, ID = "f.eid")

ssri_metscore_ad <- ssri_metscore_ad %>% left_join(ssri_testing_data %>% select(ID, ssri_current_never), by = "ID")
ssri_metscore_ad$total.MetS <- as.numeric(ssri_metscore_ad$total.MetS)
str(ssri_metscore_ad)

ssri_never_filtered <- ssri_metscore_ad %>% filter(!is.na(ssri_current_never) & !is.na(total.MetS))

logis_reg_never <- glm(ssri_current_never ~ scale(total.MetS), family=binomial (link=logit), data = ssri_never_filtered)
summary(logis_reg_never) 
exp(coef(logis_reg_never)) 

pred1 <- predict(logis_reg_never, data = ssri_never_filtered, type="response")
prob <- prediction(pred, ssri_never_filtered$ssri_current_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 

ssri_metscore_ad <- ssri_metscore_ad %>% left_join(ssri_testing_data %>% select(ID, ssri_current_past), by = "ID")
ssri_past_filtered <- ssri_metscore_ad %>% filter(!is.na(ssri_current_past) & !is.na(total.MetS))
logis_reg_past <- glm(ssri_current_past ~ scale(total.MetS), family=binomial (link=logit), data = ssri_past_filtered)
summary(logis_reg_past) 
exp(coef(logis_reg_past)) 
pred2 <- predict(logis_reg_past, data = ssri_past_filtered, type="response")
prob <- prediction(pred2, ssri_past_filtered$ssri_current_past)
roc_auc <- performance(prob, measure = "auc") 
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 

ssri_metscore_ad <- ssri_metscore_ad %>% left_join(ssri_testing_data %>% select(ID, ssri_current_future), by = "ID")
ssri_future_filtered <- ssri_metscore_ad %>% filter(!is.na(ssri_current_future) & !is.na(total.MetS))
logis_reg_future <- glm(ssri_current_future ~ scale(total.MetS), family=binomial (link=logit), data = ssri_future_filtered)
summary(logis_reg_future) 
exp(coef(logis_reg_future))
pred3 <- predict(logis_reg_future, data = ssri_future_filtered, type="response")
prob <- prediction(pred3, ssri_future_filtered$ssri_current_future)
roc_auc <- performance(prob, measure = "auc") 
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]] 
