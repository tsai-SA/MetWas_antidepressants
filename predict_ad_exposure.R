library(readr)
library(DescTools)
library(caret)      # For confusion matrix
library(ROCR)
library(pscl)

metscore_ad <- readRDS("metscore_ad.rds")

str(metscore_ad)

logis_reg_never <- glm(Current_vs_Never ~ scale(total.MetS), family=binomial (link=logit), data = metscore_ad)
exp(logis_reg_never$coefficients) # Get odd ratios
summary(logis_reg_never)

logis_reg_past <- glm(Current_vs_Past ~ scale(total.MetS), family=binomial (link=logit), data = metscore_ad)
exp(logis_reg_past$coefficients) 
summary(logis_reg_past)

logis_reg_future <- glm(Current_vs_Future ~ scale(total.MetS), family=binomial (link=logit), data = metscore_ad)
exp(logis_reg_future$coefficients) 
summary(logis_reg_future)

never_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Never) & !is.na(total.MetS))
logis_reg_never <- glm(Current_vs_Never ~ scale(total.MetS), family=binomial (link=logit), data = never_filtered)
exp(logis_reg_never$coefficients) 
summary(logis_reg_never)
pred <- predict(logis_reg_never, data = never_filtered, type="response")
prob <- prediction(pred, never_filtered$Current_vs_Never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]]

past_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Past) & !is.na(total.MetS))
logis_reg_past <- glm(Current_vs_Past ~ scale(total.MetS), family=binomial (link=logit), data = past_filtered)
exp(logis_reg_past$coefficients) 
summary(logis_reg_past)
pred <- predict(logis_reg_past, data = past_filtered, type="response")
prob <- prediction(pred, past_filtered$Current_vs_Past)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]]

future_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Future) & !is.na(total.MetS))
logis_reg_future <- glm(Current_vs_Future ~ scale(total.MetS), family=binomial (link=logit), data = future_filtered)
exp(logis_reg_future$coefficients) 
summary(logis_reg_future)
pred <- predict(logis_reg_future, data = future_filtered, type="response")
prob <- prediction(pred, future_filtered$Current_vs_Future)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]]

save.image("predict_ad_exposure.RData")
