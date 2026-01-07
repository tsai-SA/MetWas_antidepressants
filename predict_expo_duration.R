ad_baseline_duration <- readRDS("ad_baseline_duration.rds")
metscore_ad <- readRDS("metscore_ad.rds")

install.packages("pscl")
library(dplyr)
library(readr)
library(DescTools)
library(caret)      # For confusion matrix
library(ROCR)
library(pscl)

ad_baseline_duration <- rename(ad_baseline_duration, ID = f.eid)
metscore_ad <- metscore_ad %>% left_join(ad_baseline_duration %>% select(ID, duration_class), by = "ID")
metscore_ad$duration_class <- as.factor(metscore_ad$duration_class)
table(metscore_ad$duration_class)

metscore_ad <- metscore_ad %>%
  mutate(
    one_year_never  = ifelse(Current_vs_Never == "0", 0, ifelse(duration_class == "less_one", 1, NA)),
    one_to_three_never  = ifelse(Current_vs_Never == "0", 0, ifelse(duration_class == "one_to_three", 1, NA)),
    three_to_five_never  = ifelse(Current_vs_Never == "0", 0, ifelse(duration_class == "three_to_five", 1, NA)),
    longer_five_never  = ifelse(Current_vs_Never == "0", 0, ifelse(duration_class == "longer_than_five", 1, NA))
  )

table(metscore_ad$one_year_never)
table(metscore_ad$one_to_three_never)
table(metscore_ad$three_to_five_never)
table(metscore_ad$longer_five_never)

metscore_ad$one_year_never <- as.factor(metscore_ad$one_year_never)
metscore_ad$one_to_three_never <- as.factor(metscore_ad$one_to_three_never)
metscore_ad$three_to_five_never <- as.factor(metscore_ad$three_to_five_never)
metscore_ad$longer_five_never <- as.factor(metscore_ad$longer_five_never)

one_filtered <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(one_year_never))
one_model <- glm(one_year_never ~ scale(total.MetS), family=binomial (link=logit), data = one_filtered)
summary(one_model)
exp(coef(one_model))
pred <- predict(one_model, data = one_filtered, type="response")
prob <- prediction(pred, one_filtered$one_year_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]]

one_to_three_fil <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(one_to_three_never))
one_to_three_model <- glm(one_to_three_never ~ scale(total.MetS), family=binomial (link=logit), data = one_to_three_fil)
summary(one_to_three_model)
exp(coef(one_to_three_model))
pred <- predict(one_to_three_model, data = one_to_three_fil, type="response")
prob <- prediction(pred, one_to_three_fil$one_to_three_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]]

three_to_five_fil <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(three_to_five_never))
three_to_five_model <- glm(three_to_five_never ~ scale(total.MetS), family=binomial (link=logit), data = three_to_five_fil)
summary(three_to_five_model)
exp(coef(three_to_five_model))
pred <- predict(three_to_five_model, data = three_to_five_fil, type="response")
prob <- prediction(pred, three_to_five_fil$three_to_five_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]]

five_fil <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(longer_five_never))
five_fil_model <- glm(longer_five_never ~ scale(total.MetS), family=binomial (link=logit), data = five_fil)
summary(five_fil_model)
exp(coef(five_fil_model))
pred <- predict(five_fil_model, data = five_fil, type="response")
prob <- prediction(pred, five_fil$longer_five_never)
roc_auc <- performance(prob, measure = "auc")
roc_auc@y.values[[1]]
pr_auc <- performance(prob, measure = "aucpr")
pr_auc@y.values[[1]]

save.image("predict_duration.RData")
