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

### AUC plot
labels1 <- one_filtered$one_year_never
labels2 <- one_to_three_fil$one_to_three_never
labels3 <- three_to_five_fil$three_to_five_never
labels4 <- five_fil$longer_five_never

labels1 <- as.numeric(as.character(labels1))
labels2 <- as.numeric(as.character(labels2))
labels3 <- as.numeric(as.character(labels3))
labels4 <- as.numeric(as.character(labels4))

scores_list <- list(pred1, pred2, pred3, pred4)
labels_list <- list(labels1, labels2, labels3, labels4)

dsids <- c(1, 2, 3, 4)  
modnames <- c("<1 Year/Never", "1~3 Years/Never", "3~5 Years/Never", ">5 Years/Never")  
smmdat <- mmdata(scores = scores_list, labels = labels_list, dsids = dsids, modnames = modnames)

mmcurves <- evalmod(smmdat)
autoplot(mmcurves, "PRC")
autoplot(mmcurves, "ROC")

auc_values <- auc(mmcurves)
auc_values <- auc_values[-c(2,4,6,8), ]  #remove PR AUCs
auc_values$aucs <- as.numeric(as.character(auc_values$aucs))

library(ggplot2)
library(wesanderson)

roc <- autoplot(mmcurves, "ROC")
pal <- wes_palette("Zissou1", type = "continuous")
pal4 <- wes_palette("Zissou1", 4)

roc_plot <- roc + 
  geom_text(
    data = auc_values, aes(x = 0.6, y = 0.2 - 0.05 * dsids,
                           label = paste0(modnames, ": AUC= ", format(round(aucs, 2), 
                           nsmall = 2))), size = 3, inherit.aes = FALSE
  ) + 
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 15),     # rotate x labels
    axis.text.y  = element_text(size = 15),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 15),
    legend.position = "right",
    plot.title = element_text(size = 25, face = "bold")
  ) + 
  scale_color_manual(values = pal4) +
  theme_minimal() +
  geom_line(linewidth = 0.7) +
  labs(
    title = "ROC Curves",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Exposure Duration"    
  ) 

save.image("predict_duration.RData")
