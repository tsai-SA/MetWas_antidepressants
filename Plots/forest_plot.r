library(ggplot2)
library(dplyr)
library(httpgd)
hgd()


###AUC plot
metscore_ad <- readRDS("metscore_ad_v2.rds")

library(readr)
library(DescTools)
library(caret)      # For confusion matrix
library(ROCR)
library(pscl)
library(assertthat)
library(precrec)
library(ggplot2)
library(grid)

str(metscore_ad)


never_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Never) & !is.na(total.MetS))
logis_reg_never <- glm(Current_vs_Never ~ scale(total.MetS), family=binomial (link=logit), data = never_filtered)
pred1 <- predict(logis_reg_never, data = never_filtered, type="response")
labels1 <- never_filtered$Current_vs_Never
labels1 <- as.numeric(as.character(labels1))

past_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Past) & !is.na(total.MetS))
logis_reg_past <- glm(Current_vs_Past ~ scale(total.MetS), family=binomial (link=logit), data = past_filtered)
pred2 <- predict(logis_reg_past, data = past_filtered, type="response")
labels2 <- past_filtered$Current_vs_Past
labels2 <- as.numeric(as.character(labels2))

future_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Future) & !is.na(total.MetS))
logis_reg_future <- glm(Current_vs_Future ~ scale(total.MetS), family=binomial (link=logit), data = future_filtered)
pred3 <- predict(logis_reg_future, data = future_filtered, type="response")
labels3 <- future_filtered$Current_vs_Future
labels3 <- as.numeric(as.character(labels3))

scores_list <- list(pred1, pred2, pred3)
labels_list <- list(labels1, labels2, labels3)
dsids <- c(1, 2, 3)  
modnames <- c("Current vs Never", "Current vs Past", "Current vs Future")  
# Prepare the data for precrec
smmdat <- mmdata(scores = scores_list, labels = labels_list, dsids = dsids, modnames = modnames)
mmcurves <- evalmod(smmdat)
autoplot(mmcurves, "PRC")
autoplot(mmcurves, "ROC")

auc_values <- auc(mmcurves)
auc_values <- auc_values[-c(2,4,6), ]  #remove PR AUCs
auc_values$aucs <- as.numeric(as.character(auc_values$aucs))
roc <- autoplot(mmcurves, "ROC") + geom_line(linewidth = 1.5)
roc_plot <- roc + geom_text(data = auc_values,
                aes(x = 0.6, y = 0.2 - 0.05 * dsids,
                    label = paste0(modnames, ": AUC= ", format(round(aucs, 2), nsmall = 2)),
                ), size = 5, 
                    inherit.aes = FALSE) + theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 20)
  ) + scale_color_manual(values = wes_palette("Darjeeling1", n =3 ))



#### auc duration

metscore_ad_filtered1 <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(one_year_never))
one_model <- glm(one_year_never ~ scale(total.MetS), family=binomial (link=logit), data = metscore_ad_filtered1)
exp(coef(one_model))

metscore_ad_filtered2 <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(one_to_three_never))
three_model <- glm(one_to_three_never ~ scale(total.MetS), family=binomial (link=logit), data = metscore_ad_filtered2)
exp(coef(three_model))

metscore_ad_filtered3 <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(three_to_five_never))
five_model <- glm(three_to_five_never ~ scale(total.MetS), family=binomial (link=logit), data = metscore_ad_filtered3)
exp(coef(five_model))

metscore_ad_filtered4 <- metscore_ad %>% filter(!is.na(total.MetS), !is.na(longer_five_never))
long_model <- glm(longer_five_never ~ scale(total.MetS), family=binomial (link=logit), data = metscore_ad_filtered4)
exp(coef(long_model))

pred1 <- predict(one_model, type = "response")
labels1 <- metscore_ad_filtered1$one_year_never

pred2 <- predict(three_model, type = "response")
labels2 <- metscore_ad_filtered2$one_to_three_never

pred3 <- predict(five_model, type = "response")
labels3 <- metscore_ad_filtered3$three_to_five_never

pred4 <- predict(long_model, type = "response")
labels4 <- metscore_ad_filtered4$longer_five_never

labels1 <- as.numeric(as.character(labels1))
labels2 <- as.numeric(as.character(labels2))
labels3 <- as.numeric(as.character(labels3))
labels4 <- as.numeric(as.character(labels4))

scores_list <- list(pred1, pred2, pred3, pred4)
labels_list <- list(labels1, labels2, labels3, labels4)
dsids <- c(1, 2, 3, 4)  
modnames <- c("<1 Year/Never", "1~3 Years/Never", "3~5 Years/Never", ">5 Years/Never")  
# Prepare the data for precrec
smmdat <- mmdata(scores = scores_list, labels = labels_list, dsids = dsids, modnames = modnames)

# Evaluate and plot PR curves
mmcurves <- evalmod(smmdat)
autoplot(mmcurves, "PRC")
autoplot(mmcurves, "ROC")


# Display ROC AUC values
auc_values <- auc(mmcurves)
auc_values <- auc_values[-c(2,4,6,8), ]  #remove PR AUCs
auc_values$aucs <- as.numeric(as.character(auc_values$aucs))
duration_roc <- autoplot(mmcurves, "ROC") + geom_line(linewidth = 1.5)

cols <- wes_palette("Darjeeling1", n = 5)
cols_skip <- cols[-4]

# [1] "#F1BB7B" "#FD6467" "#5B1A18" "#D67236" "#E6A0C4"


duration_roc_plot <- duration_roc + geom_text(data = auc_values,
                aes(x = 0.6, y = 0.2 - 0.05 * dsids,
                    label = paste0(modnames, ": AUC= ", format(round(aucs, 2), nsmall = 2)),
                ), size = 5, 
                    inherit.aes = FALSE) + theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 20)
  ) + scale_color_manual(values = cols_skip)


############# forest plot

odd_ratio <- data.frame(
  "Case-control comparison group" = c("Current/Never", "Current/Past", "Current/Future", "<1 Year/Never", "1~3 Years/Never", "3~5 Years/Never", ">5 Years/Never"),
  "Case(n)" = c(2749, 2749, 2749, 785, 741, 403, 820),  
  "Control(n)" = c(38136, 30420, 23584, 38136, 38136, 38136, 38136),  
  "OR" = c(exp(coef(logis_reg_never)[2]), exp(coef(logis_reg_past)[2]), exp(coef(logis_reg_future)[2]), exp(coef(one_model)[2]), exp(coef(three_model)[2]), exp(coef(five_model)[2]), exp(coef(long_model)[2])),
  "CI_lower" = c(exp(confint(logis_reg_never)[2,1]), exp(confint(logis_reg_past)[2,1]), exp(confint(logis_reg_future)[2,1]), exp(confint(one_model)[2,1]), exp(confint(three_model)[2,1]), exp(confint(five_model)[2,1]), exp(confint(long_model)[2,1])),
  "CI_upper" = c(exp(confint(logis_reg_never)[2,2]), exp(confint(logis_reg_past)[2,2]), exp(confint(logis_reg_future)[2,2]), exp(confint(one_model)[2,2]), exp(confint(three_model)[2,2]), exp(confint(five_model)[2,2]), exp(confint(long_model)[2,2]))
)

library(grid)
install.packages("forestploter")
library(forestploter)

odd_ratio$OR <- format(round(odd_ratio$OR, 2), nsmall = 2)
odd_ratio$CI_lower <- format(round(odd_ratio$CI_lower, 2), nsmall = 2)
odd_ratio$CI_upper <- format(round(odd_ratio$CI_upper, 2), nsmall = 2)
odd_ratio <- odd_ratio %>%
  mutate("Odd Ratio (95% CI)" = paste0(OR, " (", CI_lower, "-", CI_upper, ")"))

odd_ratio <- odd_ratio[, c("Case.control.comparison.group", "Case.n.", "Control.n.", "OR", "CI_lower", "CI_upper", "Odd Ratio (95% CI)")]

odd_ratio <- odd_ratio %>% rename("Case/Control" = "Case.control.comparison.group")
odd_ratio <- odd_ratio %>% rename("Case(n)" = Case.n.)
odd_ratio <- odd_ratio %>% rename("Control(n)" = Control.n.)

odd_ratio$` ` <- paste(rep(" ", 28), collapse = " ")  ##empty column for plot space
odd_ratio <- odd_ratio %>% relocate(` `, .after = "Odd Ratio (95% CI)")

odd_ratio$OR <- as.numeric(odd_ratio$OR)
odd_ratio$CI_lower <- as.numeric(odd_ratio$CI_lower)
odd_ratio$CI_upper <- as.numeric(odd_ratio$CI_upper)


library(forestploter)
library(grid)

tm <- forest_theme(
  base_size    = 15,
  ci_col       = "#ED6803", # color of the confidence interval lines
  ci_pch       = 15, #point shape
  ci_cex = 3, # point size
  ci_lwd = 2,
  refline_gp   = gpar(col = "grey50"), #reference line
  
  # force all table cells to white
  core = list(
    padding = unit(c(12, 4), "mm"),
    bg_params = list(
      fill = rep("white", 7)  # repeat enough times for all rows
    )
  ),
  
  # full plot background
  vp.background = gpar(fill = "white", col = NA)
)


forest_plot <- forest(
                    odd_ratio[, c(1:3, 7:8)],
                    est = odd_ratio$OR,
                    lower = odd_ratio$CI_lower,
                    upper = odd_ratio$CI_upper,
                    sizes = rep(0.5, nrow(odd_ratio)),
                    ref_line = 1,
                    ci_column = 5,
                    xlim = c(0.9, 1.6),
                    ticks_at = c(1, 1.25, 1.5),
                    col_widths = c(1, 4, 4, 6, 1),
                        theme = tm,
                        gp = gpar(
                            col = "#ED6803",
                            fill = "#ED6803"
                        )
                    )


library(patchwork)

combined <- (venn + all_auc_plot) / forest_plot +
  plot_annotation(
    tag_levels = "a"
  )

combined

hgd()
