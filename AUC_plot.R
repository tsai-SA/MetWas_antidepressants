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

past_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Past) & !is.na(total.MetS))
logis_reg_past <- glm(Current_vs_Past ~ scale(total.MetS), family=binomial (link=logit), data = past_filtered)
pred2 <- predict(logis_reg_past, data = past_filtered, type="response")
labels2 <- past_filtered$Current_vs_Past

future_filtered <- metscore_ad %>% filter(!is.na(Current_vs_Future) & !is.na(total.MetS))
logis_reg_future <- glm(Current_vs_Future ~ scale(total.MetS), family=binomial (link=logit), data = future_filtered)
pred3 <- predict(logis_reg_future, data = future_filtered, type="response")
labels3 <- future_filtered$Current_vs_Future

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

pred4 <- predict(one_model, data = metscore_ad_filtered1, type = "response")
labels4 <- metscore_ad_filtered1$one_year_never

pred5 <- predict(three_model, data = metscore_ad_filtered2, type = "response")
labels5 <- metscore_ad_filtered2$one_to_three_never

pred6 <- predict(five_model, data = metscore_ad_filtered3, type = "response")
labels6 <- metscore_ad_filtered3$three_to_five_never

pred7 <- predict(long_model, data = metscore_ad_filtered4, type = "response")
labels7 <- metscore_ad_filtered4$longer_five_never

labels1 <- as.numeric(as.character(labels1))
labels2 <- as.numeric(as.character(labels2))
labels3 <- as.numeric(as.character(labels3))
labels4 <- as.numeric(as.character(labels4))
labels5 <- as.numeric(as.character(labels5))
labels6 <- as.numeric(as.character(labels6))
labels7 <- as.numeric(as.character(labels7))   

scores_list <- list(pred1, pred2, pred3, pred4, pred5, pred6, pred7)
labels_list <- list(labels1, labels2, labels3, labels4, labels5, labels6, labels7)
dsids <- c(1, 2, 3, 4, 5, 6, 7)  
modnames <- c("Current/Never", "Current/Past", "Current/Future", "<1 Year/Never", "1~3 Years/Never", "3~5 Years/Never", ">5 Years/Never") 

smmdat <- mmdata(scores = scores_list, labels = labels_list, dsids = dsids, modnames = modnames)

# Evaluate and plot PR curves
mmcurves <- evalmod(smmdat)
autoplot(mmcurves, "PRC")
autoplot(mmcurves, "ROC")

# Display ROC AUC values
auc_values <- auc(mmcurves)
auc_values <- auc_values[-c(2,4,6,8,10,12,14), ]  #remove PR AUCs
auc_values$aucs <- as.numeric(as.character(auc_values$aucs))
all_roc <- autoplot(mmcurves, "ROC") + geom_line(linewidth = 0.7)

cols <- wes_palette("Zissou1", n = 7, type = "continuous")

all_auc_plots <- all_roc + geom_text(data = auc_values,
                            aes(x = 0.7, y = 0.4 - 0.05 * dsids,
                                label = paste0(modnames, ": AUC= ", format(round(aucs, 2), nsmall = 2)),
                            ), size =4, 
                                inherit.aes = FALSE) + theme(
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 15, face = "bold"),
                legend.text = element_text(size = 12)
                ) + scale_color_manual(values = cols) +
                labs(title = NULL) 


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


all_auc_plots <- all_auc_plots + labs(tag = "a") +
  theme(plot.tag = element_text(size = 15, face = "bold"))

forest_plot_gg <- as.ggplot(forest_plot)
forest_plot_gg <- forest_plot_gg + labs(tag = "b") +
  theme(plot.tag = element_text(size = 15, face = "bold"))

design <- "#AA#
           #AA#
           #AA#
           BBBB
           BBBB
            BBBB"

combined <- all_auc_plots / forest_plot_gg + plot_layout(design = design) 

ggsave(
  "auc_forest_plots.png",
  width = 11,   # match Word page width
  height = 8,
  dpi = 600
)
