ssri_main_model_never <- readRDS("ssri_main_model_never.rds")
tricyclic_main_model_never <- readRDS("tricyclic_main_model_never.rds")

colnames(ssri_main_model_never) <- paste0('ssri_', colnames(ssri_main_model_never))
colnames(ssri_main_model_never)

colnames(tricyclic_main_model_never) <- paste0('tca_', colnames(tricyclic_main_model_never))
colnames(tricyclic_main_model_never)

ssri_main_model_never <- rename(ssri_main_model_never, FieldID = ssri_FieldID)
tricyclic_main_model_never <- rename(tricyclic_main_model_never, FieldID = tca_FieldID)

combined_beta <- merge(ssri_main_model_never, tricyclic_main_model_never, by = "FieldID")
combined_beta <- left_join(combined_beta, grouping_met, by = "FieldID")

combined_beta <- combined_beta %>%
    mutate(Sig_all = case_when(
        ssri_p.fdr < 0.05 & tca_p.fdr < 0.05 ~ "Both",
        ssri_p.fdr < 0.05 & tca_p.fdr >= 0.05 ~ "SSRIs/SNRIs",
        ssri_p.fdr > 0.05 & tca_p.fdr < 0.05 ~ "TCAs",
        ssri_p.fdr > 0.05 & tca_p.fdr > 0.05 ~ "Neither"    
    ))

cor.test(combined_beta$ssri_Beta, combined_beta$tca_Beta, 
		use="complete.obs", method="pearson")

cor_ssri_tca <- ggplot(combined_beta, aes(x= ssri_Beta, y = tca_Beta)) + 
	geom_point(size=1, aes(color = Sig_all)) + 
    xlim(-0.25, 0.25) + ylim(-0.25, 0.25) + 
	labs(x = "SSRIs/SNRIs", y = "TCAs") + 
    scale_color_manual(values = wes_palette("Zissou1", n = 4, type = "continuous")) +
    theme(legend.text = element_text(size=12),
          legend.title = element_text(size=12, face = "bold"),
          legend.position = "bottom",
          plot.title = element_text(size=12, hjust = 0),
          axis.text = element_text(size=12),
          axis.title = element_text(size=15, face = "bold")) +
    annotate("text", x = 0.15, y = -0.2, label = "r = 0.91", size = 4) +
    labs(color = "Significant in", title = "Correlation of Current/Never Effect Sizes")


ssri_metscore_ad <- readRDS("ssri_metscore_ad.rds")
tricyclic_metscore_ad <- readRDS("tricyclic_metscore_ad.rds")



###ssri scores predict tca vs tca score predict tca
ssri_metscore_ad <- left_join(ssri_metscore_ad, tricyclic_metscore_ad %>% select(ID, tricyclic_current_never), by = "ID")
ssri_filter1 <- ssri_metscore_ad %>% filter(!is.na(tricyclic_current_never), !is.na(total.MetS))
ssri_pre_tca <- glm(tricyclic_current_never ~ scale(total.MetS), family=binomial (link=logit), data = ssri_filter1)
exp(coef(ssri_pre_tca))
summary(ssri_pre_tca)
pred1 <- predict(ssri_pre_tca, data = ssri_filter, type="response")
labels1 <- ssri_filter1$tricyclic_current_never

tca_filter2 <- tricyclic_metscore_ad %>% filter(!is.na(tricyclic_current_never), !is.na(total.MetS))
tca_pre_tca <- glm(tricyclic_current_never ~ scale(total.MetS), family=binomial (link=logit), data = tca_filter2)
exp(coef(tca_pre_tca))
summary(tca_pre_tca)
pred2 <- predict(tca_pre_tca, data = tca_filter2, type="response")
labels2 <- tca_filter2$tricyclic_current_never


## ssri scores predict ssri vs tca score predict ssri
ssri_filter2 <- ssri_metscore_ad %>% filter(!is.na(ssri_current_never), !is.na(total.MetS))
ssri_pre_ssri <- glm(ssri_current_never ~ scale(total.MetS), family=binomial (link=logit), data = ssri_filter2)
exp(coef(ssri_pre_ssri))
summary(ssri_pre_ssri)
pred3 <- predict(ssri_pre_ssri, data = ssri_filter2, type="response")
labels3 <- ssri_filter2$ssri_current_never

tricyclic_metscore_ad <- left_join(tricyclic_metscore_ad, ssri_metscore_ad %>% select(ID, ssri_current_never), by = "ID")
tca_filter1 <- tricyclic_metscore_ad %>% filter(!is.na(ssri_current_never), !is.na(total.MetS))
tca_pre_ssri <- glm(ssri_current_never ~ scale(total.MetS), family=binomial (link=logit), data = tca_filter1)
exp(coef(tca_pre_ssri))
summary(tca_pre_ssri)
pred4 <- predict(tca_pre_ssri, data = tca_filter1, type="response")
labels4 <- tca_filter1$ssri_current_never

labels1 <- as.numeric(as.character(labels1))
labels2 <- as.numeric(as.character(labels2))
labels3 <- as.numeric(as.character(labels3))
labels4 <- as.numeric(as.character(labels4))


scores_list <- list(pred1, pred2)
labels_list <- list(labels1, labels2)
dsids <- c(1, 2)  
modnames <- c("SSRI/SNRI scores", "TCA scores") 

smmdat <- mmdata(scores = scores_list, labels = labels_list, dsids = dsids, modnames = modnames)

# Evaluate and plot PR curves
mmcurves <- evalmod(smmdat)
autoplot(mmcurves, "PRC")
autoplot(mmcurves, "ROC")

# Display ROC AUC values
auc_values <- auc(mmcurves)
auc_values <- auc_values[-c(2,4), ]  #remove PR AUCs
auc_values$aucs <- as.numeric(as.character(auc_values$aucs))
tca_roc <- autoplot(mmcurves, "ROC") + geom_line(linewidth = 0.7)

cols <- c("#EBCC2A", "#F21A00")

pre_tca <- tca_roc + geom_text(data = auc_values, aes(x = 0.55, y = 0.1 - 0.05 * dsids,
                                label = paste0(modnames, ": AUC= ", format(round(aucs, 2), nsmall = 2)),
                            ), size =3, inherit.aes = FALSE) + 
                            theme(
                                plot.title = element_text(size = 12),
                                axis.text = element_text(size = 12),
                                axis.title = element_text(size = 15, face = "bold"),
                                legend.text = element_text(size = 12)
                                ) + 
                            scale_color_manual(values = cols) +
                            labs(title = "Classification of TCA Current/Never Users") +
                            theme(legend.position = "bottom")


scores_list2 <- list(pred3, pred4)
labels_list2 <- list(labels3, labels4)
dsids2 <- c(1, 2)  
modnames2 <- c("SSRI/SNRI scores", "TCA scores") 

smmdat2 <- mmdata(scores = scores_list2, labels = labels_list2, dsids = dsids2, modnames = modnames2)

# Evaluate and plot PR curves
mmcurves2 <- evalmod(smmdat2)
autoplot(mmcurves, "PRC")
autoplot(mmcurves, "ROC")

# Display ROC AUC values
auc_values2 <- auc(mmcurves2)
auc_values2 <- auc_values2[-c(2,4), ]  #remove PR AUCs
auc_values2$aucs <- as.numeric(as.character(auc_values2$aucs))
ssri_roc <- autoplot(mmcurves2, "ROC") + geom_line(linewidth = 0.7)

cols <- c("#EBCC2A", "#F21A00")

pre_ssri <- ssri_roc + geom_text(data = auc_values2, aes(x = 0.55, y = 0.1 - 0.05 * dsids2,
                                label = paste0(modnames2, ": AUC= ", format(round(aucs, 2), nsmall = 2)),
                            ), size =3, inherit.aes = FALSE) + 
                            theme(
                                plot.title = element_text(size = 12),
                                axis.text = element_text(size = 12),
                                axis.title = element_text(size = 15, face = "bold"),
                                legend.text = element_text(size = 12)
                                ) + 
                            scale_color_manual(values = cols) +
                            labs(title = "Classification of SSRI/SNRI Current/Never Users") +
                            theme(legend.position = "bottom")




cor_ssri_tca + pre_ssri + pre_tca 
ggsave(
  "ad_subclass_plot.png",
  width = 13,   # match Word page width
  height = 6,
  dpi = 600
)

table(combined_beta$Sig_all)



############# forest plot

table(ssri_metscore_ad$ssri_current_never)
table(ssri_metscore_ad$ssri_current_past)
table(ssri_metscore_ad$ssri_current_future)

table(tricyclic_metscore_ad$tricyclic_current_never)
table(tricyclic_metscore_ad$tricyclic_current_past)
table(tricyclic_metscore_ad$tricyclic_current_future)

ssri_never_filtered <- ssri_metscore_ad %>% filter(!is.na(ssri_current_never) & !is.na(total.MetS))
ssri_never <- glm(ssri_current_never ~ scale(total.MetS), family=binomial (link=logit), data = ssri_never_filtered)
ssri_past_filtered <- ssri_metscore_ad %>% filter(!is.na(ssri_current_past) & !is.na(total.MetS))
ssri_past <- glm(ssri_current_past ~ scale(total.MetS), family=binomial (link=logit), data = ssri_past_filtered)
ssri_future_filtered <- ssri_metscore_ad %>% filter(!is.na(ssri_current_future) & !is.na(total.MetS))
ssri_future <- glm(ssri_current_future ~ scale(total.MetS), family=binomial (link=logit), data = ssri_future_filtered)

tca_never_filtered <- tricyclic_metscore_ad %>% filter(!is.na(tricyclic_current_never) & !is.na(total.MetS))
tca_never <- glm(tricyclic_current_never ~ scale(total.MetS), family=binomial (link=logit), data = tca_never_filtered)
tca_past_filtered <- tricyclic_metscore_ad %>% filter(!is.na(tricyclic_current_past) & !is.na(total.MetS))
tca_past <- glm(tricyclic_current_past ~ scale(total.MetS), family=binomial (link=logit), data = tca_past_filtered)
tca_future_filtered <- tricyclic_metscore_ad %>% filter(!is.na(tricyclic_current_future) & !is.na(total.MetS))
tca_future <- glm(tricyclic_current_future ~ scale(total.MetS), family=binomial (link=logit), data = tca_future_filtered)

odd_ratio_ssri <- data.frame(
  "Case-control comparison group" = c("Current/Never", "Current/Past", "Current/Future"),
  "Case(n)" = c(1645, 1645, 1645),  
  "Control(n)" = c(44738, 4574, 3276),  
  "OR" = c(exp(coef(ssri_never)[2]), exp(coef(ssri_past)[2]), exp(coef(ssri_future)[2])),
  "CI_lower" = c(exp(confint(ssri_never)[2,1]), exp(confint(ssri_past)[2,1]), exp(confint(ssri_future)[2,1])),
  "CI_upper" = c(exp(confint(ssri_never)[2,2]), exp(confint(ssri_past)[2,2]), exp(confint(ssri_future)[2,2]))
)

library(grid)
install.packages("forestploter")
library(forestploter)

odd_ratio_ssri$OR <- format(round(odd_ratio_ssri$OR, 2), nsmall = 2)
odd_ratio_ssri$CI_lower <- format(round(odd_ratio_ssri$CI_lower, 2), nsmall = 2)
odd_ratio_ssri$CI_upper <- format(round(odd_ratio_ssri$CI_upper, 2), nsmall = 2)
odd_ratio_ssri <- odd_ratio_ssri %>%
  mutate("Odd Ratio (95% CI)" = paste0(OR, " (", CI_lower, "-", CI_upper, ")"))


odd_ratio_ssri <- odd_ratio_ssri[, c("Case.control.comparison.group", "Case.n.", "Control.n.", "OR", "CI_lower", "CI_upper", "Odd Ratio (95% CI)")]

odd_ratio_ssri <- odd_ratio_ssri %>% rename("Case/Control" = "Case.control.comparison.group")
odd_ratio_ssri <- odd_ratio_ssri %>% rename("Case(n)" = Case.n.)
odd_ratio_ssri <- odd_ratio_ssri %>% rename("Control(n)" = Control.n.)

odd_ratio_ssri$` ` <- paste(rep(" ", 28), collapse = " ")  ##empty column for plot space
odd_ratio_ssri <- odd_ratio_ssri %>% relocate(` `, .after = "Odd Ratio (95% CI)")

odd_ratio_ssri$OR <- as.numeric(odd_ratio_ssri$OR)
odd_ratio_ssri$CI_lower <- as.numeric(odd_ratio_ssri$CI_lower)
odd_ratio_ssri$CI_upper <- as.numeric(odd_ratio_ssri$CI_upper)


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


forest_plot_sub <- forest(
                    odd_ratio_ssri[, c(1:3, 7:8)],
                    est = odd_ratio_ssri$OR,
                    lower = odd_ratio_ssri$CI_lower,
                    upper = odd_ratio_ssri$CI_upper,
                    sizes = rep(0.5, nrow(odd_ratio_ssri)),
                    ref_line = 1,
                    ci_column = 5,
                    title = "Main metWAS models in SSRIs/SNRIs",
                    xlim = c(0.9, 1.6),
                    ticks_at = c(1, 1.25, 1.5),
                    col_widths = c(1, 4, 4, 6, 1),
                        theme = tm,
                        gp = gpar(
                            col = "#ED6803",
                            fill = "#ED6803"
                        )
                    )



odd_ratio_tca <- data.frame(
  "Case-control comparison group" = c("Current/Never", "Current/Past", "Current/Future"),
  "Case(n)" = c(1016, 1016, 1016),  
  "Control(n)" = c(43667, 5006, 4499),  
  "OR" = c(exp(coef(tca_never)[2]), exp(coef(tca_past)[2]), exp(coef(tca_future)[2])),
  "CI_lower" = c(exp(confint(tca_never)[2,1]), exp(confint(tca_past)[2,1]), exp(confint(tca_future)[2,1])),
  "CI_upper" = c(exp(confint(tca_never)[2,2]), exp(confint(tca_past)[2,2]), exp(confint(tca_future)[2,2]))
)

library(grid)
install.packages("forestploter")
library(forestploter)

odd_ratio_tca$OR <- format(round(odd_ratio_tca$OR, 2), nsmall = 2)
odd_ratio_tca$CI_lower <- format(round(odd_ratio_tca$CI_lower, 2), nsmall = 2)
odd_ratio_tca$CI_upper <- format(round(odd_ratio_tca$CI_upper, 2), nsmall = 2)
odd_ratio_tca <- odd_ratio_tca %>%
  mutate("Odd Ratio (95% CI)" = paste0(OR, " (", CI_lower, "-", CI_upper, ")"))


odd_ratio_tca <- odd_ratio_tca[, c("Case.control.comparison.group", "Case.n.", "Control.n.", "OR", "CI_lower", "CI_upper", "Odd Ratio (95% CI)")]

odd_ratio_tca <- odd_ratio_tca %>% rename("Case/Control" = "Case.control.comparison.group")
odd_ratio_tca <- odd_ratio_tca %>% rename("Case(n)" = Case.n.)
odd_ratio_tca <- odd_ratio_tca %>% rename("Control(n)" = Control.n.)

odd_ratio_tca$` ` <- paste(rep(" ", 28), collapse = " ")  ##empty column for plot space
odd_ratio_tca <- odd_ratio_tca %>% relocate(` `, .after = "Odd Ratio (95% CI)")

odd_ratio_tca$OR <- as.numeric(odd_ratio_tca$OR)
odd_ratio_tca$CI_lower <- as.numeric(odd_ratio_tca$CI_lower)
odd_ratio_tca$CI_upper <- as.numeric(odd_ratio_tca$CI_upper)


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


forest_plot_tca <- forest(
                    odd_ratio_tca[, c(1:3, 7:8)],
                    est = odd_ratio_tca$OR,
                    lower = odd_ratio_tca$CI_lower,
                    upper = odd_ratio_tca$CI_upper,
                    sizes = rep(0.5, nrow(odd_ratio_tca)),
                    title = "Main metWAS models in TCAs",
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



design <- "ABC
            DDD
            EEE"

cor_ssri_tca <- cor_ssri_tca + coord_fixed(ratio = 1)
cor_ssri_tca + pre_ssri + pre_tca + forest_plot_sub + forest_plot_tca + 
    plot_layout(design = design, width = c(1, 1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag.position = c(0.1, 1),
    plot.tag = element_text(size = 15, face = "bold")
  )


ggsave(
  "ad_subclass_plot.png",
  width = 15,   # match Word page width
  height = 8,
  dpi = 600
)
