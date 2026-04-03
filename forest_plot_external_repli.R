

nesda_beta <- 0.165637997
nesda_se <- 0.044380078
nesda_odd <- exp(nesda_beta)
nesda_lower_ci <- exp(nesda_beta - 1.96 * nesda_se)
nesda_upper_ci <- exp(nesda_beta + 1.96 * nesda_se)

alspac_beta <- 0.1176950
alspac_se <- 0.05043238
alspac_odd <- exp(alspac_beta)
alspac_lower_ci <- exp(alspac_beta - 1.96 * alspac_se)
alspac_upper_ci <- exp(alspac_beta + 1.96 * alspac_se)  

forest <- data.frame(
  cohort = c("UKB", "NESDA", "ALSPAC"),
  odd_ratio = c("1.38", "1.18", "1.12"),
  lower_ci = c("1.34", "1.08", "1.02"),
  upper_ci = c("1.43", "1.29", "1.24"),
  AUC = c("0.59", "0.63", "0.60")
)

forest <- rename(forest, Cohort = cohort)
forest <- rename(forest, "Odd Ratio" = "Odd Ratio (95% CI)")
forest <- rename(forest, "ROC AUC" = AUC)

forest$`Odd Ratio` <- as.numeric(forest$`Odd Ratio`)
forest$lower_ci <- as.numeric(forest$lower_ci)
forest$upper_ci <- as.numeric(forest$upper_ci)
forest$"ROC AUC" <- as.numeric(forest$"ROC AUC")

forest$` ` <- paste(rep(" ", 28), collapse = " ")  ##empty column for plot space

library(forestploter)

forest <- forest %>%
  mutate("Odd Ratio (95% CI)" = paste0(round(`Odd Ratio`, 2), " (", round(lower_ci, 2), "-", round(upper_ci, 2), ")"))

forest <- forest %>% relocate(` `, .after = "Odd Ratio (95% CI)")
forest <- forest %>% relocate(`ROC AUC`, .after = ` `)
forest$`Current (n)` <- c("2749", "723", "448")
forest$`Never (n)` <- c("38136", "2191", "1966")
forest <- forest %>% relocate(`Current (n)`, .after = Cohort)
forest <- forest %>% relocate(`Never (n)`, .after = `Current (n)`)



forest_plot <- forest(forest[, c(1:3, 7:9),],  #Choose only the cols you want to see in the table
            est = forest$`Odd Ratio`,
            lower = forest$lower_ci, 
            upper = forest$upper_ci,
            sizes = rep(0.4, nrow(forest)),
            ref_line = 1,
            ci_column = 5,   #col number for CI column
            xlim = c(0.9, 1.5),  #horizontal axis limits for the plot area
            ticks_at = c(1, 1.2, 1.4),
            theme = tm,
            gp = gpar(
            col = "#3A9AB2",
            fill = "#3A9AB2"
                        )
                    )


tm <- forest_theme(
  base_size    = 15,
  ci_col       = "#3A9AB2", # color of the confidence interval lines
  ci_pch       = 15, #point shape
  ci_lwd = 2,
  refline_gp   = gpar(col = "grey50"), #reference line
  
  # force all table cells to white
  core = list(
    padding = unit(c(6, 4), "mm"),
    bg_params = list(
      fill = rep("white", 7)  # repeat enough times for all rows
    )
  ),
  
  # full plot background
  vp.background = gpar(fill = "white", col = NA)
)


ggsave("forest_plot.jpeg", plot = forest_plot, width = 10, height = 2, dpi = 300)
