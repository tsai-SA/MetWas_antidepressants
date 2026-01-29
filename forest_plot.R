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
  base_size    = 18,
  ci_col       = "#3A9AB2", # color of the confidence interval lines
  ci_pch       = 15, #point shape
  refline_gp   = gpar(col = "grey50"), #reference line
  
  # force all table cells to white
  core = list(
    padding = unit(c(15, 5), "mm"),
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
                    col_widths = c(6, 4, 4, 6, 1),
                        theme = tm,
                        gp = gpar(
                            col = "#3A9AB2",
                            fill = "#3A9AB2"
                        )
                    )

