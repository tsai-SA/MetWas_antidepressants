library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(ROCR, lib="/exports/igmm/eddie/GenScotDepression/users/angela/MetWas")

setwd("/exports/igmm/eddie/GenScotDepression/users/angela/MetWas/ranger")

episcore_ad <- fread("/exports/igmm/eddie/GenScotDepression/users/angela/MetWas/ranger/DNAm_utils/episcore_result/episcore_ad")

met_data <- readRDS("/exports/igmm/eddie/GenScotDepression/users/angela/MetWas/ranger/met_data.rds")

BaselineID_ad_usage <- readRDS("/exports/igmm/eddie/GenScotDepression/users/angela/BaselineID_ad_usage.rds")

BaselineID_ad_usage_current <- BaselineID_ad_usage %>% filter(AD_use == "Current")
BaselineID_ad_usage_current <- BaselineID_ad_usage %>% filter(episode_status == "Current")

current_users <- episcore_ad %>% filter(Current_vs_Never == "1")

current_users <- current_users %>% left_join(BaselineID_ad_usage_current %>% select(ID, issue_date_duration), by = "ID")

current_users$issue_date_duration <- as.numeric(current_users$issue_date_duration)

summary(current_users$issue_date_duration)

current_users$issue_year_duration <- current_users$issue_date_duration / 365

current_users$issue_year_duration <- round(current_users$issue_year_duration, digits = 1)

current_year_model <- lm(issue_year_duration ~ scale(total.MRS), data = current_users)

library(ggplot2)

ggplot(current_users, aes(x = scale(total.MRS), y = issue_year_duration)) +

    geom_point(
        color="#69b3a2",
        fill="#69b3a2",
        shape=21,
        alpha=0.5,
        size=2,
        stroke = 1
        ) + 

    scale_y_continuous(
    breaks = seq(0, 28, by = 5)) +

    geom_smooth(method = "lm", se = TRUE, color ="#ec832d") +

    theme_classic() + 
    
    labs(x = "Standardised metabolic profile score", 
            y = "Duration of antidepressant use (years)") +

    theme(axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)) 


summary(current_users$issue_year_duration)

jpeg(
  "duration.jpeg",
  width = 7,
  height = 7,
  units = "in",
  res = 300
)

dev.off()

