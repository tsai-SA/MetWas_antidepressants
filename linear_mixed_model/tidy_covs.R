
repeat_asse_pheno <- rename(repeat_asse_pheno, bmi_t0 = p21001_i0)
repeat_asse_pheno <- rename(repeat_asse_pheno, bmi_t1 = p21001_i1)

repeat_asse_pheno <- rename(repeat_asse_pheno, asse_centre_t0 = p54_i0)
repeat_asse_pheno <- rename(repeat_asse_pheno, asse_centre_t1 = p54_i1) #remove this covs cos t1 were all in same centre

repeat_asse_pheno <- rename(repeat_asse_pheno, spectro_t0 = p23650_i0)
repeat_asse_pheno <- rename(repeat_asse_pheno, spectro_t1 = p23650_i1)

repeat_asse_pheno <- rename(repeat_asse_pheno, age_t0 = p21003_i0)
repeat_asse_pheno <- rename(repeat_asse_pheno, age_t1 = p21003_i1)

repeat_asse_pheno <- rename(repeat_asse_pheno, sex_t0 = p21_i0)
repeat_asse_pheno <- rename(repeat_asse_pheno, sex_t1 = p21_i1)

repeat_asse_pheno <- rename(repeat_asse_pheno, test_date_t0 = p3166_i0_a0)
repeat_asse_pheno <- rename(repeat_asse_pheno, test_date_t1 = p3166_i1_a0)

repeat_asse_pheno <- rename(repeat_asse_pheno, sex_t0 = p31)
repeat_asse_pheno$sex_t1 <- repeat_asse_pheno$sex_t0

repeat_asse_pheno <- left_join(repeat_asse_pheno, sex, by ="eid")
repeat_asse_pheno <- repeat_asse_pheno %>%
  select("eid", "bmi_t0", "bmi_t1", "spectro_t0", "spectro_t1", "age_t0", "age_t1", "sex_t0", "sex_t1", "test_date_t0", "test_date_t1")

library(tibble)
library(dplyr)
library(lubridate)
library(stringr)

repeat_asse_pheno <- repeat_asse_pheno %>%
  mutate(test_date_t0 = str_trim(test_date_t0),                 # remove spaces
         test_date_t0 = na_if(test_date_t0, ""),                # empty → NA
         test_date_t0 = na_if(test_date_t0, "NA"),              # literal "NA" → NA
         test_date_t0 = na_if(test_date_t0, "Not performed")   # "not performed" → NA
  ) %>%
  mutate(test_date_t0 = parse_date_time(test_date_t0,
                                           orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                                           tz = "UTC"))


repeat_asse_pheno <- repeat_asse_pheno %>%
  mutate(test_date_t1 = str_trim(test_date_t1),                 # remove spaces
         test_date_t1 = na_if(test_date_t1, ""),                # empty → NA
         test_date_t1 = na_if(test_date_t1, "NA"),              # literal "NA" → NA
         test_date_t1 = na_if(test_date_t1, "Not performed")   # "not performed" → NA
  ) %>%
  mutate(test_date_t1 = parse_date_time(test_date_t1,
                                           orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                                           tz = "UTC"))

repeat_asse_pheno <- repeat_asse_pheno %>%
  mutate(test_period_t0 = case_when(
    hour(test_date_t0) >= 5  & hour(test_date_t0) < 12 ~ "morning",
    hour(test_date_t0) >= 12 & hour(test_date_t0) < 17 ~ "afternoon",
    hour(test_date_t0) >= 17 & hour(test_date_t0) < 21 ~ "evening",
    hour(test_date_t0) >= 21 | hour(test_date_t0) < 5  ~ "night",
    TRUE ~ NA_character_
  ))

repeat_asse_pheno <- repeat_asse_pheno %>%
  mutate(test_period_t1 = case_when(
    hour(test_date_t1) >= 5  & hour(test_date_t1) < 12 ~ "morning",
    hour(test_date_t1) >= 12 & hour(test_date_t1) < 17 ~ "afternoon",
    hour(test_date_t1) >= 17 & hour(test_date_t1) < 21 ~ "evening",
    hour(test_date_t1) >= 21 | hour(test_date_t1) < 5  ~ "night",
    TRUE ~ NA_character_
  ))

repeat_asse_pheno$test_month_t0 <- month(repeat_asse_pheno$test_date_t0)
repeat_asse_pheno$test_month_t1 <- month(repeat_asse_pheno$test_date_t1)

saveRDS(repeat_asse_pheno, "mixed_model_covs.rds")
