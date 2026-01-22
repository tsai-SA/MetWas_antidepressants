
phenotype_export <- rename(phenotype_export, bmi_t0 = p21001_i0)
phenotype_export <- rename(phenotype_export, bmi_t1 = p21001_i1)

phenotype_export$p54_i0 <- NULL
phenotype_export$p54_i1 <- NULL #remove this covs cos t1 were all in same centre

phenotype_export <- rename(phenotype_export, spectro_t0 = p23650_i0)
phenotype_export <- rename(phenotype_export, spectro_t1 = p23650_i1)

phenotype_export <- rename(phenotype_export, age_t0 = p21003_i0)
phenotype_export <- rename(phenotype_export, age_t1 = p21003_i1)

phenotype_export <- rename(phenotype_export, test_date_t0 = p3166_i0_a0)
phenotype_export <- rename(phenotype_export, test_date_t1 = p3166_i1_a0)

phenotype_export <- rename(phenotype_export, sex_t0 = p31)
phenotype_export$sex_t1 <- phenotype_export$sex_t0

phenotype_export <- rename(phenotype_export, gp = p42038)

phenotype_export <- left_join(phenotype_export, sex, by ="eid")
phenotype_export <- phenotype_export %>%
  select("eid", "bmi_t0", "bmi_t1", "spectro_t0", "spectro_t1", "age_t0", "age_t1", "sex_t0", "sex_t1", "test_date_t0", "test_date_t1", "gp")

library(tibble)
library(dplyr)
library(lubridate)
library(stringr)

phenotype_export <- phenotype_export %>%
  mutate(test_date_t0 = str_trim(test_date_t0),                 # remove spaces
         test_date_t0 = na_if(test_date_t0, ""),                # empty → NA
         test_date_t0 = na_if(test_date_t0, "NA"),              # literal "NA" → NA
         test_date_t0 = na_if(test_date_t0, "Not performed")   # "not performed" → NA
  ) %>%
  mutate(test_date_t0 = parse_date_time(test_date_t0,
                                        orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                                        tz = "UTC"))


phenotype_export <- phenotype_export %>%
  mutate(test_date_t1 = str_trim(test_date_t1),                 # remove spaces
         test_date_t1 = na_if(test_date_t1, ""),                # empty → NA
         test_date_t1 = na_if(test_date_t1, "NA"),              # literal "NA" → NA
         test_date_t1 = na_if(test_date_t1, "Not performed")   # "not performed" → NA
  ) %>%
  mutate(test_date_t1 = parse_date_time(test_date_t1,
                                        orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                                        tz = "UTC"))

phenotype_export <- phenotype_export %>%
  mutate(test_period_t0 = case_when(
    hour(test_date_t0) >= 5  & hour(test_date_t0) < 12 ~ "morning",
    hour(test_date_t0) >= 12 & hour(test_date_t0) < 17 ~ "afternoon",
    hour(test_date_t0) >= 17 & hour(test_date_t0) < 21 ~ "evening",
    hour(test_date_t0) >= 21 | hour(test_date_t0) < 5  ~ "night",
    TRUE ~ NA_character_
  ))

phenotype_export <- phenotype_export %>%
  mutate(test_period_t1 = case_when(
    hour(test_date_t1) >= 5  & hour(test_date_t1) < 12 ~ "morning",
    hour(test_date_t1) >= 12 & hour(test_date_t1) < 17 ~ "afternoon",
    hour(test_date_t1) >= 17 & hour(test_date_t1) < 21 ~ "evening",
    hour(test_date_t1) >= 21 | hour(test_date_t1) < 5  ~ "night",
    TRUE ~ NA_character_
  ))

phenotype_export$test_month_t0 <- month(phenotype_export$test_date_t0)
phenotype_export$test_month_t1 <- month(phenotype_export$test_date_t1)

saveRDS(phenotype_export, "mixed_model_covs.rds")
