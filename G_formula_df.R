met_data_t0 <- readRDS("baseline_met_data_final_version.rds")
met_data_t1 <- readRDS("met_data_repeat_impt.rds")
mixed_model_covs <- readRDS("mixed_model_covs.rds")
mixed_model_covs <- rename(mixed_model_covs, f.eid = eid)
ad_t0_t1 <- readRDS("ad_t0_t1.rds")

met_data_t0 <- rename(met_data_t0, f.eid = "Participant ID")
met_data_t1 <- rename(met_data_t1, f.eid = "Participant ID")

longi_users <- ad_t0_t1 %>% 
  filter(!is.na(ad_use_longi))

longi_users <- longi_users %>%
  select(f.eid, ad_use_longi)

never_stayer <- longi_users %>%
  filter(ad_use_longi %in% c("Never", "Stayer"))

table(longi_users$ad_use_longi)
table(never_stayer$ad_use_longi)

never_stayer <- never_stayer %>%
  mutate(
    treatment_t0 = case_when(
      ad_use_longi == "Never" ~ 0,
      ad_use_longi == "Stayer" ~ 1,
      TRUE ~ NA_real_
    )
  )

never_stayer <- never_stayer %>%
  mutate(
    treatment_t1 = case_when(
      ad_use_longi == "Never" ~ 0,
      ad_use_longi == "Stayer" ~ 1,
      TRUE ~ NA_real_
    )
  )


# IDs present in all three datasets
overlap_ids <- Reduce(intersect, list(
  met_data_t0$f.eid,
  met_data_t1$f.eid,
  never_stayer$f.eid
))

length(overlap_ids)  # 5877

overlap_ids <- as.data.frame(overlap_ids)
overlap_ids <- rename(overlap_ids, f.eid = "overlap_ids")

met_data_t0 <- met_data_t0 %>% left_join(mixed_model_covs %>% select(f.eid, bmi_t1), by = "f.eid")

met_data_t1 <- met_data_t1 %>% left_join(mixed_model_covs %>% select(f.eid, bmi_t1), by = "f.eid")

covs_t0 <- met_data_t0 %>%
  select(f.eid, Age, Sex, BMI)

covs_t0 <- rename(covs_t0, age_t0 = Age)
covs_t0 <- rename(covs_t0, sex_t0 = Sex)
covs_t0 <- rename(covs_t0, bmi_t0 = BMI)

met_data_t1$Age <- NULL
met_data_t1$Sex <- NULL


colnames(met_data_t1)
overlap_ids <- left_join(overlap_ids, never_stayer, by = "f.eid")
overlap_ids_t0 <- left_join(overlap_ids, covs_t0, by = "f.eid")
met_ad_t0_t1_g_formu <- left_join(overlap_ids_t0, met_data_t1, by = "f.eid")

colnames(met_ad_t0_t1_g_formu)
table(met_ad_t0_t1_g_formu$ad_use_longi)
table(met_ad_t0_t1_g_formu$treatment_t0)
table(met_ad_t0_t1_g_formu$treatment_t1)

saveRDS(met_ad_t0_t1_g_formu, "met_ad_t0_t1_g_formu.rds")
