met_data_t0 <- readRDS("baseline_met_data_final_version.rds")
met_data_t1 <- readRDS("met_data_t1.rds")

met_data_t0 <- rename(met_data_t0, f.eid = "Participant ID")
met_data_t1 <- rename(met_data_t1, f.eid = "Participant ID")

longi_users <- ad_t0_t1 %>% 
  filter(!is.na(ad_use_longi))

longi_users <- longi_users %>%
  select(f.eid, ad_use_longi)

# IDs present in all three datasets
overlap_ids <- Reduce(intersect, list(
  met_data_t0$f.eid,
  met_data_t1$f.eid,
  longi_users$f.eid
))

length(overlap_ids)  # 6078

overlap_ids <- as.data.frame(overlap_ids)
overlap_ids <- rename(overlap_ids, f.eid = "overlap_ids")


met_data_t1$time <- "1"
mixed_model_covs <- rename(mixed_model_covs, f.eid = eid)
met_data_t1 <- met_data_t1 %>% left_join(mixed_model_covs %>% select(f.eid, bmi_t1), by = "f.eid")
met_data_t1 <- met_data_t1 %>% left_join(mixed_model_covs %>% select(f.eid, spectro_t1), by = "f.eid")
met_data_t1 <- met_data_t1 %>% left_join(mixed_model_covs %>% select(f.eid, test_period_t1), by = "f.eid")
met_data_t1 <- met_data_t1 %>% left_join(mixed_model_covs %>% select(f.eid, test_month_t1), by = "f.eid")

met_data_t1 <- rename(met_data_t1, "age" = "Age")
met_data_t1 <- rename(met_data_t1, "sex" = "Sex")
met_data_t1 <- rename(met_data_t1, "bmi" = "bmi_t1")
met_data_t1 <- rename(met_data_t1, "spectrometer" = "spectro_t1")
met_data_t1 <- rename(met_data_t1, "test_period" = "test_period_t1")
met_data_t1 <- rename(met_data_t1, "test_month" = "test_month_t1")
met_data_t1 <- met_data_t1 %>%
  rename_with(
    ~ paste0("f.", .x),
    .cols = 2:252
  )

met_data_t0 <- met_data_t0 %>%
  select(1:254)

# Remove .0.0 from all column names
met_data_t0 <- met_data_t0 %>%
  rename_with(~ gsub("\\.0\\.0$", "", .x))

met_data_t0 <- rename(met_data_t0, "f.eid" = "Participant ID")
met_data_t0 <- rename(met_data_t0, "age" = "Age")
met_data_t0 <- rename(met_data_t0, "sex" = "Sex")
met_data_t0 <- met_data_t0 %>% left_join(mixed_model_covs %>% select(f.eid, bmi_t0), by = "f.eid")
met_data_t0 <- met_data_t0 %>% left_join(mixed_model_covs %>% select(f.eid, spectro_t0), by = "f.eid")
met_data_t0 <- met_data_t0 %>% left_join(mixed_model_covs %>% select(f.eid, test_period_t0), by = "f.eid")
met_data_t0 <- met_data_t0 %>% left_join(mixed_model_covs %>% select(f.eid, test_month_t0), by = "f.eid")
met_data_t0 <- rename(met_data_t0, "bmi" = "bmi_t0")
met_data_t0 <- rename(met_data_t0, "spectrometer" = "spectro_t0")
met_data_t0 <- rename(met_data_t0, "test_period" = "test_period_t0")
met_data_t0 <- rename(met_data_t0, "test_month" = "test_month_t0")
met_data_t0$time <- "0"

overlap_ids <- left_join(overlap_ids, longi_users, by = "f.eid")
overlap_ids_t0 <- left_join(overlap_ids, met_data_t0, by = "f.eid")
overlap_ids_t1 <- left_join(overlap_ids, met_data_t1, by = "f.eid")

met_ad_t0_t1 <- bind_rows(overlap_ids_t0, overlap_ids_t1)
met_ad_t0_t1 

saveRDS(met_ad_t0_t1, "met_ad_t0_t1.rds")
