

ad_dose_episodes_baseline <- readRDS("ad_dose_episodes_baseline.rds")
mixed_model_covs <- readRDS("mixed_model_covs.rds")

gp_regis <- rename(gp_regis, f.eid = "eid" )
gp_regis <- rename(gp_regis, regis_record = "p42038" )

gp_regis <- gp_regis %>%
  left_join(ad_dose_episodes_baseline, by = "f.eid")

gp_regis <- gp_regis %>% left_join(repeat_asse_pheno %>% select(f.eid, test_date_t1), by = "f.eid")

gp_regis <- gp_regis %>%
  mutate(ad_episodes_t1 = case_when(

    ## Never users FIRST
    !is.na(regis_record) & 
      is.na(issue_date_start) & 
      is.na(issue_date_end) ~ "Never",
    
    ## Current users
    test_date_t1 >= issue_date_start &
      test_date_t1 <= issue_date_end ~ "Current",
    
    ## Past users (ended ≥ 6 months before assessment)
    issue_date_end <= test_date_t1 %m-% months(6) ~ "Past",
    
    ## Future users
    issue_date_start > test_date_t1 ~ "Future",
    
    ## No GP registration → NA
    is.na(regis_record) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))


gp_regis <- gp_regis %>%
  group_by(f.eid) %>%
  mutate(
    ad_pheno_t1 = case_when(
      ## No usable AD data at all
      all(is.na(ad_episodes_t1)) ~ NA_character_,
      ## At least one current episode → Current
      any(ad_episodes_t1 == "Current", na.rm = TRUE) ~ "Current",
      ## No current, but at least one past → Past
      !any(ad_episodes_t1 == "Current", na.rm = TRUE) &
        any(ad_episodes_t1 == "Past", na.rm = TRUE) ~ "Past",
      ## No current or past, but at least one future → Future
      !any(ad_episodes_t1 %in% c("Current", "Past"), na.rm = TRUE) &
        any(ad_episodes_t1 == "Future", na.rm = TRUE) ~ "Future",
      ## All episodes are Never → Never
      all(ad_episodes_t1 == "Never", na.rm = TRUE) ~ "Never",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

saveRDS(gp_regis, "gp_regis_ad_epi.rds")

ad_t1 <- gp_regis %>%
  group_by(f.eid) %>%
  summarise(ad_pheno_t1 = first(ad_pheno_t1), .groups = "drop")

BaselineID_ad_usage <- readRDS("BaselineID_ad_usage.rds")

BaselineID_ad_usage <- rename(BaselineID_ad_usage, ad_use_t0 = AD_use)
ad_t1 <- rename(ad_t1, ad_use_t1 = ad_pheno_t1)

ad_t0_t1 <- left_join(BaselineID_ad_usage, ad_t1, by ="f.eid")

table(ad_t0_t1$ad_use_t1)

ad_t0_t1 <- ad_t0_t1 %>% 
  mutate(
    ad_use_longi = case_when(
      ## Stable never-use
      ad_use_t0 == "Never" & ad_use_t1 == "Never" ~ "Never",
      ## New use at T1
      ad_use_t1 == "Current" & ad_use_t0 %in% c("Never", "Past", "Future") ~ "New",
      ad_use_t0 == "Current" & ad_use_t1 == "Current" ~ "Current",
      TRUE ~ NA_character_
    )
  )

saveRDS(ad_t0_t1, "ad_t0_t1.rds")     

ad_t0_t1 <- readRDS("ad_t0_t1.rds")

longi_users <- ad_t0_t1 %>% 
  filter(!is.na(ad_use_longi))

longi_users <- longi_users %>%
  select(f.eid, ad_use_longi)

met_data_repeat_impt <- readRDS("met_data_repeat_impt.rds")
met_data_repeat_impt <- rename(met_data_repeat_impt, f.eid = "Participant ID")

met_data_repeat_impt <- left_join(met_data_repeat_impt, longi_users, by = "f.eid")
colnames(met_data_repeat_impt)

overlap_ids <- as.data.frame(met_data_repeat_impt$f.eid)
overlap_ids <- rename(overlap_ids, f.eid = "met_data_repeat_impt$f.eid")


met_data_baseline <- readRDS("baseline_met_data_final_version.rds")
met_data_baseline <- rename(met_data_baseline, f.eid = "Participant ID")
met_data_t0 <- left_join(overlap_ids, met_data_baseline, by = "f.eid")


mixed_model_covs <- readRDS("mixed_model_covs.rds")

met_data_repeat_impt$time <- "1"
mixed_model_covs <- rename(mixed_model_covs, f.eid = eid)
met_data_repeat_impt <- met_data_repeat_impt %>% left_join(mixed_model_covs %>% select(f.eid, bmi_t1), by = "f.eid")
met_data_repeat_impt <- met_data_repeat_impt %>% left_join(mixed_model_covs %>% select(f.eid, spectro_t1), by = "f.eid")
met_data_repeat_impt <- met_data_repeat_impt %>% left_join(mixed_model_covs %>% select(f.eid, test_period_t1), by = "f.eid")
met_data_repeat_impt <- met_data_repeat_impt %>% left_join(mixed_model_covs %>% select(f.eid, test_month_t1), by = "f.eid")

met_data_repeat_impt <- rename(met_data_repeat_impt, "age" = "Age")
met_data_repeat_impt <- rename(met_data_repeat_impt, "sex" = "Sex")
met_data_repeat_impt <- rename(met_data_repeat_impt, "bmi" = "bmi_t1")
met_data_repeat_impt <- rename(met_data_repeat_impt, "spectrometer" = "spectro_t1")
met_data_repeat_impt <- rename(met_data_repeat_impt, "test_period" = "test_period_t1")
met_data_repeat_impt <- rename(met_data_repeat_impt, "test_month" = "test_month_t1")
met_data_repeat_impt <- met_data_repeat_impt %>%
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
met_data_t0 <- left_join(met_data_t0, longi_users, by = "f.eid")

longi_t0_t1 <- bind_rows(longi_users_t0, longi_users_t1)

met_data_t0_t1 <- met_data_t0_t1 %>%
  filter(ad_use_longi != "Current") #keep only controls vs new users 
