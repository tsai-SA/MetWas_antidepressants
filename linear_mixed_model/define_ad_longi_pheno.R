

ad_dose_episodes_baseline <- readRDS("ad_dose_episodes_baseline.rds")
mixed_model_covs <- readRDS("mixed_model_covs.rds")
mixed_model_covs <- rename(mixed_model_covs, f.eid = eid)

gp_regis <- mixed_model_covs %>%
  select(f.eid, gp)

ad_dose_episodes_baseline_filter <- ad_dose_episodes_baseline %>%
  filter(prescriptions > 1) # remove those with single prescriptions

gp_regis <- gp_regis %>%
  filter(!is.na(gp))

gp_regis <- left_join(gp_regis, ad_dose_episodes_baseline_filter, by = "f.eid")

gp_regis <- gp_regis %>% left_join(mixed_model_covs %>% select(f.eid, test_date_t1), by = "f.eid")

gp_regis <- gp_regis %>%
  mutate(ad_episodes_t1 = case_when(
    
    ## Never users FIRST
    !is.na(gp) & 
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
    is.na(gp) ~ NA_character_,
    
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
      ad_use_t0 == "Never" & ad_use_t1 == "Never" ~ "Never",
      ad_use_t1 == "Current" & ad_use_t0 %in% c("Never", "Past", "Future") ~ "New",
      ad_use_t0 == "Current" & ad_use_t1 == "Current" ~ "Stayer",
      ad_use_t0 == "Current" & ad_use_t1 == "Past" ~ "Leaver",
      TRUE ~ NA_character_
    )
  )

table(ad_t0_t1$ad_use_longi)


saveRDS(ad_t0_t1, "ad_t0_t1.rds")     
