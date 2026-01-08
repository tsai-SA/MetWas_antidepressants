baseline_ad_subclass <- readRDS("baseline_ad_subclass.rds")


baseline_ad_subclass$ssri_current_never <- ifelse(
  baseline_ad_subclass$SSRI_status %in% c("SSRI_Current", "SSRI_Never"),
  baseline_ad_subclass$SSRI_status,
  NA
)

baseline_ad_subclass$ssri_current_past <- ifelse(
  baseline_ad_subclass$SSRI_status %in% c("SSRI_Current", "SSRI_Past"),
  baseline_ad_subclass$SSRI_status,
  NA
)

baseline_ad_subclass$ssri_current_future <- ifelse(
  baseline_ad_subclass$SSRI_status %in% c("SSRI_Current", "SSRI_Future"),
  baseline_ad_subclass$SSRI_status,
  NA
)

baseline_ad_subclass$tricyclic_current_never <- ifelse(
  baseline_ad_subclass$Tricyclic_status %in% c("Tricyclic_Current", "Tricyclic_Never"),
  baseline_ad_subclass$Tricyclic_status,
  NA
)

baseline_ad_subclass$tricyclic_current_past <- ifelse(
  baseline_ad_subclass$Tricyclic_status %in% c("Tricyclic_Current", "Tricyclic_Past"),
  baseline_ad_subclass$Tricyclic_status,
  NA
)

baseline_ad_subclass$tricyclic_current_future <- ifelse(
  baseline_ad_subclass$Tricyclic_status %in% c("Tricyclic_Current", "Tricyclic_Future"),
  baseline_ad_subclass$Tricyclic_status,
  NA
)

baseline_ad_subclass$other_current_never <- ifelse(
  baseline_ad_subclass$other_status %in% c("other_Current", "other_Never"),
  baseline_ad_subclass$other_status,
  NA
)
baseline_ad_subclass$other_current_past <- ifelse(
  baseline_ad_subclass$other_status %in% c("other_Current", "other_Past"),
  baseline_ad_subclass$other_status,
  NA
)
baseline_ad_subclass$other_current_future <- ifelse(
  baseline_ad_subclass$other_status %in% c("other_Current", "other_Future"),
  baseline_ad_subclass$other_status,
  NA
)


# Change to 0/1, 1 = current, 0 = never/past/future
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(ssri_current_never, ~ replace(., . == "SSRI_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(ssri_current_never, ~ replace(., . == "SSRI_Never", "0")))
baseline_ad_subclass$ssri_current_never <- as.factor(baseline_ad_subclass$ssri_current_never)

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(ssri_current_past, ~ replace(., . == "SSRI_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(ssri_current_past, ~ replace(., . == "SSRI_Past", "0")))
baseline_ad_subclass$ssri_current_past <- as.factor(baseline_ad_subclass$ssri_current_past)

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(ssri_current_future, ~ replace(., . == "SSRI_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(ssri_current_future, ~ replace(., . == "SSRI_Future", "0")))
baseline_ad_subclass$ssri_current_future <- as.factor(baseline_ad_subclass$ssri_current_future)

table(baseline_ad_subclass$SSRI_status)
table(baseline_ad_subclass$ssri_current_never)
table(baseline_ad_subclass$ssri_current_past)
table(baseline_ad_subclass$ssri_current_future)

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(tricyclic_current_never, ~ replace(., . == "Tricyclic_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(tricyclic_current_never, ~ replace(., . == "Tricyclic_Never", "0")))
baseline_ad_subclass$tricyclic_current_never <- as.factor(baseline_ad_subclass$tricyclic_current_never) 

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(tricyclic_current_past, ~ replace(., . == "Tricyclic_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(tricyclic_current_past, ~ replace(., . == "Tricyclic_Past", "0")))
baseline_ad_subclass$tricyclic_current_past <- as.factor(baseline_ad_subclass$tricyclic_current_past)

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(tricyclic_current_future, ~ replace(., . == "Tricyclic_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(tricyclic_current_future, ~ replace(., . == "Tricyclic_Future", "0")))
baseline_ad_subclass$tricyclic_current_future <- as.factor(baseline_ad_subclass$tricyclic_current_future)

table(baseline_ad_subclass$Tricyclic_status)
table(baseline_ad_subclass$tricyclic_current_never)
table(baseline_ad_subclass$tricyclic_current_past)
table(baseline_ad_subclass$tricyclic_current_future)

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(other_current_never, ~ replace(., . == "other_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(other_current_never, ~ replace(., . == "other_Never", "0")))
baseline_ad_subclass$other_current_never <- as.factor(baseline_ad_subclass$other_current_never)

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(other_current_past, ~ replace(., . == "other_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(other_current_past, ~ replace(., . == "other_Past", "0")))
baseline_ad_subclass$other_current_past <- as.factor(baseline_ad_subclass$other_current_past)

baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(other_current_future, ~ replace(., . == "other_Current", "1")))
baseline_ad_subclass <- baseline_ad_subclass %>% mutate(across(other_current_future, ~ replace(., . == "other_Future", "0")))
baseline_ad_subclass$other_current_future <- as.factor(baseline_ad_subclass$other_current_future)

table(baseline_ad_subclass$other_status)
table(baseline_ad_subclass$other_current_never)
table(baseline_ad_subclass$other_current_past)
table(baseline_ad_subclass$other_current_future)

head(baseline_ad_subclass)

baseline_ad_subclass_pheno <- baseline_ad_subclass %>%
  dplyr::select(
    f.eid,
    ssri_current_never,
    ssri_current_past,
    ssri_current_future,
    tricyclic_current_never,
    tricyclic_current_past,
    tricyclic_current_future,
    other_current_never,
    other_current_past,
    other_current_future
  )

saveRDS(baseline_ad_subclass_pheno, "baseline_ad_subclass_pheno.rds")

met_data <- readRDS("met_data.rds")
baseline_ad_subclass_pheno <- readRDS("baseline_ad_subclass_pheno.rds")

library(dplyr)

met_data <- rename(met_data, "f.eid" = "Participant ID")

ssri_met_data <- met_data %>% 
  left_join(baseline_ad_subclass_pheno %>% 
              select(f.eid, ssri_current_never, ssri_current_past, ssri_current_future), 
                by = "f.eid"
            )

ssri_met_data <- ssri_met_data %>% select(-Current_vs_Never, -Current_vs_Past, -Current_vs_Future)

ssri_met_data <- ssri_met_data %>%
  filter(!if_all(c(ssri_current_never, ssri_current_past, ssri_current_future), is.na))

tricy_met_data <- met_data %>% 
  left_join(baseline_ad_subclass_pheno %>% 
              select(f.eid, tricyclic_current_never, tricyclic_current_past, tricyclic_current_future), 
            by = "f.eid"
  )

tricy_met_data <- tricy_met_data %>%
  filter(!if_all(c(tricyclic_current_never, tricyclic_current_past, tricyclic_current_future), is.na))

tricy_met_data <- tricy_met_data %>% select(-Current_vs_Never, -Current_vs_Past, -Current_vs_Future)

other_met_data <- met_data %>% 
  left_join(baseline_ad_subclass_pheno %>% 
              select(f.eid, other_current_never,other_current_past, other_current_future), 
            by = "f.eid"
  )

other_met_data <- other_met_data %>%
  filter(!if_all(c(other_current_never,other_current_past, other_current_future), is.na))

other_met_data <- other_met_data %>% select(-Current_vs_Never, -Current_vs_Past, -Current_vs_Future)

saveRDS(ssri_met_data, "ssri_met_data.rds")
saveRDS(tricy_met_data, "tricy_met_data.rds")
saveRDS(other_met_data, "other_met_data.rds")