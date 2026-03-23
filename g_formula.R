####g formula on complete cases, scale each met from t0 and t1 seperatly 

install.packages("mice")
install.packages("gFormulaMI")

library(mice)
library(gFormulaMI)
library(dplyr)
library(tidyr)

met_ad_t0_t1 <- readRDS("met_ad_t0_t1_g_formu_v3.rds")
sig_met_metwas <- readRDS("sig_met_metwas.rds")
sig_baseline_met <- sig_met_metwas$Outcome
sig_outcome_met <- sig_met_metwas$FieldID #same as sig_baseline_met
covs <- c("treatment_t0", "treatment_t1",
          "age_t0", "sex_t0", "bmi_t0", "bmi_t1", "age_t1")

met_ad_t0_t1_filtered <- met_ad_t0_t1 %>% 
  select(any_of(c(covs, sig_baseline_met, sig_outcome_met))) #keep only covs and sig met

baseline_met <- colnames(met_ad_t0_t1_filtered[, 8:159])

##scale met
met_ad_t0_t1_filtered <- met_ad_t0_t1_filtered %>%
  mutate(across(8:311, ~ as.numeric(scale(.))))

all_covs_t0 <- list()

for (x in baseline_met) {
  covs_t0 <- c(x, "sex_t0", "age_t0", "bmi_t0")
  all_covs_t0[[x]] <- covs_t0
}

names(all_covs_t0) <- sub("^f\\.([0-9]+)\\.0\\.0$", "\\1", names(all_covs_t0))

treatment_t0 = "treatment_t0"
covs_t1 <- c("age_t1", "bmi_t1")
treatment_t1 = "treatment_t1"

met_outcome <- names(met_ad_t0_t1_filtered)[160:311]

str(met_ad_t0_t1_filtered)
met_ad_t0_t1_filtered$treatment_t0 <- as.factor(met_ad_t0_t1_filtered$treatment_t0)
met_ad_t0_t1_filtered$treatment_t1 <- as.factor(met_ad_t0_t1_filtered$treatment_t1)

##only complete cases in this analysis
met_ad_t0_t1_filtered <- met_ad_t0_t1_filtered[complete.cases(met_ad_t0_t1_filtered), ]

df_list <- lapply(met_outcome, function(outcome_name) { #For each element of met_outcome, call it outcome_name
  
  met_ad_t0_t1_filtered %>%
    select(all_of(c(
      all_covs_t0[[outcome_name]],
      treatment_t0,
      covs_t1,
      treatment_t1,
      outcome_name
    )))
})

names(df_list) <- met_outcome
head(df_list$"20280")

imps_list <- lapply(seq_along(df_list), function(i) { #For each element of df_list, call it met_df
  
  met_df <- df_list[[i]]
  
  imp_method <- sapply(met_df, function(x) { #for covariates
    if (is.numeric(x)) {
      return("norm")
    } else if (is.factor(x)) {
      return("logreg")
    } else {
      return("")  # do not impute other types 
    }
  })
  
  imps <- gFormulaImpute(data=met_df, 
                         method = imp_method,
                         M=50, 
                         trtVars=c("treatment_t0", "treatment_t1"),
                         trtRegimes=list(c(0,0),c(0,1))) #data simulation
  
  imps_values <- as.data.frame(mice::complete(imps)) #simulated data
  
  outcome_met <- names(met_df)[ncol(met_df)] #last col of df must be the outcome variable, thus name the output accord to last col
  
  fits <- with(imps, lm(as.formula(paste0("`", outcome_met, "` ~ factor(regime)"))))
  
  pooled <- as.data.frame(syntheticPool(fits))
  pooled$Outcome <- outcome_met
  return(pooled)  
  
  logs <- as.data.frame(imps$loggedEvents)
  method <- as.data.frame(imps$method) #check if all variables are imputed correctly
  pm <- as.data.frame(imps$predictorMatrix)
  
  return(logs)
  return(method)
  return(pm)
  
})


controls_vs_new_results <- bind_rows(lapply(imps_list, tail, 1))
controls_vs_new_sig <- controls_vs_new_results %>% filter(p<0.05)


#### controls vs stayers 
####g formula on complete cases, scale each met from t0 and t1 seperatly 

install.packages("mice")
install.packages("gFormulaMI")

library(mice)
library(gFormulaMI)
library(dplyr)
library(tidyr)

met_ad_t0_t1 <- readRDS("met_ad_t0_t1_g_formu_v3.rds")
sig_met_metwas <- readRDS("sig_met_metwas.rds")
sig_baseline_met <- sig_met_metwas$Outcome
sig_outcome_met <- sig_met_metwas$FieldID #same as sig_baseline_met
covs <- c("treatment_t0", "treatment_t1",
          "age_t0", "sex_t0", "bmi_t0", "bmi_t1", "age_t1")

met_ad_t0_t1_filtered <- met_ad_t0_t1 %>% 
    select(any_of(c(covs, sig_baseline_met, sig_outcome_met))) #keep only covs and sig met

baseline_met <- colnames(met_ad_t0_t1_filtered[, 8:159])

##scale met
met_ad_t0_t1_filtered <- met_ad_t0_t1_filtered %>%
  mutate(across(8:311, ~ as.numeric(scale(.))))

all_covs_t0 <- list()

for (x in baseline_met) {
  covs_t0 <- c(x, "sex_t0", "age_t0", "bmi_t0")
  all_covs_t0[[x]] <- covs_t0
}

names(all_covs_t0) <- sub("^f\\.([0-9]+)\\.0\\.0$", "\\1", names(all_covs_t0))

treatment_t0 = "treatment_t0"
covs_t1 <- c("age_t1", "bmi_t1")
treatment_t1 = "treatment_t1"

met_outcome <- names(met_ad_t0_t1_filtered)[160:311]

str(met_ad_t0_t1_filtered)
met_ad_t0_t1_filtered$treatment_t0 <- as.factor(met_ad_t0_t1_filtered$treatment_t0)
met_ad_t0_t1_filtered$treatment_t1 <- as.factor(met_ad_t0_t1_filtered$treatment_t1)

##only complete cases in this analysis
met_ad_t0_t1_filtered <- met_ad_t0_t1_filtered[complete.cases(met_ad_t0_t1_filtered), ]

df_list <- lapply(met_outcome, function(outcome_name) { #For each element of met_outcome, call it outcome_name
  
  met_ad_t0_t1_filtered %>%
    select(all_of(c(
      all_covs_t0[[outcome_name]],
      treatment_t0,
      covs_t1,
      treatment_t1,
      outcome_name
    )))
})

names(df_list) <- met_outcome
head(df_list$"20280")

imps_list <- lapply(seq_along(df_list), function(i) { #For each element of df_list, call it met_df
  
  met_df <- df_list[[i]]
  
  imp_method <- sapply(met_df, function(x) { #for covariates
    if (is.numeric(x)) {
      return("norm")
    } else if (is.factor(x)) {
      return("logreg")
    } else {
      return("")  # do not impute other types 
    }
  })
  
  imps <- gFormulaImpute(data=met_df, 
                         method = imp_method,
                         M=50, 
                         trtVars=c("treatment_t0", "treatment_t1"),
                         trtRegimes=list(c(0,0),c(1,1))) #data simulation
  
  imps_values <- as.data.frame(mice::complete(imps)) #simulated data
  
  outcome_met <- names(met_df)[ncol(met_df)] #last col of df must be the outcome variable, thus name the output accord to last col
  
  fits <- with(imps, lm(as.formula(paste0("`", outcome_met, "` ~ factor(regime)"))))
  
  pooled <- as.data.frame(syntheticPool(fits))
  pooled$Outcome <- outcome_met
  return(pooled)  
  
  logs <- as.data.frame(imps$loggedEvents)
  method <- as.data.frame(imps$method) #check if all variables are imputed correctly
  pm <- as.data.frame(imps$predictorMatrix)
  
  return(logs)
  return(method)
  return(pm)
  
})


results <- bind_rows(lapply(imps_list, tail, 1))
