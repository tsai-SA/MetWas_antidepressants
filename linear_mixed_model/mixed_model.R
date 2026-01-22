
library(pbapply)
library(lmerTest)

new_never <- met_ad_t0_t1 %>%
  filter(ad_use_longi %in% c("New", "Never"))
  

new_never <- new_never %>%
  mutate(
    across(ad_use_longi, ~ replace(., . == "Never", "0"))
  )

new_never <- new_never %>%
  mutate(
    across(ad_use_longi, ~ replace(., . == "New", "1"))
  )

class(new_never$ad_use_longi)

new_never$ad_use_longi <- as.factor(new_never$ad_use_longi)

install.packages("tibble")


library(pbapply)
library(lmerTest)
library(dplyr)
library(readr)



sig_met_metwas <- readRDS("sig_met_metwas.rds") #sig met from main metwas
new_never <- readRDS("new_never.rds")


str(new_never)

new_never$sex <- as.factor(new_never$sex)
new_never$spectrometer <- as.factor(new_never$spectrometer)
new_never$test_period <- as.factor(new_never$test_period)
new_never$test_month <- as.factor(new_never$test_month)
new_never$time <- as.factor(new_never$time)
saveRDS(new_never, "new_never.rds")

test <- lmer(scale(f.20280) ~ age + sex + bmi + spectrometer + test_month + test_period + ad_use_longi*time + (1 | f.eid), data = new_never)

base_covariates <- c("age", "sex", "spectrometer", "bmi", "test_month", "test_period")
interact_cov <- c("time", "ad_use_longi")

sig_met_metwas$FieldID <- paste0("f.", sig_met_metwas$FieldID) 
ls.met <- sig_met_metwas$FieldID


group_interaction <- function(x.met = ls.met, x.predict, x.dat, x.covs = base_covariates) {
  
  covs.formu = paste0(x.covs, collapse = '+')  #covariates
  
  x.predict = paste0(interact_cov, collapse = '*') #interaction
  
  x.formula = paste0('scale(', x.met, ')~', covs.formu, '+', x.predict, '+', '(1 | f.eid)') %>%
    as.list  ##formula
  
  names(x.formula) = x.met
  
  x.res = names(x.formula) %>% as.list %>%
    pblapply(., function(x) {
      x.iv = x
      x.fit = lmer(as.formula(x.formula[[x]]), data = x.dat)   #linear mixed effect model
      
      x.img.res = summary(x.fit)$coefficients %>%
        tail(n = 1) %>%
        data.frame(.)
      
      # Add the ls.met name as an additional column
      x.img.res$Outcome <- x.iv
      return(x.img.res)
    })
  
  # Combine results
  x.res <- bind_rows(x.res)
  # Rename columns and adjust for FDR
  colnames(x.res) = c('Beta', 'std', 'df', 't', 'p', 'Outcome')
  x.res = x.res %>%
    mutate(p.fdr = p.adjust(p, method = 'fdr'))
  
  # Remove row names
  rownames(x.res) = NULL
  
  return(x.res)
}

new_vs_never_model <- group_interaction(ls.met, interact_cov, new_never, base_covariates)

sum(new_vs_never_model$p.fdr < 0.05)

sig <- new_vs_never_model %>% 
  filter(p < 0.05)

grouping_met_enrich <- readRDS("grouping_met_enrich.rds")
grouping_met_enrich$FieldID <- paste0("f.", grouping_met_enrich$FieldID) 
sig <- rename(sig, FieldID = "Outcome") 
sig <- left_join(sig, grouping_met_enrich, by ="FieldID")

saveRDS(new_vs_never_model, "new_vs_never_model.rds")

