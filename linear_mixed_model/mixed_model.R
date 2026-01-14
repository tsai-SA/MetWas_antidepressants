library(pbapply)
library(lmerTest)


str(longi_t0_t1)


longi_t0_t1 <- longi_t0_t1 %>%
  mutate(
    across(ad_use_longi, ~ replace(., . == "Never", "0"))
  )

longi_t0_t1 <- longi_t0_t1 %>%
  mutate(
    across(ad_use_longi, ~ replace(., . == "New", "1"))
  )

class(longi_t0_t1$ad_use_longi)

longi_t0_t1$ad_use_longi <- as.factor(longi_t0_t1$ad_use_longi)


str(longi_t0_t1[, 240:260])

longi_t0_t1$sex <- as.factor(longi_t0_t1$sex)
longi_t0_t1$spectrometer <- as.factor(longi_t0_t1$spectrometer)
longi_t0_t1$test_period <- as.factor(longi_t0_t1$test_period)
longi_t0_t1$test_month <- as.factor(longi_t0_t1$test_month)
longi_t0_t1$time <- as.factor(longi_t0_t1$time)

test <- lmer(scale(f.20280) ~ age + sex + bmi + spectrometer + test_month + test_period + ad_use_longi*time + (1 | f.eid), data = longi_t0_t1)

base_covariates <- c("age", "sex", "spectrometer", "bmi", "test_month", "test_period")
interact_cov <- c("time", "ad_use_longi")
ls.met <- colnames(longi_t0_t1)[3:253]


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

new_vs_never_model <- group_interaction(ls.met, interact_cov, longi_t0_t1, base_covariates)
