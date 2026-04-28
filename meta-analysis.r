library(dplyr)
library(tidyverse)
install.packages("meta")
library(meta)


setwd("/Users/angelatsaii/Desktop/aduse-metwas-nesda/complex_model_mdd_current/")

nesda_complex_mdd <- readRDS("nesda_complex_model_mdd_MetS_AD_coefficients.rds")
head(nesda_complex_mdd)

complex_mdd <- nesda_complex_mdd %>% slice(2)
complex_mdd$cohorts <- "nesda"


# Add a row using nrow()
complex_mdd[nrow(complex_mdd) + 1, ] <- c("scale(AD_MetS)", 0.31706074, 0.12963202, 2.4458521, 0.014451028, "alspac")
head(complex_mdd)

complex_mdd$Estimate <- as.numeric(complex_mdd$Estimate)
complex_mdd$"Std. Error" <- as.numeric(complex_mdd$"Std. Error")
complex_mdd$"z value" <- as.numeric(complex_mdd$"z value")
complex_mdd$"Pr(>|z|)" <- as.numeric(complex_mdd$"Pr(>|z|)")

complex_mdd <- rename(complex_mdd, SE=`Std. Error`)

test_meta <- meta::metagen(
  TE = Estimate,
  seTE = SE,
  studlab = cohorts,
  sm = "OR",
  data = complex_mdd,
  random = TRUE,
  method.tau = "DL",
  title = "Antidepressant exposure ~ MetScore"
)
