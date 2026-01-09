library(readr)
install.packages("readr")

tricyclic_testing_resid <- readRDS("tricyclic_testing_resid.rds")
write_tsv(tricyclic_testing_resid, "tricyclic_testing_resid.tsv")

testing_IDs <- tricyclic_testing_resid %>%
  select(ID)

write_tsv(testing_IDs, "testing_IDs.tsv")

tricyclic_lasso_coef <- readRDS("tricyclic_lasso_coef.rds")
write_tsv(tricyclic_lasso_coef, "tricyclic_lasso_coef.tsv")

######## Create tricyclic_calc_metscore.R file
## Create metabolic scores in a testing dataset
## Open R terminal and dx download all the files to working env
## Creat /residuals/ folder in home directory and move met_resid_testing.tsv there


## Info on file formats
## met_resid_testing.tsv = residuals of testing dataset
## testing_IDs.tsv = file with a column called ID which matches the ID (colnames) in met_resid_testing.tsv
## lasso_coef.tsv = file with 2 columns: metabolite marker and coefficient from lasso
## metscore_ad.tsv = output file name
### NOTE that files must be in those names

## Create metabolic scores in a testing dataset

# Load required packages ---------------------------------------------------
pkg <- c("dplyr", "data.table", "readr", "tibble", "purrr")

new.pkg <- pkg[!(pkg %in% installed.packages())]
if(length(new.pkg)) install.packages(new.pkg, repos="http://cran.rstudio.com")

library(data.table)
library(dplyr)
library(readr)
library(tibble)
library(purrr)

# Hardcoded file paths -----------------------------------------------------
F_meta      <- "./residuals/tricyclic_testing_resid.tsv"
subID       <- "./testing_IDs.tsv"
F_LassoCoef <- "./tricyclic_lasso_coef.tsv"
output      <- "./tricyclic_metscore_ad.tsv"

# Load data ---------------------------------------------------------------
ID.include   <- read_tsv(subID) %>% .$ID %>% as.vector
coef.training <- read_tsv(F_LassoCoef)
colnames(coef.training) <- c('Marker', 'Weight')

# Logging function ---------------------------------------------------------
log_file <- paste0(output,'.log')
logging <- function(str) { 
  cat(paste0(paste0(str, collapse=''), '\n'), file=log_file, append=TRUE) 
  cat(paste0(paste0(str, collapse=''), '\n')) 
}

# Function to calculate metabolic scores -----------------------------------
calc_MetS <- function(F_mvalue, Obj_pheno, Obj_weight) {
  logging(c('Processing:\t', F_mvalue %>% basename))
  
  # Load metabolite data
  data <- read_tsv(F_mvalue)
  
  # Subset individuals for testing
  met = data %>% .[.$ID %in% ID.include, ]
  
  # Select intersecting metabolites
  probes_ <- intersect(colnames(met), Obj_weight$Marker)
  met.id <- met %>% .[, c('ID', probes_)]
  
  # Reorder weight vector to match metabolite columns
  rownames(Obj_weight) <- Obj_weight$Marker
  Obj_weight <- Obj_weight[probes_, ]
  
  # Calculate metabolic score
  out_score <- met.id %>% select(-ID) %>% as.matrix %*% Obj_weight$Weight
  pred_dep.chr <- data.frame(ID = met.id$ID, Score = out_score)
  
  return(pred_dep.chr)
}

# Run analysis -------------------------------------------------------------
logging('Create MetS')
logging(c("Started: ", date()))
logging(c('M-value file: ', F_meta))
logging(c('Testing sample: ', subID))
logging(c('Lasso regression weights: ', F_LassoCoef))
logging(c('Output file: ', output))
logging(' ')

# Wrap F_meta in a list to use lapply
pred_dep.allCHR <- list(F_meta) %>%
  lapply(FUN = calc_MetS, Obj_pheno = ID.include, Obj_weight = coef.training)

# Combine results
ref.ID <- pred_dep.allCHR[[1]]$ID
pred_dep.allCHR <- pred_dep.allCHR %>%
  purrr::reduce(full_join, by = 'ID') %>%
  select(-ID) %>%
  mutate(total.MetS = rowSums(.)) %>%
  mutate(ID = ref.ID) %>%
  select(ID, total.MetS)

# Save output --------------------------------------------------------------
write_tsv(pred_dep.allCHR, file = output)
logging(c('Output saved: ', output))
logging(c('Ended: ', date()))

###############

```bash
Rscript tricyclic_calc_metscore.R \
--methFolder ./residuals \
--subID testing_IDs.tsv \
--lassoCoef tricyclic_lasso_coef.tsv \
--out tricyclic_metscore_ad.tsv
```


