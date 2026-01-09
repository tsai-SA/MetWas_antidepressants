library(dplyr)
install.packages("pbapply")
library(caret)
library(pbapply)
library(ROSE)
library(tibble)



ssri_met_data <- readRDS("ssri_met_data.rds")

trainIndex <- createDataPartition(ssri_met_data$ssri_current_never, 0.75, list = F, times = 1)

training_data <- ssri_met_data[trainIndex,]
testing_data <- ssri_met_data[-trainIndex,]

saveRDS(training_data, "ssri_training_data.rds")
saveRDS(testing_data, "ssri_testing_data.rds")

base_covariates <- c("Age", "Assessment_centre", "Sex", "Spectrometer", 
                     "Smoking_status", "Socioeconomic_status", "Education", 
                     "BMI", "Ethnicity", "mdd_nerves", "phq_sumscore", 
                     "Alcohol_status", "Alcohol_freq"
)

ls.met <- colnames(training_data)[2:252]

str(training_data[, 253:279])

met_assoc <- function(x.met = ls.met, x.predict, x.dat, x.covs = base_covariates) {
  
  covs.formu = paste0(x.covs, collapse = '+')
  
  x.formula = paste0('scale(', x.met, ')~', covs.formu, '+', x.predict) %>%
    as.list
  names(x.formula) = x.met
  
  # Use pblapply to calculate results
  x.res = names(x.formula) %>% as.list %>%
    pblapply(., function(x) {
      x.iv = x
      x.fit = glm(as.formula(x.formula[[x]]), data = x.dat)
      
      x.img.res = summary(x.fit)$coefficients %>%
        tail(n = 1) %>%
        data.frame(.)
      
      # Add the ls.met name as an additional column
      x.img.res$Outcome <- x.iv
      return(x.img.res)
    })
  
  # Combine results into a single dataframe
  x.res = dplyr::bind_rows(x.res)
  
  # Rename columns and adjust for FDR
  colnames(x.res) = c('Beta', 'std', 't', 'p', 'Outcome')
  x.res = x.res %>%
    mutate(p.fdr = p.adjust(p, method = 'fdr'))
  
  # Remove row names
  rownames(x.res) = NULL
  
  return(x.res)
}

main_model_never <- met_assoc(ls.met, "ssri_current_never", training_data, base_covariates)
main_model_past <- met_assoc(ls.met, "ssri_current_past", training_data, base_covariates)
main_model_future <- met_assoc(ls.met, "ssri_current_future", training_data, base_covariates)

main_model_never$Significant <- if_else(main_model_never$p.fdr < 0.05, "Yes", "No")
main_model_past$Significant <- if_else(main_model_past$p.fdr < 0.05, "Yes", "No")
main_model_future$Significant <- if_else(main_model_future$p.fdr < 0.05, "Yes", "No")

grouping_met <- readRDS("grouping_met.rds")

main_model_never <- left_join(main_model_never, grouping_met, by = 'Outcome')
main_model_past <- left_join(main_model_past, grouping_met, by = 'Outcome')
main_model_future <- left_join(main_model_future, grouping_met, by = 'Outcome')

sig_never <- main_model_never$Outcome[main_model_never$Significant == "Yes"]
sig_past <- main_model_past$Outcome[main_model_past$Significant == "Yes"]
sig_future <- main_model_future$Outcome[main_model_future$Significant == "Yes"]
ls.met <- Reduce(intersect, list(sig_never, sig_past, sig_future))  ###135

saveRDS(main_model_never, "ssri_main_model_never_training.rds")
saveRDS(main_model_past, "ssri_main_model_past_training.rds")
saveRDS(main_model_future, "ssri_main_model_future_training.rds")

#### Residualize metabolites in training and testing data, only sig met found in training data

training_data <- readRDS("ssri_training_data.rds")
testing_data <- readRDS("ssri_testing_data.rds")
ls.met <- Reduce(intersect, list(sig_never, sig_past, sig_future))  ###135

base_covariates <- c(
  "Age", "Assessment_centre", "Sex", "Spectrometer", 
  "Smoking_status", "Socioeconomic_status", "Education", 
  "BMI", "Ethnicity", "Alcohol_status", "Alcohol_freq"
)

get_resid <- function(ls.met,x.in_dat = training_data,x.covs=base_covariates){
  x.formu = paste0('scale(',ls.met,') ~', paste0(x.covs,collapse = '+'))
  x.dat = x.in_dat %>% 
    select(all_of(c('f.eid',x.covs,ls.met))) %>% 
    .[complete.cases(.),]
  x.fit = glm(as.formula(x.formu),data = x.dat)
  output = data.frame(f.eid = x.dat$f.eid, resid_met = resid(x.fit))
  colnames(output)[2] = ls.met
  return(output)
}         
met_resid = ls.met %>% .[. %in% colnames(training_data)] %>% 
  as.list %>% 
  pblapply(.,get_resid,x.in_dat=training_data) %>% 
  purrr::reduce(.,full_join,by='f.eid') 

saveRDS(met_resid, "ssri_met_resid.rds")

testing_data <- readRDS("ssri_testing_data.rds")

get_resid <- function(ls.met,x.in_dat = testing_data,x.covs=base_covariates){
  x.formu = paste0('scale(',ls.met,') ~', paste0(x.covs,collapse = '+'))
  x.dat = x.in_dat %>% 
    select(all_of(c('f.eid',x.covs,ls.met))) %>% 
    .[complete.cases(.),]
  x.fit = glm(as.formula(x.formu),data = x.dat)
  output = data.frame(f.eid = x.dat$f.eid, resid_met = resid(x.fit))
  colnames(output)[2] = ls.met
  return(output)
}

testing_resid = ls.met %>% .[. %in% colnames(testing_data)] %>% 
  as.list %>% 
  pblapply(.,get_resid,x.in_dat=testing_data) %>% 
  purrr::reduce(.,full_join,by='f.eid')

testing_resid <- rename(testing_resid, ID= f.eid)
saveRDS(testing_resid, "ssri_testing_resid.rds")

ssri_met_resid <- readRDS("ssri_met_resid.rds")
meth <- ssri_met_resid 
meth <- rename(meth, ID = f.eid)

pheno.dat <- training_data %>% select(f.eid, ssri_current_never)
pheno.dat <- rename(pheno.dat, ID = f.eid)
meth_pheno <- meth %>% left_join(.,pheno.dat, by ='ID') %>% .[complete.cases(.),] 
rownames(meth_pheno) <- NULL
meth_intersected <- meth_pheno %>%
  select(-ssri_current_never) %>% 
  as.data.frame() %>%
  tibble::column_to_rownames(var = 'ID') 
X.bm <- as.matrix(meth_intersected)
meth_pheno$ssri_current_never <- as.factor(meth_pheno$ssri_current_never)
y.input = meth_pheno$ssri_current_never

tmp <- train(
  x = X.bm, 
  y = y.input, 
  metric = "Kappa",
  method = "glmnet", 
  family = "binomial",
  trControl = trainControl(method = "repeatedcv", 
                           number = 3, 
                           repeats = 5, 
                           sampling = "rose",
                           allowParallel = TRUE)
)

lasso_coef <- as.matrix(coef(tmp$finalModel, 
                             s = tmp$finalModel$lambdaOpt)
)

 
colnames(lasso_coef)[1] <- "coef"
lasso_coef <- rownames_to_column(as.data.frame(lasso_coef), var = "Marker")
lasso_coef <- lasso_coef[-1, ] # Remove intercept row 
lasso_coef <- lasso_coef %>% 
  filter(coef != 0) #remove markers with coef = 0, 131 met left


saveRDS(lasso_coef, "ssri_lasso_coef.rds")
