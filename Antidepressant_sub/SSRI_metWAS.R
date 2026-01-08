library(dplyr)
library(pbapply)
library(ggVennDiagram)
library(ggplot2)
library(stringr)
install.packages("ggVennDiagram")

str(ssri_met_data[, 253:279])

## venn diagram on all data

base_covariates <- c("Age", "Assessment_centre", "Sex", "Spectrometer", 
                     "Smoking_status", "Socioeconomic_status", "Education", 
                     "BMI", "Ethnicity", "mdd_nerves", "phq_sumscore", 
                     "Alcohol_status", "Alcohol_freq"
)

ls.met <- colnames(ssri_met_data)[2:252]

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

ssri_met_data$ssri_current_never <- as.factor(ssri_met_data$ssri_current_never)
ssri_met_data$ssri_current_past <- as.factor(ssri_met_data$ssri_current_past)
ssri_met_data$ssri_current_future <- as.factor(ssri_met_data$ssri_current_future)

ssri_main_model_never <- met_assoc(ls.met, "ssri_current_never", ssri_met_data, base_covariates)
ssri_main_model_past <- met_assoc(ls.met, "ssri_current_past", ssri_met_data, base_covariates)
ssri_main_model_future <- met_assoc(ls.met, "ssri_current_future", ssri_met_data, base_covariates)

ssri_main_model_never$Significant <- if_else(ssri_main_model_never$p.fdr < 0.05, "Yes", "No")
ssri_main_model_past$Significant <- if_else(ssri_main_model_past$p.fdr < 0.05, "Yes", "No")
ssri_main_model_future$Significant <- if_else(ssri_main_model_future$p.fdr < 0.05, "Yes", "No")

grouping_met <- readRDS("grouping_met_enrich.rds")

ssri_main_model_never <- left_join(ssri_main_model_never, grouping_met, by = 'Outcome')
ssri_main_model_past <- left_join(ssri_main_model_past, grouping_met, by = 'Outcome')
ssri_main_model_future <- left_join(ssri_main_model_future, grouping_met, by = 'Outcome')

saveRDS(ssri_main_model_never, "ssri_main_model_never.rds")
saveRDS(ssri_main_model_past, "ssri_main_model_past.rds")
saveRDS(ssri_main_model_future, "ssri_main_model_future.rds")


full.set1.fdr <- ssri_main_model_never %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set2.fdr <- ssri_main_model_past %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set3.fdr <- ssri_main_model_future %>% filter(p.fdr < 0.05) %>% {.$Metabolite}

ssri_main_venn <- ggVennDiagram(list(full.set1.fdr,full.set2.fdr,full.set3.fdr),label = "count",
                           category.names = c("Current v.s. Never","Current v.s. Past","Current v.s. Future")) +
  scale_fill_distiller(palette = "Paired") +
  scale_x_continuous(expand = expansion(mult = .15))+
  ggtitle('FDR-sig metabolites per set')

jpeg("ssri_main_venn", width = 646, height = 465)
print(ssri_main_venn)
dev.off()
