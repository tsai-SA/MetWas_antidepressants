other_met_data <- readRDS("other_met_data.rds")

str(other_met_data[, 253:279])

## venn diagram on all data

base_covariates <- c("Age", "Assessment_centre", "Sex", "Spectrometer", 
                     "Smoking_status", "Socioeconomic_status", "Education", 
                     "BMI", "Ethnicity", "mdd_nerves", "phq_sumscore", 
                     "Alcohol_status", "Alcohol_freq"
)

ls.met <- colnames(other_met_data)[2:252]

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

other_met_data$other_current_never <- as.factor(other_met_data$other_current_never)
other_met_data$other_current_past <- as.factor(other_met_data$other_current_past)
other_met_data$other_current_future <- as.factor(other_met_data$other_current_future)

other_main_model_never <- met_assoc(ls.met, "other_current_never", other_met_data, base_covariates)
other_main_model_past <- met_assoc(ls.met, "other_current_past", other_met_data, base_covariates)
other_main_model_future <- met_assoc(ls.met, "other_current_future", other_met_data, base_covariates)

other_main_model_never$Significant <- if_else(other_main_model_never$p.fdr < 0.05, "Yes", "No")
other_main_model_past$Significant <- if_else(other_main_model_past$p.fdr < 0.05, "Yes", "No")
other_main_model_future$Significant <- if_else(other_main_model_future$p.fdr < 0.05, "Yes", "No")

grouping_met <- readRDS("grouping_met_enrich.rds")

other_main_model_never <- left_join(other_main_model_never, grouping_met, by = 'Outcome')
other_main_model_past <- left_join(other_main_model_past, grouping_met, by = 'Outcome')
other_main_model_future <- left_join(other_main_model_future, grouping_met, by = 'Outcome')

saveRDS(other_main_model_never, "other_main_model_never.rds")
saveRDS(other_main_model_past, "other_main_model_past.rds")
saveRDS(other_main_model_future, "other_main_model_future.rds")


full.set1.fdr <- other_main_model_never %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set2.fdr <- other_main_model_past %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set3.fdr <- other_main_model_future %>% filter(p.fdr < 0.05) %>% {.$Metabolite}

other_main_venn <- ggVennDiagram(list(full.set1.fdr,full.set2.fdr,full.set3.fdr),label = "count",
                                category.names = c("Current v.s. Never","Current v.s. Past","Current v.s. Future")) +
  scale_fill_distiller(palette = "Paired") +
  scale_x_continuous(expand = expansion(mult = .15))+
  ggtitle('FDR-sig metabolites per set')

jpeg("other_main_venn", width = 646, height = 465)
print(other_main_venn)
dev.off()

sig_other <- other_main_model_never %>%
  filter(Significant == "Yes")

summary(other_main_model_never$Beta)

cor.test(sig_tca$Beta, sig_other$Beta, 
         use="complete.obs", method="pearson")

cor.test(other_main_model_never$Beta, tricyclic_main_model_never$Beta, 
         use="complete.obs", method="pearson")

cor.test(other_main_model_never$Beta, ssri_main_model_never$Beta, 
         use="complete.obs", method="pearson")


