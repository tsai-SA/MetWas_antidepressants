met_data <- readRDS("met_data_9th.rds")

base_covariates <- c("Age", "Assessment_centre", "Sex", "Spectrometer", 
                     "Smoking_status", "Socioeconomic_status", "Education", 
                     "BMI", "Ethnicity", "mdd_nerves", "phq_sumscore", 
                     "Alcohol_status", "Alcohol_freq")


library(dplyr)
install.packages("pbapply")
library(pbapply)
library(stringr)
install.packages("ggVennDiagram")
library(ggVennDiagram)
library(ggplot2)



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


met_data$Sex <- as.factor(met_data$Sex)
met_data$Age <- as.integer(met_data$Age)
met_data$Current_vs_Never <- as.factor(met_data$Current_vs_Never)
met_data$Current_vs_Past <- as.factor(met_data$Current_vs_Past)
met_data$Current_vs_Future <- as.factor(met_data$Current_vs_Future)
met_data$Assessment_centre <- as.factor(met_data$Assessment_centre)
met_data$Spectrometer <- as.factor(met_data$Spectrometer)
met_data$Smoking_status <- as.factor(met_data$Smoking_status)
met_data$Socioeconomic_status <- as.numeric(met_data$Socioeconomic_status)
met_data$Education <- as.factor(met_data$Education)
met_data$BMI <- as.numeric(met_data$BMI)
met_data$Ethnicity <- as.factor(met_data$Ethnicity)
met_data$mdd_nerves <- as.factor(met_data$mdd_nerves)
met_data$phq_sumscore <- as.integer(met_data$phq_sumscore)
met_data$Alcohol_status <- as.factor(met_data$Alcohol_status)
met_data$Alcohol_freq <- as.factor(met_data$Alcohol_freq)

str(met_data[, 253:269])

grouping_met <- readRDS("grouping_met.rds")

#change col names to ukb met field number
met_data <- met_data %>% 
  rename_with(~ str_replace_all(., " \\| Instance 0", ""))

new_names <- setNames(as.character(grouping_met$Outcome), grouping_met$Metabolite)
colnames(met_data) <- ifelse(colnames(met_data) %in% grouping_met$Metabolite, 
                                  new_names[colnames(met_data)], 
                                  colnames(met_data))

ls.met <- colnames(met_data)[2:252]

main_model_never <- met_assoc(ls.met, "Current_vs_Never", met_data, base_covariates)
main_model_past <- met_assoc(ls.met, "Current_vs_Past", met_data, base_covariates)
main_model_future <- met_assoc(ls.met, "Current_vs_Future", met_data, base_covariates)

main_model_never$sig <- if_else(main_model_never$p.fdr < 0.05, "Yes", "No")
main_model_past$sig <- if_else(main_model_past$p.fdr < 0.05, "Yes", "No")
main_model_future$sig <- if_else(main_model_future$p.fdr < 0.05, "Yes", "No")

main_model_never <- left_join(main_model_never, grouping_met, by = 'Outcome')
main_model_past <- left_join(main_model_past, grouping_met, by = 'Outcome')
main_model_future <- left_join(main_model_future, grouping_met, by = 'Outcome')

full.set1.fdr <- main_model_never %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set2.fdr <- main_model_past %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set3.fdr <- main_model_future %>% filter(p.fdr < 0.05) %>% {.$Metabolite}

main_venn <- ggVennDiagram(list(full.set1.fdr,full.set2.fdr,full.set3.fdr),label = "count",
                           category.names = c("Current v.s. Never","Current v.s. Past","Current v.s. Future")) +
  scale_fill_distiller(palette = "Paired") +
  scale_x_continuous(expand = expansion(mult = .15))+
  ggtitle('FDR-sig metabolites per set')

saveRDS(main_model_never, "main_model_never.rds")
saveRDS(main_model_past, "main_model_past.rds")
saveRDS(main_model_future, "main_model_future.rds")

#### Basic model
base_covariates <- c("Age", "Assessment_centre", "Sex", "Spectrometer")
base_model_never <- met_assoc(ls.met, "Current_vs_Never", met_data, base_covariates)
base_model_past <- met_assoc(ls.met, "Current_vs_Past", met_data, base_covariates)
base_model_future <- met_assoc(ls.met, "Current_vs_Future", met_data, base_covariates)

base_model_never <- left_join(base_model_never, grouping_met, by = 'Outcome')
base_model_past <- left_join(base_model_past, grouping_met, by = 'Outcome')
base_model_future <- left_join(base_model_future, grouping_met, by = 'Outcome')

full.set1.fdr <- base_model_never %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set2.fdr <- base_model_past %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set3.fdr <- base_model_future %>% filter(p.fdr < 0.05) %>% {.$Metabolite}

base_venn <- ggVennDiagram(list(full.set1.fdr,full.set2.fdr,full.set3.fdr),label = "count",
                           category.names = c("Current v.s. Never","Current v.s. Past","Current v.s. Future")) +
  scale_fill_distiller(palette = "Paired") +
  scale_x_continuous(expand = expansion(mult = .15))+
  ggtitle('FDR-sig metabolites per set')

jpeg("base_venn.jpeg", width = 1200, height = 900, res = 150)
print(base_venn)
dev.off()

saveRDS(base_model_never, "base_model_never.rds")
saveRDS(base_model_past, "base_model_past.rds")
saveRDS(base_model_future, "base_model_future.rds")

save.image("main_basic_models.RData")

