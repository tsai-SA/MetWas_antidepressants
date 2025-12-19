system("pip install dxpy --upgrade")
system("pip install pandas==1.3.5")

pkg <- c("data.table", "dplyr", "stringr")   #Basic libraries
new.pkg <- pkg[!(pkg %in% installed.packages())]

library(data.table)
library(dplyr)
library(stringr)

project <- Sys.getenv("DX_PROJECT_CONTEXT_ID")

datasets <- system("dx find data --type Dataset --delimiter ','", intern = TRUE)

dataset_info <- data.frame(
  prefix = str_extract(datasets, "/([^,]+)\\.dataset") |> str_remove("^/"),
  record = str_split(datasets, ",") |> sapply(`[`, 5),
  stringsAsFactors = FALSE
)

latest_idx <- which(dataset_info$prefix == max(dataset_info$prefix))
dataset_info[latest_idx, ]
dataset_prefix <- dataset_info$prefix[latest_idx]
dataset_record <- dataset_info$record[latest_idx]

dataset_id <- paste0(project, ":", dataset_record)

data_dict_file <- paste0(dataset_prefix, ".data_dictionary.csv")

if (!file.exists(data_dict_file)) {
  message("Extracting dataset data dictionary...")
  system(paste0("dx extract_dataset ", dataset_id, " -ddd"), intern = TRUE)
} else {
  message("Data dictionary already exists. Using existing file.")
}

datadict <- data.table::fread(data_dict_file)
field_schema <- data.table::fread('/mnt/project/Showcase metadata/field.tsv') #For list of met fields

category_of_interest <- c(20002)

category_fields_extract <- field_schema |>
  dplyr::filter(main_category %in% category_of_interest) |>
  dplyr::pull(field_id)

field_names <- sapply(category_of_interest, function(f) {
  regex <- paste0('^p', f, '(?![0-9])')
  datadict %>%
    filter(str_detect(name, regex)) %>%
    pull(name)
})

field_names <- c("eid", field_names)  # include participant ID

fields_file <- paste0(dataset_prefix, "_fields.txt")
write.table(field_names, fields_file, row.names = FALSE, col.names = FALSE, quote = FALSE)

file_upload <- system(paste0("dx upload ", fields_file, " --brief"), intern = TRUE)
file_id <- str_extract(file_upload, "file-[A-Za-z0-9]+")

output_name <- paste0(dataset_prefix, "_phenotype_data")
system(paste0(
  "dx run table-exporter ",
  "-idataset_or_cohort_or_dashboard=", dataset_id, " ",
  "-ifield_names_file_txt=", file_id, " ",
  "-ientity=participant ",
  "-ioutput=", output_name, " ",
  "-iheader_style=FIELD-TITLE"
), intern = TRUE)
message("Table-exporter job submitted. Output folder: ", output_name)

system("dx download file-J52Z960JYqk9VvYQvjpJJjPj", intern = TRUE)
illness <- fread("app4844_20251209141336.dataset_phenotype_data.csv")


illness_baseline <- illness %>%
  select("Participant ID", contains("Instance 0")) 

list_illness <- as.data.frame(table(illness_baseline$"Non-cancer illness code, self-reported | Instance 0 | Array 0"))

illness_baseline <- illness_baseline %>%
  mutate(type_1_diabetes = as.integer(if_any(
    everything(),
    ~ grepl("type 1 diabetes", .)
  )))

illness_baseline$type_1_diabetes <- as.factor(illness_baseline$type_1_diabetes)
table(illness_baseline$type_1_diabetes)

illness_baseline <- illness_baseline %>%
  mutate(type_2_diabetes = as.integer(if_any(
    everything(),
    ~ grepl("type 2 diabetes", .)
  )))

illness_baseline <- illness_baseline %>%
  mutate(rheumatoid_arthritis = as.integer(if_any(
    everything(),
    ~ grepl("rheumatoid arthritis", .)
  )))

illness_baseline <- illness_baseline %>%
  mutate(kidney_failure = as.integer(if_any(
    everything(),
    ~ grepl("kidney failure", .)
  )))

illness_baseline <- illness_baseline %>%
  mutate(kidney_nephropathy = as.integer(if_any(
    everything(),
    ~ grepl("kidney nephropathy", .)
  )))

illness_baseline <- illness_baseline %>%
  mutate(glomerulnephritis = as.integer(if_any(
    everything(),
    ~ grepl("glomerulnephritis", .)
  )))

illness_baseline <- illness_baseline %>%
  mutate(migraine = as.integer(if_any(
    everything(),
    ~ grepl("migraine", .)
  )))

illness_baseline <- illness_baseline %>%
  mutate(systemic_lupus = as.integer(if_any(
    everything(),
    ~ grepl("systemic lupus", .)
  )))

illness_baseline <- illness_baseline %>%
  mutate(erectile_dysfunction = as.integer(if_any(
    everything(),
    ~ grepl("erectile dysfunction", .)
  )))

illness_baseline_list <- illness_baseline %>%
  select("Participant ID", type_1_diabetes:erectile_dysfunction)

met_data <- readRDS("met_data_9th.rds")

met_data <- merge(met_data, illness_baseline_list, by = "Participant ID")

#### Complex model
base_covariates <- c("Age", "Assessment_centre", "Sex", "Spectrometer", 
                     "Smoking_status", "Socioeconomic_status", "Education", 
                     "BMI", "Ethnicity", "mdd_nerves", "phq_sumscore", 
                     "Alcohol_status", "Alcohol_freq", 
                     "type_1_diabetes", "type_2_diabetes", 
                     "rheumatoid_arthritis", "atrial_fibrillation", 
                     "kidney_failure", "kidney_nephropathy", 
                     "glomerulnephritis", "migraine", "systemic_lupus", 
                     "erectile_dysfunction")

met_data <- met_data %>%
  mutate(across(270:279, as.factor))

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

grouping_met <- readRDS("grouping_met.rds")

#change col names to ukb met field number
met_data <- met_data %>% 
  rename_with(~ str_replace_all(., " \\| Instance 0", ""))

new_names <- setNames(as.character(grouping_met$Outcome), grouping_met$Metabolite)
colnames(met_data) <- ifelse(colnames(met_data) %in% grouping_met$Metabolite, 
                             new_names[colnames(met_data)], 
                             colnames(met_data))

ls.met <- colnames(met_data)[2:252]

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

complex_model_never <- met_assoc(ls.met, "Current_vs_Never", met_data, base_covariates)
complex_model_past <- met_assoc(ls.met, "Current_vs_Past", met_data, base_covariates)
complex_model_future <- met_assoc(ls.met, "Current_vs_Future", met_data, base_covariates)

complex_model_never <- left_join(complex_model_never, grouping_met, by = 'Outcome')
complex_model_past <- left_join(complex_model_past, grouping_met, by = 'Outcome')
complex_model_future <- left_join(complex_model_future, grouping_met, by = 'Outcome')

full.set1.fdr <- complex_model_never %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set2.fdr <- complex_model_past %>% filter(p.fdr < 0.05) %>% {.$Metabolite}
full.set3.fdr <- complex_model_future %>% filter(p.fdr < 0.05) %>% {.$Metabolite}


complex_venn <- ggVennDiagram(list(full.set1.fdr,full.set2.fdr,full.set3.fdr),label = "count",
                           category.names = c("Current v.s. Never","Current v.s. Past","Current v.s. Future")) +
  scale_fill_distiller(palette = "Paired") +
  scale_x_continuous(expand = expansion(mult = .15))+
  ggtitle('FDR-sig metabolites per set')

saveRDS(complex_model_never, "complex_model_never.rds")
saveRDS(complex_model_past, "complex_model_past.rds")
saveRDS(complex_model_future, "complex_model_future.rds")
saveRDS(met_data, "met_data.rds")
