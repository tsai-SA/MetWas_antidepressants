###################################################
#### Script Name: Extract_met_age_sex
#### Author: Somying Tsai
#### Description: script to extract metabolites, age, sex from your project environment to R for imputation
#### Date : 04/12/2025

# ---------------------------
# Step 0: Setting up
# -----------------------------
system("pip install dxpy --upgrade")
system("pip install pandas==1.3.5")

pkg <- c("data.table", "dplyr", "stringr")   #Basic libraries
new.pkg <- pkg[!(pkg %in% installed.packages())]

library(data.table)
library(dplyr)
library(stringr)

# -----------------------------
# Step 1: Get your RAP project ID
# -----------------------------
project <- Sys.getenv("DX_PROJECT_CONTEXT_ID")

# -----------------------------
# Step 2: List all the datasets available in the project, all data are already in your project environment, 
# Steps 2 and 3 aren’t about discovering new datasets; they’re about picking the dataset to extract variables from, especially if multiple versions exist.
# -----------------------------
datasets <- system("dx find data --type Dataset --delimiter ','", intern = TRUE)

# -----------------------------
# Step 3: Pick latest dataset (by timestamp in prefix)
# -----------------------------
# Extract prefixes and record IDs
dataset_info <- data.frame(
  prefix = str_extract(datasets, "/([^,]+)\\.dataset") |> str_remove("^/"),
  record = str_split(datasets, ",") |> sapply(`[`, 5),
  stringsAsFactors = FALSE
)

# Select the latest dataset by prefix (assumes timestamp in name)
latest_idx <- which(dataset_info$prefix == max(dataset_info$prefix))
dataset_info[latest_idx, ]
dataset_prefix <- dataset_info$prefix[latest_idx]
dataset_record <- dataset_info$record[latest_idx]

# Construct full DNAnexus dataset ID
dataset_id <- paste0(project, ":", dataset_record)

# -----------------------------
# Step 4: Extract data dictionary (if not exists)
# -----------------------------
data_dict_file <- paste0(dataset_prefix, ".data_dictionary.csv")

if (!file.exists(data_dict_file)) {
  message("Extracting dataset data dictionary...")
  system(paste0("dx extract_dataset ", dataset_id, " -ddd"), intern = TRUE)
} else {
  message("Data dictionary already exists. Using existing file.")
}

# -----------------------------
# Step 5: Load data dictionary
# -----------------------------
datadict <- data.table::fread(data_dict_file)
field_schema <- data.table::fread('/mnt/project/Showcase metadata/field.tsv') #For list of met fields

# -----------------------------
# Step 6: Extract sex, age, assessment centre, spectrometer, BMI, GP registration info
# -----------------------------
fields_of_interest <- c(21001, 54, 23650, 21003, 31, 42038)
category_of_interest = NULL

category_fields_extract <- field_schema |>
  dplyr::filter(main_category %in% category_of_interest) |>
  dplyr::pull(field_id)

category_fields = c(category_fields_extract, fields_of_interest)

# Convert to column names in DNAnexus data
field_names <- lapply(category_fields, function(f) {
  regex <- paste0('^p', f, '(?![0-9])')
  datadict %>%
    filter(str_detect(name, regex)) %>%
    pull(name)
}) |>
  unlist(use.names = FALSE) |>
  unique()


field_names <- c("eid", field_names)  # include participant ID

# -----------------------------
# Step 7: Save field list for table-exporter
# -----------------------------
fields_file <- paste0(dataset_prefix, "_fields.txt")
write.table(field_names, fields_file, row.names = FALSE, col.names = FALSE, quote = FALSE)

# -----------------------------
# Step 8: Upload field list to RAP
# -----------------------------
file_upload <- system(paste0("dx upload ", fields_file, " --brief"), intern = TRUE)
file_id <- str_extract(file_upload, "file-[A-Za-z0-9]+")

# -----------------------------
# Step 9: Run table-exporter
# -----------------------------
output_name <- paste0(dataset_prefix, "_phenotype_data")

system(paste0(
  "dx run table-exporter ",
  "-idataset_or_cohort_or_dashboard=", dataset_id, " ",
  "-ifield_names_file_txt=", file_id, " ",
  "-ientity=participant ",
  "-ioutput=phenotype_export ",
  "-iheader_style=FIELD-NAME ",
  "--yes"
), intern = TRUE)

message("Table-exporter job submitted. Output folder: ", output_name)

# -----------------------------
# Step 10: Load data to Rstudio
# -----------------------------

message("Table-exporter job submitted. Output folder: ", output_name) # Look for output file name like app4844_20251202064400.dataset_phenotype_data.csv
system("dx describe app4844_20251209141336.dataset_phenotype_data.csv", intern = TRUE) # Look for file ID, like file-J4Q33x8J7Kb7v0z577QVg7bV
system("dx download file-J5Yj5z0JK1YP49kFY3x777Vv", intern = TRUE)
repeat_asse_pheno <- fread("app4844_20251209141336.dataset_phenotype_data.csv")  # Load the data to R

# -----------------------------
# Step 11: Save file to project enviroment 
# -----------------------------

#upload file to project space
saveRDS(age_sex, "age_sex.rds") # you need to save your file first in your session environment
file_upload <- system('dx upload age_sex.rds', intern = TRUE) # upload file into project
