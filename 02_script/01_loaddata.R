# Date:     December 2023
# Paper:    Unplanned parenthood and employment complexity
# Author:   Gädecke, Martin
# Script:   01_datamerge
# Purpose:  Merge the data and save it into one single file
# Output:   01_pairfam1-14_temp.Rds

# Key variables:
# - frt5 - ideal no of children (W1-W13)
# - frt6 - realistic no of children W1,W2
# - frt7 - Intention to become mother/father within next 2 years (W1-W13)
# - frt26, 27 - (more) children realistic (W3-W13)

# List all variables in the environment
all_variables <- ls()

# Identify variables with the suffix "_dir"
dir_variables <- grep("_dir$", all_variables, value = TRUE)

# Remove variables that don't have the "_dir" suffix
rm(list = setdiff(all_variables, dir_variables))

# Open the log file
logdate <- format(Sys.Date(), "%Y%m%d")
sink(file.path(log_dir, paste0("01_datamerge_", logdate, ".txt")), append = FALSE)

# Specify the variables you want to include
selected_variables <- c("id", "wave", "sample", "sex_gen", "psex_gen",
                        "age", "page", "cohort", "cob", "pcob", "mcob", "fcob",
                        "migstatus", "relstat", "marstat",
                        "infertile", "pregnant", "nkids",
                        "frt3", "frt5", "frt7", "pid",
                        "k1type", "k2type", "k3type")


# Wave
wave <- 14

# Merge waves 12/13
for (num in 12:13) {
  file_capi <- paste0("anchor", num, "_capi.dta")
  file_cati <- paste0("anchor", num, "_cati.dta")
  
  anchor_num <- read_dta(file.path(data_raw_dir, file_capi))
  anchor_num <- rbind(anchor_num, read_dta(file.path(data_raw_dir, file_cati)))
  saveRDS(anchor_num, file.path(data_raw_dir, sprintf("anchor%d.Rds", num)))
}

## Merge wave 14
# Set the wave number
num <- 14

# Define file names
file_capi <- paste0("anchor", num, "_capi.dta")
file_cawi <- paste0("anchor", num, "_cawi.dta")

# Read the Stata files
anchor_capi <- read_dta(file.path(data_raw_dir, file_capi))
anchor_cawi <- read_dta(file.path(data_raw_dir, file_cawi))

# Combine the data frames
anchor_num <- rbind(anchor_capi, anchor_cawi)

# Save the combined data frame
saveRDS(anchor_num, file.path(data_raw_dir, sprintf("anchor%d.Rds", num)))

### Waves 1-14
# Read anchor1 and anchor1_DD
anchor1 <- haven::read_dta(file.path(data_raw_dir, "anchor1.dta"))
anchor1_DD <- haven::read_dta(file.path(data_raw_dir, "anchor1_DD.dta"))

# Convert labeled variables to regular variables
anchor1 <- haven::zap_labels(anchor1)
anchor1_DD <- haven::zap_labels(anchor1_DD)

# Check column names and structure
cat("Column names in anchor1: ", names(anchor1), "\n")
cat("Column names in anchor1_DD: ", names(anchor1_DD), "\n")

# Identify the missing columns
missing_columns <- setdiff(names(anchor1), names(anchor1_DD))

# Add missing columns to anchor1_DD with NA values
for (col in missing_columns) {
  anchor1_DD[[col]] <- NA
}

# Combine the data frames
merged_data <- rbind(anchor1, anchor1_DD)

# Save merged_data as an RDS file in the raw_data directory with the name "anchor1.Rds"
saveRDS(merged_data, file.path(data_raw_dir, "anchor1.Rds"))

# Read anchor1 and anchor1_DD
anchor1 <- haven::read_dta(file.path(data_raw_dir, "anchor1.dta"))
anchor1_DD <- haven::read_dta(file.path(data_raw_dir, "anchor1_DD.dta"))

# Convert labeled variables to regular variables
anchor1 <- haven::zap_labels(anchor1)
anchor1_DD <- haven::zap_labels(anchor1_DD)

# Check column names and structure
cat("Column names in anchor1: ", names(anchor1), "\n")
cat("Column names in anchor1_DD: ", names(anchor1_DD), "\n")

# Identify the missing columns
missing_columns <- setdiff(names(anchor1), names(anchor1_DD))

# Add missing columns to anchor1_DD with NA values
for (col in missing_columns) {
  anchor1_DD[[col]] <- NA
}

# Combine the data frames
merged_data <- rbind(anchor1, anchor1_DD)

# Save merged_data as an RDS file in the raw_data directory with the name "anchor1.Rds"
saveRDS(merged_data, file.path(data_raw_dir, "anchor1.Rds"))

# Function to read Stata file, convert to .Rds, and save
read_and_save <- function(wave_num) {
  file_name <- sprintf("anchor%d.dta", wave_num)
  data <- haven::read_dta(file.path(data_raw_dir, file_name))
  data <- haven::zap_labels(data)
  saveRDS(data, file.path(data_raw_dir, sprintf("anchor%d.Rds", wave_num)))
  return(data)
}

# Read and save datasets for waves 2-11
data_list <- lapply(2:11, read_and_save)

### Only include variables that I'd need at the end

# Function to read and filter a wave file
read_and_filter_wave <- function(wave_num) {
  file_path <- file.path(data_raw_dir, sprintf("anchor%d.Rds", wave_num))
  data <- readRDS(file_path)
  data <- select(data, selected_variables)
  return(data)
}

# Read and filter datasets for waves 1-14
filtered_data_list <- lapply(c(1:14), read_and_filter_wave)

# Combine filtered datasets into a single dataframe
final_merged_data <- bind_rows(filtered_data_list)

# Save the final merged dataset as an RDS file
saveRDS(final_merged_data, file.path(data_temp_dir, "01_pairfam1-14.Rds"))

# Clear workspace
rm(list = ls())

# Move on to -> 02_datacleaning

# Close the log file
sink()
