# Date: December 2023
# Paper: Unplanned parenthood and employment complexity
# Author: Gädecke, Martin
# Script: 01_datamerge
# Purpose: Merge the data and save it into one single file
# Output: 01_pairfam1-14.Rds

# List all variables in the environment
all_variables <- ls()

# Identify variables with the suffix "_dir"
dir_variables <- grep("_dir$", all_variables, value = TRUE)

# Remove variables that don't have the "_dir" suffix
rm(list = setdiff(all_variables, dir_variables))

# Key variables
selected_variables <- c("id", "wave", "sample", "sex_gen", "psex_gen",
                        "age", "page", "cohort", "doby_gen", "dobm_gen",
                        "cob", "pcob", "mcob", "fcob",
                        "migstatus", "relstat", "marstat",
                        "infertile", "pregnant", "nkids",
                        "frt3", "frt5", "frt7", "pid",
                        "k1type", "k2type", "k3type", "d2weight")

# Open the log file
logdate <- format(Sys.Date(), "%Y%m%d")
sink(file.path(log_dir, paste0("01_datamerge_", logdate, ".txt")), append = FALSE)

# Merge waves 12/13
for (num in 12:13) {
  file_capi <- paste0("anchor", num, "_capi.dta")
  file_cati <- paste0("anchor", num, "_cati.dta")
  
  anchor_num <- read_dta(file.path(data_raw_dir, file_capi))
  anchor_num <- rbind(anchor_num, read_dta(file.path(data_raw_dir, file_cati)))
  saveRDS(anchor_num, file.path(data_raw_dir, sprintf("anchor%d.Rds", num)))
}

# Merge wave 14
num <- 14
file_capi <- paste0("anchor", num, "_capi.dta")
file_cawi <- paste0("anchor", num, "_cawi.dta")

anchor_capi <- read_dta(file.path(data_raw_dir, file_capi))
anchor_cawi <- read_dta(file.path(data_raw_dir, file_cawi))
anchor_num <- rbind(anchor_capi, anchor_cawi)
saveRDS(anchor_num, file.path(data_raw_dir, sprintf("anchor%d.Rds", num)))

# Read and save datasets for waves 2-11
data_list <- lapply(2:11, function(wave_num) {
  file_name <- sprintf("anchor%d.dta", wave_num)
  data <- read_dta(file.path(data_raw_dir, file_name))
  saveRDS(data, file.path(data_raw_dir, sprintf("anchor%d.Rds", wave_num)))
  return(data)
})

# Read and filter datasets for waves 1-14
filtered_data_list <- lapply(1:14, function(wave_num) {
  data <- readRDS(file.path(data_raw_dir, sprintf("anchor%d.Rds", wave_num)))
  
  # Check the column names in your dataset
  print(colnames(data))
  
  # Use dplyr::select explicitly to avoid conflicts
  data <- dplyr::select(data, selected_variables)
  
  return(data)
})

# Combine filtered datasets into a single dataframe
final_merged_data <- bind_rows(filtered_data_list)

# Save the final merged dataset as an RDS file
saveRDS(final_merged_data, file.path(data_temp_dir, "01_pairfam1-14.Rds"))

# Close the log file
sink()
