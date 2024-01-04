# ==============================================================================
# Date: January 2024
# Paper: Unplanned parenthood and employment complexity
# Author: Gädecke, Martin
# Script: 04_sequence_complexity
# Input:  01_bioactdemog1-3; 01_seq1-3
# Output: figures, tables
# ==============================================================================

# List all variables in the environment
all_variables <- ls()

# Identify variables with the suffix "_dir"
dir_variables <- grep("_dir$", all_variables, value = TRUE)

# Remove variables that don't have the "_dir" suffix
rm(list = setdiff(all_variables, dir_variables))

# Open the log file
logdate <- format(Sys.Date(), "%Y%m%d")
sink(file.path(log_dir, paste0("04_complexity_", logdate, ".txt")), 
     append = FALSE)

## Read in data

# Specify the path to the folder containing your datasets

# Get a list of files in the folder with a specific extension (e.g., .Rds)
file_list <- list.files(data_posted_dir, pattern = "\\.Rds$", full.names = TRUE)

# Loop through the list of files and import each dataset
for (file_path in file_list) {
  # Extract the dataset name from the file path
  dataset_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Import the dataset using readRDS or your preferred method
  assign(dataset_name, readRDS(file_path), envir = .GlobalEnv)
}
