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

### Calculate average complexity over sequence objects

# Create an empty data frame to store results
results <- data.frame(seq_name = character(),
                      average_complexity = numeric(),
                      std_dev_complexity = numeric())

# List of sequence object names
seq_object_names <- c(
  "01_seq_1", "01_seq_2", "01_seq_3",
  "01_seq1_men", "01_seq1_ph1", "01_seq1_ph2", "01_seq1_ph3", "01_seq1_ph4", "01_seq1_women",
  "01_seq2_men", "01_seq2_ph1", "01_seq2_ph2", "01_seq2_ph3", "01_seq2_ph4", "01_seq2_women",
  "01_seq3_men", "01_seq3_ph1", "01_seq3_ph2", "01_seq3_ph3", "01_seq3_ph4", "01_seq3_women"
)

# Loop through each sequence object
for (seq_name in seq_object_names) {
  # Get the actual sequence object (replace with your loading code)
  seq_object <- readRDS(file.path(data_posted_dir, paste0(seq_name, ".Rds")))
  
  # Calculate sequence complexity
  complexity <- seqici(seq_object, with.missing = FALSE, silent = TRUE)
  
  # Calculate average complexity
  avg_complexity <- mean(complexity, na.rm = TRUE)
  
  # Calculate standard deviation of complexity
  std_dev_complexity <- sd(complexity, na.rm = TRUE)
  
  # Add the results to the data frame
  results <- rbind(results, data.frame(seq_name = seq_name, average_complexity = avg_complexity, std_dev_complexity = std_dev_complexity))
}

# Print the results
print(results)

# Quarto document)
results_table <- kable(results, format = "markdown")

