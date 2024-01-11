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
# Load and assign bioact_seq_list and seq_list for each i
for (i in 1:3) {
  assign(paste0("bioact_seq_", i), readRDS(file.path(data_posted_dir, paste0("01_bioactdemog_", i, ".Rds"))), envir = .GlobalEnv)
  assign(paste0("seq_", i), readRDS(file.path(data_posted_dir, paste0("01_seq_", i, ".Rds"))), envir = .GlobalEnv)
}

seq_all <- readRDS(file.path(data_posted_dir, file="seq_all.Rds"))
dem_all <- readRDS(file.path(data_posted_dir, file="bioact_all.Rds"))

# ...
# Create an empty list to store the dataframes
result_df_list <- list()
seq_list <- list(seq_1, seq_2, seq_3)

# Specify the timeframes
timeframes <- seq(6, 72, by = 6)

# Loop through each child
for (i in 1:3) {
  # Extract the relevant columns for the current child
  seq_all <- seq_list[[i]]
  seq_all[seq_all == "*" | seq_all == "%"] <- NA
  
  # Use the appropriate demographic dataset for the current child
  current_demog <- get(paste0("bioact_seq_", i))
  current_demog <- current_demog[, 1:5]
  
  # Create a list to store dataframes for each timeframe
  result_df_list_child <- list()  # <-- Create a list for each child
  
  for (t in timeframes) {
    
    # Extract the relevant columns for the current timeframe
    seq_subset <- seqdef(seq_all, 1:t, id = "auto", missing = NA)
    
    # Calculate the cumulative complexity for the current timeframe using seqici
    complexity_values <- seqici(seq_subset) * 100
    
   # Create a dataframe with ID, time, and complexity
    subset_result_df <- data.frame(
      id = current_demog$id,
      time = t - 12,
      complexity = complexity_values,
      childno = i
    )
    
    # Append the dataframe to the list for each child
    result_df_list_child[[length(result_df_list_child) + 1]] <- subset_result_df
}
  
  # Combine all dataframes for each child into a single dataframe
  final_result_child <- do.call(rbind, result_df_list_child)
  
  # Sort the combined final dataset by id and time
  final_result_child <- final_result_child %>%
    arrange(id, time)
  
  # Merge demographic data from bioact_seq1-3 to final_result_child
  final_result_child <- merge(final_result_child, current_demog, by = "id", all.x = TRUE)
  
  # Save the final datasets for each child
  saveRDS(final_result_child, file.path(data_posted_dir, paste0("final_complexity_child_", i, ".rds")))
  saveRDS(current_demog, file.path(data_posted_dir, paste0("demog_child_", i, ".rds")))
  
  write.dta(final_result_child, file.path(data_posted_dir, paste0("final_complexity_child_", i, ".dta")))
  write.dta(current_demog, file.path(data_posted_dir, paste0("demog_child_", i, ".dta")))
  
   # Assign the final dataset to the global environment
  assign(paste0("final_result_child_", i), final_result_child, envir = .GlobalEnv)

}

# Close the log file
sink()