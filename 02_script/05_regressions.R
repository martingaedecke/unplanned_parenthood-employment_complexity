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
sink(file.path(log_dir, paste0("05_regressions_", logdate, ".txt")), 
     append = FALSE)

## Read in data
# Load and assign bioact_seq_list and seq_list for each i
for (i in 1:3) {
  assign(paste0("demog_seq_", i), 
          readRDS(file.path(data_posted_dir, 
          paste0("demog_child_", i, ".Rds"))), envir = .GlobalEnv)
  assign(paste0("complexity_seq_", i), 
          readRDS(file.path(data_posted_dir, 
          paste0("final_complexity_child_", i, ".Rds"))), envir = .GlobalEnv)
}

### Linear models
library(marginaleffects)
lm_model <- lm(C ~ time + parenthood_status + sex_gen + time * parenthood_status * sex_gen, data = complexity_seq_3)

plot_predictions(lm_model, condition = list("time", "parenthood_status", "sex_gen"), conf_level = 0.95)
