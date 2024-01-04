# ==============================================================================
# Date: January 2024
# Paper: Unplanned parenthood and employment complexity
# Author: Gädecke, Martin
# Script: 04_sequence_visualisation
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
sink(file.path(log_dir, paste0("04_visualisation_", logdate, ".txt")), 
     append = FALSE)

# Read in data
loaded_bioact_seq_list <- list()
loaded_seq_list <- list()

for (i in 1:3) {
  loaded_bioact_seq_list[[i]] <-  readRDS(file.path(data_posted_dir, 
                                  paste0("01_bioactdemog_", i, ".Rds")))
  loaded_seq_list[[i]] <-         readRDS(file.path(data_posted_dir, 
                                  paste0("01_seq_", i, ".Rds")))
  
  # Assign the loaded datasets to the global environment
  assign(paste0("bioact_seq_", i), loaded_bioact_seq_list[[i]], 
         envir = .GlobalEnv)
  assign(paste0("seq_", i), loaded_seq_list[[i]], 
         envir = .GlobalEnv)
}

# Remove the lists from the global environment
rm(loaded_bioact_seq_list, loaded_seq_list, envir = .GlobalEnv)

### Some preparations for visualisation
# Get Legend box
source(file.path(script_dir, file = "99_legendbox.R"))

# Corresponding entropy values
stateentropy <- list()
for (i in 1:3) {
  seq_name <- paste0("seq_", i)
  stateentropy[[i]] <- seqstatd(get(seq_name))$Entropy
  assign(paste0("stateentropy.seq", i), stateentropy[[i]], envir = .GlobalEnv)
}
rm(stateentropy)

# Colors & legend
colorpalette <- divergingx_hcl(8, palette="Spectral")
col.legend <- c(colorpalette, "white")
lab.legend <- c(attributes(seq_1)$labels, "Entropy")
bcol.legend <- c(rep("black",9), "white")

seqdplot(seq_3)





