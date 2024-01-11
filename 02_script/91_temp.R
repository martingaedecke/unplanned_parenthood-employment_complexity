# Create a list to store sequence objects for each child
seq_list <- list()

# Iterate over child numbers (1, 2, 3)
for (child_no in 1:3) {
  
  # Filter biochild for the current child number
  child_filter <- biochild$childno == child_no
  biochild_filtered <- biochild[child_filter, ]
  
  # Left join bioact_tot with biochild_filtered based on the id column
  bioact_tot_filtered <- left_join(bioact_tot, biochild_filtered %>% dplyr::select(id, dobk, parenthood_status), by = "id")
  
  # Create a new variable indicating the minimum allowed birthdate for each individual
  bioact_tot_filtered <- bioact_tot_filtered %>%
    group_by(id) %>%
    mutate(min_allowed_birthdate = min(dobk, na.rm = TRUE) - months_before_birth)
  
  # Filter for observations within the specified time frame
  bioact_tot_filtered <- bioact_tot_filtered %>%
    filter(actend > min_allowed_birthdate)
  
  # Create a sequence object for the filtered data
  seq_child <- seqdef(bioact_tot_filtered, id = "id",
                      8:ncol(bioact_tot_filtered),
                      missing = NA, states = shortlab.empl, 
                      cpal = colorpalette,
                      missing.color = 'darkgrey',
                      right = "DEL")
  
  # Store the sequence object in the list
  seq_list[[paste0("child_", child_no)]] <- seq_child
  
}

# Save sequence objects and related data in own datafiles for each child
for (child_no in 1:3) {
  saveRDS(seq_list[[paste0("child_", child_no)]], file.path(data_posted_dir, paste0("seq_child_", child_no, ".Rds")))
}

# Optionally, assign the sequence objects to the list in the global environment
assign("seq_list", seq_list, envir = .GlobalEnv)


colnames(bioact_tot_filtered)


# Filter biochild for the current child number
child_filter <- biochild$childno == 1
biochild_filtered <- biochild[child_filter, ]
