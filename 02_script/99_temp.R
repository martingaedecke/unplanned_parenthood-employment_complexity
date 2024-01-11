## unused comments

# Extract unique IDs from bioact_tot
unique_ids <- unique(bioact_tot$id)

# Create a new data frame with one row per unique ID
new_df <- data.frame(id = unique_ids)

# Create a sequence of column names
column_names <- paste0("state", min_actbeg:max_actend)

# Add columns for each state variable with value NA
new_df[column_names] <- NA

# Identify unique activity states
unique_states <- unique(bioact_tot$activity)

# Function to check for overlapping intervals
check_overlap <- function(actbeg1, actend1, actbeg2, actend2) {
  if (actend1 >= actbeg2 && actend2 >= actbeg1) {
    return(TRUE)  # Overlapping
  } else {
    return(FALSE) # Not overlapping
  }
}

# Identify cases where intervals overlap for the same ID
bioact_tot <- bioact_tot %>%
  group_by(id) %>%
  mutate(double = ifelse(n() > 1, any(sapply(1:(n() - 1), function(i) any(check_overlap(actbeg[i], actend[i], actbeg[(i + 1):n()], actend[(i + 1):n()])))), FALSE)) %>%
  ungroup()

# Check cases where double == FALSE
filtered_rows <- bioact_tot %>%
  filter(double == FALSE)

# Print the resulting dataframe
view(filtered_rows)
