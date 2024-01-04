# ==============================================================================
# Date: December 2023
# Paper: Unplanned parenthood and employment complexity
# Author: Gädecke, Martin
# Script: 03_spellmerge
# Input:  03_pairfam1-14_phstatusdobk.Rds, bioact, bioact_rtr
# Output: 04_pairfam1-14.Rds
# ==============================================================================

# List all variables in the environment
all_variables <- ls()

# Identify variables with the suffix "_dir"
dir_variables <- grep("_dir$", all_variables, value = TRUE)

# Remove variables that don't have the "_dir" suffix
rm(list = setdiff(all_variables, dir_variables))

# Open the log file
logdate <- format(Sys.Date(), "%Y%m%d")
sink(file.path(log_dir, paste0("01_spelldata_", logdate, ".txt")), append = FALSE)

## Load data from bioact, bioact_rtr and biochild, also demograhic data 
## (pairfam 1-14)
bioact <- read_dta(file.path(data_raw_dir, 
                              file = "bioact.dta"))
bioact_rtr <- read_dta(file.path(data_raw_dir, 
                                  file = "bioact_rtr.dta"))
biochild <- read_rds(file.path(data_temp_dir, 
                                file="03_pairfam1-14_phstatusdobk.Rds"))
demogdata <- read_rds(file.path(data_temp_dir, file="01_pairfam1-14.Rds"))

# Calculate the maximum value across columns intdatw1, intdatw2,intdatw
bioact$lastintdat <- apply(bioact[, grep("intdatw", names(bioact))], 
                           1, max, na.rm = TRUE)

bioact <- bioact[c('id', 
                   'activity', 
                   'actspell', 
                   'actbeg', 
                   'actend', 
                   'actcensor', 
                   'lastintdat')]

# Replace right-censored activity end dates with interview date
bioact$actend[bioact$actend == 97] <- bioact$lastintdat[bioact$actend == 97]

# replace actend if missing with interviewdate of wave3 
bioact_rtr$actend_rtr <- ifelse(bioact_rtr$actend_rtr == 97, 
                                bioact_rtr$intdatw3, bioact_rtr$actend_rtr)

bioact_rtr <- bioact_rtr %>%
  rename(
    actend = actend_rtr,
    actbeg = actbeg_rtr,
    activity = activity_rtr,
    actspell = actspell_rtr,
    actcensor = actcensor_rtr,
    )

# Append data
bioact_tot <- bind_rows(bioact, bioact_rtr)

bioact_tot <- bioact_tot %>%
  mutate(activity = case_when(
    activity %in% c(1:9, 18) ~ "EDU",
    ## activity == 18 ~ "MCS", for now i included MCS in EDU (04/01/2024)
    activity == 12 ~ "PT",
    activity == 10 ~ "FT",
    activity == 11 ~ "SE",
    activity == 17 ~ "PL",
    activity %in% 13:16 ~ "ME",
    activity == 19 ~ "UE",
    activity %in% c(20, 21, 22) ~ "NE"
  ))

### Check frequencies and percentages
bioact_tot_summary <- bioact_tot %>%
  group_by(activity) %>%
  summarise(
    count = n(),
    percentage = n() / nrow(bioact_tot) * 100
  ) %>%
  arrange(desc(count))

# Print the summary table
print(bioact_tot_summary)

### for quarto document:
# library(knitr)
# 
# kable(bioact_tot_summary, format = "markdown", col.names = c("Activity", "Count", "Percentage"), align = "c")

### Only keep variables that we need: id, activity, actspell, 
### actbeg, actend, actcensor
bioact_tot <- bioact_tot[c('id', 'activity', 'actbeg', 'actend')]
bioact_tot <- bioact_tot %>% arrange(id, actbeg)

### delete rows where everything except for id is missing
bioact_tot <- bioact_tot[complete.cases(bioact_tot[, -1]), ]

### 77539 obs. left

# Drop rows where either actbeg or actend is less than 0
bioact_tot <- bioact_tot[!(bioact_tot$actbeg < 0 | bioact_tot$actend < 0), , 
                         drop = FALSE]

### 75080 obs. left

# Now filter bioact_tot to the IDs that are also present in 
# biochild (for which information on parenthood status is present)

# Filter unique IDs from "c"
unique_ids <- distinct(biochild, id)

## 1083 individual persons

# Filter "bioact_tot" based on unique IDs
bioact_tot <- bioact_tot %>% filter(id %in% unique_ids$id)

## Now calculate min value of dobk within whole dataset and add 1 year prior; 
## max value is the last activity value
min_actbeg <- min(biochild$dobk, na.rm = TRUE) - 12
max_actend <- max(bioact_tot$actend, na.rm = TRUE)

## Search for inconsistent cases where actend is smaller than actbeg
inconsistent_cases <- bioact_tot[bioact_tot$actend < bioact_tot$actbeg, ] # N=5 
bioact_tot <- bioact_tot[!(bioact_tot$actend < bioact_tot$actbeg),] 

# Set the desired time frame around childbirth seperately for child1, 2 and 3
months_before_birth <- 12

# Create an empty list to store filtered datasets
filtered_datasets <- list()

# Loop through each child (1, 2, and 3)
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
  
  # Store the filtered dataset in the list
  filtered_datasets[[child_no]] <- bioact_tot_filtered

  # Assign the filtered dataset to the global environment
  assign(paste0("filtered_dataset_", child_no), bioact_tot_filtered, envir = .GlobalEnv)

}

# Create an empty list to store the merged sequence objects
bioact_seq_merged <- list()

# Loop through each child (1, 2, and 3)
for (i in 1:3) {
  # Use the already filtered dataset for the current child
  filtered_dataset <- filtered_datasets[[i]]
  
  ## USE TRAMINER package to create a sequence object
  bioact_seq_merged[[i]] <- seqformat(filtered_dataset,
                                      from = "SPELL", to = "STS",
                                      id = "id", 
                                      begin = "actbeg", end = "actend", 
                                      status = "activity",
                                      covar = c("id", "dobk"),
                                      process = FALSE,
                                      overwrite = TRUE,
                                      limit = 21) %>%
    seqdef()
  
  # Optionally, assign the bioact datasets to the list
  assign(paste0("bioact_seq_", i), bioact_seq_merged[[i]], envir = .GlobalEnv)
}

### hier weitermachen
for (i in 1:3) {
  id <- as.numeric(rownames(get(paste0("bioact_seq_", i))))
  data <- get(paste0("bioact_seq_", i))
  
  # Add the "id" column using cbind
  data <- cbind(id = id, data)
  
  # Update the modified data back to the environment
  assign(paste0("bioact_seq_", i), data, envir = .GlobalEnv)
}

# Left join based on the "id" column
bioact_seq_1 <- left_join(bioact_seq_1, filtered_dataset_1[c("id", "dobk", "parenthood_status")],
                          by = "id") %>%
  # Keep only unique rows based on "id"
  distinct(id, .keep_all = TRUE)

bioact_seq_2 <- left_join(bioact_seq_2, filtered_dataset_2[c("id", "dobk", "parenthood_status")],
                          by = "id") %>%
  # Keep only unique rows based on "id"
  distinct(id, .keep_all = TRUE)

bioact_seq_3 <- left_join(bioact_seq_3, filtered_dataset_3[c("id", "dobk", "parenthood_status")],
                          by = "id") %>%
  
  # Keep only unique rows based on "id"
  distinct(id, .keep_all = TRUE)


# Reorder columns and exclude specific columns
bioact_seq_1 <- relocate(bioact_seq_1, "dobk", "parenthood_status", .after = id)
bioact_seq_2 <- relocate(bioact_seq_2, "dobk", "parenthood_status", .after = id)
bioact_seq_3 <- relocate(bioact_seq_3, "dobk", "parenthood_status", .after = id)

# Define data frames and filtered data frames
bioact_seqs <- list(bioact_seq_1, bioact_seq_2, bioact_seq_3)

# Merge more person information in a loop
variable_list <- c("id", "sex_gen", "doby_gen", "dobm_gen", "cohort")

for (i in seq_along(bioact_seqs)) {
  bioact_seqs[[i]] <- left_join(bioact_seqs[[i]], demogdata[variable_list], by = "id",
                                multiple = "first", relationship = "many-to-many") %>%
    relocate("sex_gen", "doby_gen", "dobm_gen", "cohort", .after = "parenthood_status")
}

# Assign the modified data frames back to their original names
bioact_seq_1 <- bioact_seqs[[1]]
bioact_seq_2 <- bioact_seqs[[2]]
bioact_seq_3 <- bioact_seqs[[3]]

# Number of months to shift for children ==3
months <- 12
num_children <- 3

# Loop through each child
for (i in 1:num_children) {
  # Access the bioact_seq for the current child
  bioact_seq <- get(paste0("bioact_seq_", i))
  
  # Create a new column for the shifted date
  bioact_seq <- bioact_seq %>%
    mutate(shifted_date = dobk - months)
  
  # Gather the year columns into long format
  df_long <- bioact_seq %>%
    gather(key = "year", value = "activity", starts_with("y")) %>%
    mutate(year = as.numeric(sub("y", "", year)))
  
  # Filter the dataframe based on the timeshifted date
  df_filtered <- df_long %>%
    filter(year >= shifted_date)
  
  # Calculate the time difference from dobk
  df_filtered <- df_filtered %>%
    mutate(time_diff = year - dobk)
  
  # Spread the dataframe back to wide format using pivot_wider
  df_result <- df_filtered %>%
    pivot_wider(id_cols = id, names_from = time_diff, values_from = activity, values_fill = NULL)
  
  # Assign the result back to the original bioact_seq
  assign(paste0("bioact_seq_", i), df_result, envir = .GlobalEnv)
}

### Attach information on sex_gen, parenthood_status and age at parenthood towards the data
# List of bioact_seq data frames
bioact_seq_list <- list(bioact_seq_1, bioact_seq_2, bioact_seq_3)
filtered_datasets <- list(filtered_dataset_1, filtered_dataset_2, 
                          filtered_dataset_3)

# Loop through each data frame in the list
for (i in seq_along(bioact_seq_list)) {
  
  # Merge information for sex_gen, doby, dobm, and parenthood_status to dataframe
  bioact_seq_list[[i]] <- left_join(bioact_seq_list[[i]], demogdata[c("id", "sex_gen", "doby_gen", "dobm_gen")],
                                    by = "id") %>%
    distinct(id, .keep_all = TRUE)  # Keep only the first row for each id
  
   # Use semi_join to keep only the observations in both data frames
  bioact_seq_list[[i]] <- left_join(bioact_seq_list[[i]], 
                                  filtered_datasets[[i]][c("id", "dobk", "parenthood_status")],
                                  by = "id") %>%
  distinct(id, .keep_all = TRUE)  # Keep only the first row for each id  

    # Convert year and month to Date objects for the parent's birthdate
  bioact_seq_list[[i]]$dob_parent <- as.Date(paste(bioact_seq_list[[i]]$doby_gen, bioact_seq_list[[i]]$dobm_gen, 1, sep = "-"))
  
  # Calculate the difference in months from January 1900 for the parent's birthdate
  bioact_seq_list[[i]]$dob_parent_months <- as.numeric(difftime(bioact_seq_list[[i]]$dob_parent, as.Date("1900-01-01"), units = "days") / 30.44)
  
  # Round the month to a full month
  bioact_seq_list[[i]]$dob_parent_months <- round(bioact_seq_list[[i]]$dob_parent_months)
  
  # Calculate the age at parenthood
  bioact_seq_list[[i]]$age_at_parenthood <- bioact_seq_list[[i]]$dobk - bioact_seq_list[[i]]$dob_parent_months
  
  # Calculate the age of the parent in full years
  bioact_seq_list[[i]]$age_at_parenthood <- round(bioact_seq_list[[i]]$age_at_parenthood / 12)
  
  # Relocate variables
  bioact_seq_list[[i]] <- bioact_seq_list[[i]] %>% relocate(c("sex_gen", "dobk", "age_at_parenthood", "parenthood_status"), .after = "id")
  
  # Remove non-needed variables
  bioact_seq_list[[i]] <- subset(bioact_seq_list[[i]], select = -c(dob_parent, doby_gen, dobm_gen, dob_parent_months))  
  
  # Assign the result back to the original bioact_seq
  assign(paste0("bioact_seq_", i), bioact_seq_list[[i]], envir = .GlobalEnv)

}

### Assign labels and colors
shortlab.empl <-  c("EDU", "FT", "ME", "NE", "PL", "PT", "SE", "UE")
colorpalette <- divergingx_hcl(8, palette="Spectral")

### Create sequence objects
bioact_seq_list <- list(bioact_seq_1, bioact_seq_2, bioact_seq_3)
seq_list <- list()

# Loop through each data frame in the list
for (i in seq_along(bioact_seq_list)) {
  # We have to recode the missing values in the dataframes bioact_seq*
  bioact_seq_list[[i]] <- bioact_seq_list[[i]] %>%
    mutate_all(~ ifelse(. %in% c("*", "%"), NA, .))
  
  # sequence definition
  seq_list[[i]] <- seqdef(bioact_seq_list[[i]], 
                          6:ncol(bioact_seq_list[[i]]),
                          missing = NA, states = shortlab.empl, 
                          cpal = colorpalette,
                          missing.color = 'darkgrey',
                          right = "DEL")
  
  assign(paste0("seq_", i), seq_list[[i]], envir = .GlobalEnv)
}

### Save sequence objects and responding demographic data in datasets
for (i in 1:3) {
  saveRDS(bioact_seq_list[[i]], file.path(data_posted_dir, paste0("01_bioactdemog_", i, ".Rds")))
  saveRDS(seq_list[[i]], file.path(data_posted_dir, paste0("01_seq_", i, ".Rds")))
}

## Define different sequence objects (by men and women)
# List of bioact_seq data frames
bioact_seq_list <- list(bioact_seq_1, bioact_seq_2, bioact_seq_3)

# Loop through each data frame in the list
for (i in seq_along(bioact_seq_list)) {
  # Subset data for men
  seq_men <- bioact_seq_list[[i]][bioact_seq_list[[i]]$sex_gen == 1, ]
  seq_men <- seqdef(seq_men, 6:ncol(seq_men),
                    missing = NA, states = shortlab.empl,
                    cpal = colorpalette,
                    missing.color = 'darkgrey',
                    right = "DEL")
  
  # Save the sequence object for men
  saveRDS(seq_men, file.path(data_posted_dir, paste0("01_seq", i, "_men.Rds")))
  
  # Subset data for women
  seq_women <- bioact_seq_list[[i]][bioact_seq_list[[i]]$sex_gen == 2, ]
  seq_women <- seqdef(seq_women, 6:ncol(seq_women),
                      missing = NA, states = shortlab.empl,
                      cpal = colorpalette,
                      missing.color = 'darkgrey',
                      right = "DEL")
  
  # Save the sequence object for women
  saveRDS(seq_women, file.path(data_posted_dir, paste0("01_seq", i, "_women.Rds")))
}

## Define different sequence objects (by parenthood_status)
# List of bioact_seq data frames
bioact_seq_list <- list(bioact_seq_1, bioact_seq_2, bioact_seq_3)

# Loop through each data frame in the list
for (i in seq_along(bioact_seq_list)) {
  
  # Subset data for child1 and ph==1 -> Planned
  seq_ph1 <- bioact_seq_list[[i]][bioact_seq_list[[i]]$parenthood_status == 1, ]
  seq_ph1 <- seqdef(seq_ph1, 6:ncol(seq_ph1),
                    missing = NA, states = shortlab.empl,
                    cpal = colorpalette,
                    missing.color = 'darkgrey',
                    right = "DEL")
  
  # Save the sequence object
  saveRDS(seq_ph1, file.path(data_posted_dir, paste0("01_seq", i, "_ph1.Rds")))
  
  # Subset data for child1 and ph==2 -> Intended
  seq_ph2 <- bioact_seq_list[[i]][bioact_seq_list[[i]]$parenthood_status == 2, ]
  seq_ph2 <- seqdef(seq_ph2, 6:ncol(seq_ph2),
                    missing = NA, states = shortlab.empl,
                    cpal = colorpalette,
                    missing.color = 'darkgrey',
                    right = "DEL")
  
  # Save the sequence object
  saveRDS(seq_ph2, file.path(data_posted_dir, paste0("01_seq", i, "_ph2.Rds")))
  
  # Subset data for child1 and ph==3 -> Sooner-than-intended
  seq_ph3 <- bioact_seq_list[[i]][bioact_seq_list[[i]]$parenthood_status == 3, ]
  seq_ph3 <- seqdef(seq_ph3, 6:ncol(seq_ph3),
                    missing = NA, states = shortlab.empl,
                    cpal = colorpalette,
                    missing.color = 'darkgrey',
                    right = "DEL")
  
  # Save the sequence object
  saveRDS(seq_ph3, file.path(data_posted_dir, paste0("01_seq", i, "_ph3.Rds")))
  
  # Subset data for child1 and ph==4 -> Unintended
  seq_ph4 <- bioact_seq_list[[i]][bioact_seq_list[[i]]$parenthood_status == 4, ]
  seq_ph4 <- seqdef(seq_ph4, 6:ncol(seq_ph4),
                    missing = NA, states = shortlab.empl,
                    cpal = colorpalette,
                    missing.color = 'darkgrey',
                    right = "DEL")
  
  # Save the sequence object
  saveRDS(seq_ph4, file.path(data_posted_dir, paste0("01_seq", i, "_ph4.Rds")))
}

# Close the log file
sink()