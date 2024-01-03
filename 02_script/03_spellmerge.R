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

## Load data from bioact, bioact_rtr and biochild, also demograhic data (pairfam 1-14)
bioact <- read_dta(file.path(data_raw_dir, file = "bioact.dta"))
bioact_rtr <- read_dta(file.path(data_raw_dir, file = "bioact_rtr.dta"))
biochild <- read_rds(file.path(data_temp_dir, file="03_pairfam1-14_phstatusdobk.Rds"))
demogdata <- read_rds(file.path(data_temp_dir, file="01_pairfam1-14.Rds"))

# Calculate the maximum value across columns intdatw1, intdatw2, ..., intdatw?
bioact$lastintdat <- apply(bioact[, grep("intdatw", names(bioact))], 1, max, na.rm = TRUE)

bioact <- bioact[c('id', 'activity', 'actspell', 'actbeg', 'actend', 'actcensor', 'lastintdat')]

# Replace right-censored activity end dates with interview date
bioact$actend[bioact$actend == 97] <- bioact$lastintdat[bioact$actend == 97]

# replace actend if missing with interviewdate of wave3 
bioact_rtr$actend_rtr <- ifelse(bioact_rtr$actend_rtr == 97, bioact_rtr$intdatw3, bioact_rtr$actend_rtr)

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
    activity %in% 1:9 ~ "education",
    activity == 18 ~ "military/civilian service",
    activity == 12 ~ "part-time",
    activity == 10 ~ "full-time",
    activity == 11 ~ "self-employed",
    activity == 17 ~ "parental leave/childcare",
    activity %in% 13:16 ~ "marginal employed",
    activity == 19 ~ "unemployed",
    activity %in% c(20, 21, 22) ~ "not employed"
  ))

### Only keep variables that we need: id, activity, actspell, actbeg, actend, actcensor
bioact_tot <- bioact_tot[c('id', 'activity', 'actbeg', 'actend')]
bioact_tot <- bioact_tot %>% arrange(id, actbeg)

### delete rows where everything except for id is missing
bioact_tot <- bioact_tot[complete.cases(bioact_tot[, -1]), ]

### 77539 obs. left

# Drop rows where either actbeg or actend is less than 0
bioact_tot <- bioact_tot[!(bioact_tot$actbeg < 0 | bioact_tot$actend < 0), , drop = FALSE]

### 75080 obs. left

## Now filter bioact_tot to the IDs that are also present in biochild (for which information on parenthood status is present)

# Filter unique IDs from "c"
unique_ids <- distinct(biochild, id)

## 1083 individual persons

# Filter "bioact_tot" based on unique IDs
bioact_tot <- bioact_tot %>% filter(id %in% unique_ids$id)

## Now calculate min value of dobk within whole dataset and add 1 year prior; max value is the last activity value
min_actbeg <- min(biochild$dobk, na.rm = TRUE) - 12
max_actend <- max(bioact_tot$actend, na.rm = TRUE)

## Search for inconsistent cases where actend is smaller than actbeg
inconsistent_cases <- bioact_tot[bioact_tot$actend < bioact_tot$actbeg, ] ### 5 cases
bioact_tot <- bioact_tot[!(bioact_tot$actend < bioact_tot$actbeg),] 

## Filter for observations within timeframe of min_actbeg and max_actend
bioact_tot <- bioact_tot %>%
  filter(actend > min_actbeg, actbeg < max_actend)

### 6996 observations left

## Now create three sequence objects for each child
# Columns to merge on
columns_to_merge <- c("id", "dobk", "childno")

# Create an empty list to store the merged sequence objects
bioact_seq_merged <- list()

# Create an empty list to store filtered datasets
filtered_datasets <- list()
# Initialize bioact_seq as an empty list
bioact_seq <- list()

# Loop through each child
for (i in 1:3) {
  # Filter the user dataset for the current child
  filtered_dataset <- biochild[biochild$childno == i,]
  
  # Merge data for the current child
  merged_df <- merge(bioact_tot, filtered_dataset[, c("id", "dobk", "childno", "parenthood_status")], by = "id", all.x = TRUE)
  
  # Filter out rows where "childno" is TRUE
  filtered_df <- merged_df[!is.na(merged_df$childno), ]
  
  # Optionally, you can assign the filtered dataset to a new variable in the global environment
  assign(paste0("filtered_df_", i), filtered_df, envir = .GlobalEnv)
  
  # Store the filtered dataset in the list
  filtered_datasets[[i]] <- filtered_df
  
  ## USE TRAMINER package to create a sequence object
  bioact_seq[[i]] <- seqformat(filtered_datasets[[i]],
                               from = "SPELL", to = "STS",
                               id = "id", 
                               begin = "actbeg", end = "actend", 
                               status = "activity",
                               covar = c("id", "dobk"),
                               process = FALSE, limit = 21,
                               overwrite = TRUE
                               ) %>%
    seqdef()

  # Optionally, assign the bioact datsets
  assign(paste0("bioact_seq_", i), bioact_seq[[i]], envir = .GlobalEnv)
  }

for (i in 1:3) {
  id <- as.numeric(rownames(get(paste0("bioact_seq_", i))))
  data <- get(paste0("bioact_seq_", i))
  
  # Add the "id" column using cbind
  data <- cbind(id = id, data)
  
  # Update the modified data back to the environment
  assign(paste0("bioact_seq_", i), data, envir = .GlobalEnv)
}


# Left join based on the "id" column
bioact_seq_1 <- left_join(bioact_seq_1, filtered_df_1[c("id", "dobk", "parenthood_status")],
                          by = "id") %>%
  # Keep only unique rows based on "id"
  distinct(id, .keep_all = TRUE)

bioact_seq_2 <- left_join(bioact_seq_2, filtered_df_2[c("id", "dobk", "parenthood_status")],
                          by = "id") %>%
  # Keep only unique rows based on "id"
  distinct(id, .keep_all = TRUE)

bioact_seq_3 <- left_join(bioact_seq_3, filtered_df_3[c("id", "dobk", "parenthood_status")],
                          by = "id") %>%

# Keep only unique rows based on "id"
  distinct(id, .keep_all = TRUE)


# Reorder columns and exclude specific columns
bioact_seq_1 <- relocate(bioact_seq_1, "dobk", "parenthood_status", .after = id)
bioact_seq_2 <- relocate(bioact_seq_2, "dobk", "parenthood_status", .after = id)
bioact_seq_3 <- relocate(bioact_seq_3, "dobk", "parenthood_status", .after = id)

## Merge more person information
variable_list <- c("id","sex_gen", "doby_gen", "dobm_gen", "cohort")

sessionInfo()

bioact_seq_1 <- left_join(bioact_seq_1, demogdata[variable_list], by = "id", 
                          multiple = "first", relationship = "many-to-many")
bioact_seq_2 <- left_join(bioact_seq_2, demogdata[variable_list], by = "id")
bioact_seq_3 <- left_join(bioact_seq_3, demogdata[variable_list], by = "id")




