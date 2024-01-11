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
unique_ids <- distinct(biochild, id, childno)

## 1083 individual persons, 1268 births

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

##################################
### Doing it for the whole dataset
##################################

# Left join bioact_tot with biochild based on the id column
bioact_all <- left_join(bioact_tot,
                        biochild %>% dplyr::select(id, dobk,
                        parenthood_status, childno), by = "id",
                        relationship = "many-to-many")

# Create a new variable indicating the minimum allowed birthdate for each individual
bioact_all <- bioact_all %>%
  group_by(id) %>%
  mutate(min_allowed_birthdate = min(dobk, na.rm = TRUE) - months_before_birth)

# Filter for observations within the specified time frame
bioact_all <- bioact_all %>%
  filter(actend > min_allowed_birthdate)

## USE TRAMINER package to create a sequence object
bioact_seq_all <- seqformat(bioact_all,
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
assign(paste0("bioact_seq_all"), bioact_seq_all, envir = .GlobalEnv)

# Join ID to dataset
id <- as.numeric(rownames(get("bioact_seq_all")))
data <- bioact_seq_all

# Add the "id" column using cbind
data <- cbind(id = id, data)

# Update the modified data back to the environment
assign("bioact_seq_all", data, envir = .GlobalEnv)

# Left join based on the "id" column
bioact_seq_all <- left_join(bioact_seq_all, bioact_all[c("id", "dobk", "parenthood_status")],
                          by = "id") %>%
  # Keep only unique rows based on "id"
  distinct(id, .keep_all = TRUE)

# Reorder columns and exclude specific columns
bioact_seq_all <- relocate(bioact_seq_all, "dobk", "parenthood_status", .after = id)

# Merge more person information
variable_list <- c("id", "sex_gen", "doby_gen", "dobm_gen", "cohort", "d2weight")

bioact_seq_all <- left_join(bioact_seq_all, demogdata[variable_list], by = "id",
                                multiple = "first", relationship = "many-to-many") %>%
    relocate("sex_gen", "doby_gen", "dobm_gen", "cohort", .after = "parenthood_status")

# Number of months to shift for children
months <- 12

# Create a new column for the shifted date
  bioact_seq_all <- bioact_seq_all %>%
  mutate(shifted_date = dobk - months)

# Gather the year columns into long format
  df_long <- bioact_seq_all %>%
    gather(key = "year", value = "activity", starts_with("y")) %>%
    mutate(year = as.numeric(sub("y", "", year)))
  
  # Filter the dataframe based on the timeshifted date
  df_long <- df_long %>%
    filter(year >= shifted_date)
  
  # Calculate the time difference from dobk
  df_long <- df_long %>%
    mutate(time_diff = year - dobk)

variables_to_keep <- c("id", "dobk", "parenthood_status", "sex_gen", "doby_gen",
                       "dobm_gen", "cohort", "d2weight")

# Spread the dataframe back to wide format using pivot_wider
  bioact_seq_all_result <- df_long %>%
    pivot_wider(id_cols = variables_to_keep, names_from = time_diff, values_from = activity, values_fill = NULL)
  
### Attach information on sex_gen, parenthood_status and age at parenthood towards the data


  # Convert year and month to Date objects for the parent's birthdate
  bioact_seq_all_result$dob_parent <- 
    as.Date(paste(bioact_seq_all_result$doby_gen, bioact_seq_all_result$dobm_gen, 1, sep = "-"))
  
  # Calculate the difference in months from January 1900 for the parent's birthdate
  bioact_seq_all_result$dob_parent_months <- as.numeric(difftime(bioact_seq_all_result$dob_parent, as.Date("1900-01-01"), units = "days") / 30.44)
  
  # Round the month to a full month
  bioact_seq_all_result$dob_parent_months <- round(bioact_seq_all_result$dob_parent_months)
  
  # Calculate the age at parenthood
  bioact_seq_all_result$age_at_parenthood <- bioact_seq_all_result$dobk - bioact_seq_all_result$dob_parent_months
  
  # Calculate the age of the parent in full years
  bioact_seq_all_result$age_at_parenthood <- round(bioact_seq_all_result$age_at_parenthood / 12)
  
  # Relocate variables
  bioact_seq_all_result <- bioact_seq_all_result %>% relocate(c("sex_gen", "dobk", "age_at_parenthood", "parenthood_status"), .after = "id")
  
  # Remove non-needed variables
  bioact_seq_all_result <- subset(bioact_seq_all_result, select = -c(dob_parent, doby_gen, dobm_gen, dob_parent_months))  
  

### Assign labels and colors
shortlab.empl <-  c("EDU", "FT", "ME", "NE", "PL", "PT", "SE", "UE")
colorpalette <- divergingx_hcl(8, palette="Spectral")

### Create sequence object
### Missing codes recoding before creating sequence object (% and *)
bioact_seq_all_result <- bioact_seq_all_result %>%
    mutate_all(~ ifelse(. %in% c("*", "%"), NA, .))

# sequence definition
seq_all <- seqdef(bioact_seq_all_result, id = id,
                          8:ncol(bioact_seq_all_result),
                          missing = NA, states = shortlab.empl, 
                          cpal = colorpalette,
                          missing.color = 'darkgrey',
                          right = "DEL")

# Save sequence object and related data in own datafiles
saveRDS(seq_all, file.path(data_posted_dir, "seq_all.Rds"))
saveRDS(bioact_seq_all_result, file.path(data_posted_dir, "bioact_all.Rds"))

# Close the log file
sink()