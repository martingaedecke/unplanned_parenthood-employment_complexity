
## Load data from bioact
bioact <- read_dta(file.path(data_raw_dir, file = "bioact.dta"))

# Calculate the maximum value across columns intdatw1, intdatw2, ..., intdatw?
bioact$lastintdat <- apply(bioact[, grep("intdatw", names(bioact))], 1, max, na.rm = TRUE)

bioact <- bioact[c('id', 'activity', 'actspell', 'actbeg', 'actend', 'actcensor', 'lastintdat')]

# Replace right-censored activity end dates with interview date
bioact$actend[bioact$actend == 97] <- bioact$lastintdat[bioact$actend == 97]

## Load data from bioact_rtr
bioact_rtr <- read_dta(file.path(data_raw_dir, file = "bioact_rtr.dta"))

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
bioact_tot <- bioact_tot[c('id', 'activity', 'actspell', 'actbeg', 'actend', 'actcensor')]
bioact_tot <- bioact_tot %>% arrange(id, actbeg)

### delete rows where everything except for id is missing
bioact_tot <- bioact_tot[complete.cases(bioact_tot[, -1]), ]

### 77539 obs. left

# Drop rows where either actbeg or actend is less than 0
bioact_tot <- bioact_tot[!(bioact_tot$actbeg < 0 | bioact_tot$actend < 0), , drop = FALSE]

### 75080 obs. left

## Now calculate min and max value of actbeg and actend within whole dataset

min_actbeg <- min(bioact_tot$actbeg, na.rm = TRUE)
max_actend <- max(bioact_tot$actend, na.rm = TRUE)

# Assuming bioact_tot is your data frame

library(dplyr)
library(tidyr)

# Calculate min and max values of actbeg and actend within the whole dataset
min_actbeg <- min(bioact_tot$actbeg, na.rm = TRUE)
max_actend <- max(bioact_tot$actend, na.rm = TRUE)

# Extract unique IDs from bioact_tot
unique_ids <- unique(bioact_tot$id)

# Create a new data frame with one row per unique ID
new_df <- data.frame(id = unique_ids)

# Create a sequence of column names
column_names <- paste0("state", min_actbeg:max_actend)

# Add columns for each state variable with value NA
new_df[column_names] <- NA

merged_df <- merge(new_df, bioact_tot, by = "id", all.x = TRUE)

# Update state variables based on actbegin and actend
for (i in min_actbeg:max_actend) {
  merged_df[[paste0("state", i)]] <- ifelse(bioact_tot$actbeg <= i & i <= bioact_tot$actend, bioact_tot$activity, merged_df[[paste0("state", i)]])
}

