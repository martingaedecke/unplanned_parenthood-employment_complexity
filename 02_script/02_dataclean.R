# ==============================================================================
# Date: December 2023
# Paper: Unplanned parenthood and employment complexity
# Author: Gädecke, Martin
# Script: 02_dataclean
# Input:  01_pairfam1-14.Rds
# Output: 02_pairfam1-14.Rds
# ==============================================================================

rm(list = ls())

# Load data
data_temp <- readRDS(file.path(data_temp_dir, "01_pairfam1-14.Rds"))

# Code negative values as missings
data_temp[data_temp < 0] <- NA

# Sort the data frame by id and wave
data_temp <- data_temp[order(data_temp$id, data_temp$wave), ]

# Create a new variable 'd_chslw' indicating the change in the number of children
data_temp$d_chslw <- with(data_temp, nkids - lag(nkids))

# Recode 'd_chslw' to handle special cases
data_temp$d_chslw <- ifelse(data_temp$d_chslw %in% c(-4, -3, -2, -1),
                            NA, # For cases like 'ch passed away/ moved out'
                            ifelse(data_temp$d_chslw %in% c(2, 3, 4, 5, 6),
                                   2, # For cases like 'twins/triplets and more'
                                   data_temp$d_chslw))

# Order the data by ID, wave and d_chslw
data_temp %>% select(id, wave, d_chslw, everything())


# Use select() to reorder the variables
data_temp <- select(all_of(desired_order))

# Sort the data frame by id, wave, and d_chslw
data_temp <- data_temp[order(data_temp$id, data_temp$wave, data_temp$d_chslw), ]

# Drop rows where 'd_chslwtot' is zero
data_temp <- subset(data_temp, ave(data_temp$d_chslw, data_temp$id, FUN = sum) != 0)

# Filter out rows where 'd_chslwtot' is zero
data_temp <- data_temp[ave(data_temp$d_chslw, data_temp$id, FUN = sum) != 0, ]

# Sort the data by 'id' and 'wave'
data_temp <- data_temp %>% arrange(id, wave)

# Replace 'd_chslw' with its lag value for individuals with twins/triplets
data_temp <- data_temp %>%
  group_by(id) %>%
  mutate(d_chslw = ifelse(lag(d_chslw) == 2, lag(d_chslw), d_chslw))

# Sort the data again
data_temp <- data_temp %>% arrange(id, wave)

# Drop individuals with twins/triplets
data_temp <- data_temp %>% filter(d_chslw != 2)

# Sort the data by 'id' and 'wave'
data_temp <- data_temp %>% arrange(id, wave)

# Create 'childno' variable for the number of children born when dummy_pregchild == 1
data_temp <- data_temp %>%
  mutate(childno = ifelse(d_chslw == 1, nkids, NA))

# Print the frequencies of 'childno'
table_childno <- table(data_temp$childno)
table_childno

# Sort the data again
data_temp <- data_temp %>% arrange(id, wave)

# Recode 'k1type', 'k2type', and 'k3type' into 'childtype1', 'childtype2', and 'childtype3'
for (num in 1:3) {
  col_name <- paste0("childtype", num)
  data_temp <- data_temp %>%
    mutate(
      !!col_name :=
        case_when(
          get(paste0("k", num, "type")) %in% c(2, 3) ~ NA_real_,
          get(paste0("k", num, "type")) %in% c(4, 7) ~ NA_real_,
          get(paste0("k", num, "type")) %in% c(5, 6) ~ 2,
          get(paste0("k", num, "type")) %in% c(8, 9) ~ 1,
          TRUE ~ NA_real_
        )
    )
}

# Print the frequencies of 'd_chslw'
table_d_chslw <- table(data_temp$d_chslw)
table_d_chslw

# Load the required packages
library(dplyr)
library(tidyr)
library(tsibble)

# Sort the data by 'id' and 'wave'
data_temp <- data_temp %>% arrange(id, wave)

# Create 'flag_tsfill' variable
data_temp <- data_temp %>%
  group_by(id) %>%
  mutate(flag_tsfill = ifelse(!is.na(id), 1, NA)) %>%
  ungroup()

# Convert 'wave' to numeric
data_temp$wave <- as.numeric(data_temp$wave)

# Convert to time series data
data_temp_ts <- data_temp %>%
  as_tsibble(key = id, index = wave)

# Fill missing waves using tsfill
data_temp_ts_filled <- data_temp_ts %>%
  fill_gaps(wave = full_seq(wave, 1L), .full = TRUE)

# Convert back to data frame
data_temp_filled <- as.data.frame(data_temp_ts_filled)

# Sort the data again
data_temp_filled <- data_temp_filled %>% arrange(id, wave)

# Save cleaned data
saveRDS(file.path(data_temp_dir, "02_pairfam1-14.Rds"))

# Close the log file
sink()
