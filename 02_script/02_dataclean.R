# ==============================================================================
# Date: December 2023
# Paper: Unplanned parenthood and employment complexity
# Author: Gädecke, Martin
# Script: 02_dataclean
# Input:  01_pairfam1-14.Rds
# Output: 02_pairfam1-14.Rds
# ==============================================================================

# List all variables in the environment
all_variables <- ls()

# Identify variables with the suffix "_dir"
dir_variables <- grep("_dir$", all_variables, value = TRUE)

# Remove variables that don't have the "_dir" suffix
rm(list = setdiff(all_variables, dir_variables))

# Open the log file
logdate <- format(Sys.Date(), "%Y%m%d")
sink(file.path(log_dir, paste0("01_clean_", logdate, ".txt")), append = FALSE)

# Load data
data_temp <- readRDS(file.path(data_temp_dir, "01_pairfam1-14.Rds"))

# Code negative values as missings
data_temp[data_temp < 0] <- NA

# Sort the data frame by id and wave
data_temp <- data_temp[order(data_temp$id, data_temp$wave), ]

# Create a new variable 'd_chslw' indicating the change in the number of children
data_temp <- data_temp %>%
  group_by(id) %>%
  mutate(d_chslw = ifelse(row_number() == 1, NA, nkids - lag(nkids)))

# Recode 'd_chslw' to handle special cases
data_temp$d_chslw <- case_when(
  data_temp$d_chslw < 0 ~ NA_real_,
  data_temp$d_chslw %in% c(2, 3, 4, 5, 6) ~ 2,
  TRUE ~ data_temp$d_chslw
)

# Print the frequency table
frequency_table <- table(data_temp$d_chslw)
print(frequency_table)

## 3707 births of one child in that timeframe, 192 births of twins/triples etc.

data_temp_temp <- data_temp

# Order the data by ID, wave and d_chslw
data_temp <- data_temp_temp %>% relocate(id, wave, d_chslw, .before = sample, everything())

data_temp_temp <- NULL

# Drop IDs where the total sum of d_chslw is zero (didn't become parents throughout survey)
data_temp <- data_temp %>%
  group_by(id) %>%
  filter(any(d_chslw %in% c(1, 2)))

## -> 28118 observations left

# Sort the data by 'id' and 'wave'
data_temp <- data_temp %>% arrange(id, wave)

# # # Replace 'd_chslw' with its lag value for individuals with twins/triplets
# # data_temp <- data_temp %>%
# #   group_by(id) %>%
# #   mutate(d_chslw = ifelse(lag(d_chslw) == 2, lag(d_chslw), d_chslw))
# 
# # Drop individuals with twins/triplets
# data_temp <- data_temp %>%
#   group_by(id) %>%
#   filter(all(d_chslw != 2))

# Create 'childno' variable for the number of children born when dummy_pregchild == 1
data_temp <- data_temp %>%
  mutate(childno = ifelse(d_chslw == 1, nkids, NA))

# Print the frequencies of 'childno'
table_childno <- table(data_temp$childno)
table_childno

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

## 3707 births of one child and 192 of twins/triplets etc.

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

# install.packages("devtools")
devtools::install_github("NickCH-K/pmdplyr")
library(pmdplyr)

data_temp <- panel_fill(data_temp_ts, .set_NA = TRUE,
                                  .i = id, .t = wave)

# Convert back to data frame
data_temp <- as.data.frame(data_temp)

# Sort the data again
data_temp <- data_temp %>% arrange(id, wave)

##### Create planned / unplanned parenthood variable

data_temp <- data_temp %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(
    # Lagged variables
    lag_pregnant = lag(pregnant),
    lag_frt3 = lag(frt3),
    lag_frt5 = lag(frt5),
    lag_nkids = lag(nkids),
    lag_frt7 = lag(frt7),
    
    parenthood_status = factor(
      case_when(
        # Planned
        d_chslw == 1 & lag_pregnant == 0 & lag_frt3 == 1 & lag_frt5 > lag_nkids & (lag_frt7 == 1 | lag_frt7 == 2) ~ 1,
        
        # Intended
        d_chslw == 1 & lag_pregnant == 0 & lag_frt3 == 2 & lag_frt5 > lag_nkids & (lag_frt7 == 1 | lag_frt7 == 2) ~ 2,
        
        # Sooner-than-intended
        d_chslw == 1 & lag_pregnant == 0 & lag_frt3 == 2 & lag_frt5 > lag_nkids & (lag_frt7 == 3 | lag_frt7 == 4 | lag_frt7 == 7) ~ 3,
        
        # Unintended
        d_chslw == 1 & lag_pregnant == 0 & lag_frt3 == 2 & lag_frt5 <= lag_nkids ~ 4,
        
        # Default case (no change in parenthood status)
        TRUE ~ NA_integer_
      ),
      levels = c(NA, 1, 2, 3, 4),
      labels = c("planned", "intended", "sooner-than-intended", "unintended")
    )
  ) %>%
  ungroup()

## Now for those who have been pregnant in the year before (fertility intentions from two waves before)

data_temp <- data_temp %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(
    # Lagged variables based on year-2 (_n-2)
    lag2_pregnant = lag(pregnant, 2),
    lag2_frt3 = lag(frt3, 2),
    lag2_frt5 = lag(frt5, 2),
    lag2_nkids = lag(nkids, 2),
    lag2_frt7 = lag(frt7, 2),
    
    parenthood_status = factor(
      case_when(
        # Planned
        d_chslw == 1 & lag_pregnant == 1 & lag2_frt3 == 1 & lag2_frt5 > lag2_nkids & (lag2_frt7 == 1 | lag2_frt7 == 2) ~ 1,
        
        # Intended
        d_chslw == 1 & lag_pregnant == 1 & lag2_frt3 == 2 & lag2_frt5 > lag2_nkids & (lag2_frt7 == 1 | lag2_frt7 == 2) ~ 2,
        
        # Sooner-than-intended
        d_chslw == 1 & lag_pregnant == 1 & lag2_frt3 == 2 & lag2_frt5 > lag2_nkids & (lag2_frt7 == 3 | lag2_frt7 == 4 | lag2_frt7 == 7) ~ 3,
        
        # Unintended
        d_chslw == 1 & lag_pregnant == 1 & lag2_frt3 == 2 & lag2_frt5 <= lag2_nkids ~ 4,
        
        # Default case (no change in parenthood status)
        d_chslw == 1 & lag_pregnant == 1 ~ NA_integer_,
        
        # For other cases (not related to parenthood status), you can keep the "no change" as it is
        TRUE ~ NA_integer_
      ),
      levels = c(NA, 1, 2, 3, 4),
      labels = c("planned", "intended", "sooner-than-intended", "unintended")
    )
  ) %>%
  ungroup()


parenthood_total <- data_temp %>%
  group_by(parenthood_status) %>%
  summarise(total = n())

print(parenthood_total)

### Load "Biochild" data into R environment
biochild_data <- read_dta(file.path(data_raw_dir, "biochild.dta"))

# Load the dplyr package
library(dplyr)

# Check the column names in biochild_data
colnames(biochild_data)

# Select relevant columns from biochild_data using base R
subset_biochild <- biochild_data[, c("id", "number", "dobk")]

# Keep only the first observation for each unique combination of id and childno
distinct_subset_biochild <- subset_biochild %>% 
  distinct(id, number, .keep_all = TRUE)

# Filter out rows where dobk is negative
distinct_subset_biochild <- distinct_subset_biochild %>%
  filter(dobk >= 0)

# Perform the left join
merged_data <- left_join(
  data_temp,
  distinct_subset_biochild,
  by = c("childno" = "number", "id")
)

# Save data_temp
saveRDS(data_temp, file.path(data_temp_dir, "02_pairfam1-14.rds"))

# Now create one dataset with only ID, wave, parenthood status and dobk
# Filter to include only complete observations
filtered_data_temp <- merged_data %>%
  filter(!is.na(id) & !is.na(wave) & !is.na(parenthood_status) & !is.na(dobk))

# Select only the specified variables
selected_data_temp <- filtered_data_temp[c("id", "wave", "parenthood_status", "dobk")]

# Save cleaned data
saveRDS(selected_data_temp, file.path(data_temp_dir, "03_pairfam1-14_phstatusdobk.Rds"))

# Close the log file
sink()
