# ==============================================================================
# Date: January 2024
# Paper: Unplanned parenthood and employment complexity
# Author: Gädecke, Martin
# Script: 04_sequence_visualisation
# Input:  01_bioactdemog1-3; 01_seq1-3
# Output: figures, tables
# ==============================================================================


install.packages("viridis")
library(viridis)

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

## Read in data

# Load and assign bioact_seq_list and seq_list for each i
for (i in 1:3) {
  assign(paste0("demog_seq_", i), 
         readRDS(file.path(data_posted_dir, 
                           paste0("demog_child_", i, ".Rds"))), envir = .GlobalEnv)
  assign(paste0("complexity_seq_", i), 
         readRDS(file.path(data_posted_dir, 
                           paste0("final_complexity_child_", i, ".Rds"))), envir = .GlobalEnv)

  assign(paste0("seq_", i), 
         readRDS(file.path(data_posted_dir, 
                           paste0("01_seq_", i, ".Rds"))), envir = .GlobalEnv)
}


### Some preparations for visualisation
# Get Legend box
source(file.path(script_dir, file = "99_legendbox.R"))


# Combine data for all three children
combined_data <- bind_rows(
  mutate(demog_seq_1, childno = 1),
  mutate(demog_seq_2, childno = 2),
  mutate(demog_seq_3, childno = 3))

# Calculate percentages
combined_data %>%
  count(childno, parenthood_status) %>%       
  group_by(childno) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot() +
  aes(x = factor(childno), y = pct, fill = parenthood_status) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("[%]") +
  xlab("") +
  geom_text(aes(label = sprintf("%1.1f", pct)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4) +
  ggtitle("") +
  scale_x_discrete(labels = c("First child\n N = 558", "Second child\n N = 494", "Third child\n N = 159")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE, name = "Parenthood Status") +
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom", legend.box = "horizontal",
        legend.direction = "horizontal", legend.justification = "center",
        legend.spacing.x = unit(0.1, "cm"),
        plot.margin = margin(b = 0, l = 0.2, r = 0.2, t = 0.2, unit = "cm"))

# Save the plot as a PNG file
ggsave(file.path(graph_dir, paste0("fig1",".png")), 
       width = 8, height = 6, units = "in",
       plot = last_plot(), dpi = 600)

### Figure 2 - Average age at parenthood boxplots
combined_data %>%
  ggplot(aes(x = fct_rev(factor(childno)), y = age_at_parenthood, fill = factor(parenthood_status))) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers
  ylab("Age at Parenthood") +
  xlab("") +
  ggtitle("") +
  scale_x_discrete(labels = c("Third child", "Second child", "First child")) +
  scale_y_continuous(limits = c(20, 45), breaks = seq(20, 45, by = 5)) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE, name = "Parenthood Status") +
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom", legend.box = "horizontal",
        legend.direction = "horizontal", legend.justification = "center",
        legend.spacing.x = unit(0.1, "cm"),
        plot.margin = margin(b = 0, l = 0.2, r = 0.2, t = 0.2, unit = "cm")) +
  coord_flip()

# Save the plot as a PNG file
ggsave(file.path(graph_dir, paste0("fig2",".png")), 
         width = 8, height = 6, units = "in",
         plot = last_plot(), dpi = 600)  


### Figure 3
# Convert parenthood_status to a factor for better plotting
complexity_seq_1$parenthood_status <- factor(complexity_seq_1$parenthood_status)

# Calculate group averages and confidence intervals
summary_stats <- complexity_seq_1 %>%
  group_by(time, parenthood_status) %>%
  summarise(
    mean_complexity = mean(C),
    ci_lower = mean_complexity - 1.96 * sd(C) / sqrt(n()),
    ci_upper = mean_complexity + 1.96 * sd(C) / sqrt(n()))

# Modify the color scheme
color_scheme <- scales::viridis_pal(option = "D")(4)

# Plot the group averages and confidence intervals
ggplot(summary_stats, aes(x = time, y = mean_complexity, color = parenthood_status, group = parenthood_status)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), linetype = "dashed", alpha = 0, fill = NA) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid") +  # Add red line at t = 0
  scale_color_manual(values = color_scheme) +
  labs(title = "",
       x = "Months Since Parenthood",  # Adjust x-axis title
       y = "Mean Complexity") +
  scale_x_continuous(breaks = seq(-12, 60, by = 6)) +  # Adjust x-axis labels
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom", legend.box = "horizontal",
        legend.direction = "horizontal", legend.justification = "center",
        legend.spacing.x = unit(0.1, "cm"))

# Save the plot as a PNG file
ggsave(file.path(graph_dir, paste0("fig3",".png")), 
       width = 8, height = 6, units = "in",
       plot = last_plot(), dpi = 600)  


### Figure 4

demog_seq_1 <- demog_seq_1 %>%
  mutate(sex_gen = case_when(
    sex_gen == 1 ~ "Men",
    sex_gen == 2 ~ "Women",
    TRUE ~ as.character(sex_gen)  # Keep other values as they are
  ))

png(file.path(graph_dir, "fig4.png"), width = 8, height = 6, units = "in",
    res=600)

# Create the sequence plot
seqdplot(seq_1, group = demog_seq_1$sex_gen, border = NA)

# Close the PNG device
dev.off()