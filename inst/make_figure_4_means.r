library(IOPAC)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)

# Get vessel cost data
vessel_costs <- costf_V_list$y2023
vessel_sds <- costf_V_sd_list$y2023
vessel_counts <- costf_V_count_list$y2023

# Replace each column in vessel_counts with its max value
for (col in names(vessel_counts)) {
  if (col != names(vessel_counts)[1]) {  # skip first column if it's the cost names
    vessel_counts[[col]] <- max(vessel_counts[[col]], na.rm = TRUE)
  }
}

# First column contains the cost names
cost_names <- vessel_costs[[1]]

# Convert means to long format
means_data <- vessel_costs %>%
  mutate(Cost = cost_names) %>%
  pivot_longer(cols = -c(1, Cost), names_to = "Vessel_Type", values_to = "Mean") %>%
  select(Cost, Vessel_Type, Mean)

# Convert SDs to long format
sds_data <- vessel_sds %>%
  mutate(Cost = cost_names) %>%
  pivot_longer(cols = -c(1, Cost), names_to = "Vessel_Type",
    values_to = "SD") %>%
  select(Cost, Vessel_Type, SD)

# Convert counts to long format
counts_data <- vessel_counts %>%
  mutate(Cost = cost_names) %>%
  pivot_longer(cols = -c(1, Cost), names_to = "Vessel_Type", values_to = "Count") %>%
  select(Cost, Vessel_Type, Count)

# Create mappings for custom names
# EDIT THESE VECTORS TO RENAME COSTS (facets) and VESSEL TYPES (x-axis labels)
cost_rename <- c(
  "COST_BAIT" = "Bait",
  "COST_CAPTAIN" = "Captain wages",
  "COST_COMM" = "Communications",
  "COST_ICE" = "Ice",
  "COST_FOOD" = "Food",
  "COST_CREW" = "Crew wages",
  "COST_FREIGHT" = "Freight",
  "COST_FUEL" = "Fuel",
  "COST_INS" = "Insurance",
  "COST_MOORAGE" = "Moorage",
  "COST_LANDINGSTAX" = "Landings tax",
  "COST_ENFORCE" = "Enforcement",
  "COST_OFFLOAD" = "Offloading",
  "COST_OTHER" = "Other",
  "COST_TRAV" = "Travel",
  "COST_TRUCK" = "Trucking",
  "CREW" = "Number of crew",
  "REV" = "Revenue",
  "COST_RMI" = "Repairs and maintenance",
  "COST_DUES" = "Dues"
  # Add more mappings as needed
)

vessel_rename <- c(
  "Small.goundfish.trawler" = "Small groundfish trawler",
  "Other..more.than.15K" = "Other more than 15k",
  "Other..less.than.15K" = "Other less than 15k"
)

# Function to replace periods with spaces
replace_periods <- function(x) {
  gsub("\\.", " ", x)
}

# Combine all data
plot_data <- means_data %>%
  left_join(sds_data, by = c("Cost", "Vessel_Type")) %>%
  left_join(counts_data, by = c("Cost", "Vessel_Type")) %>%
  filter(!is.na(Mean), Mean != 0) %>%
  filter(Cost != "COST_LEA") %>%
  mutate(
    Mean = Mean / Count,
    SD = SD,
    Cost = recode(Cost, !!!cost_rename),
    Vessel_Type_Label = paste0(replace_periods(recode(Vessel_Type, !!!vessel_rename)), " (n = ", Count, ")")
  )

# Create plot with error bars
p <- ggplot(plot_data, aes(x = Vessel_Type_Label, y = Mean, fill = Vessel_Type)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, alpha = 0.8) +
  facet_wrap(~Cost, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none",
    strip.text = element_text(size = 10),
    plot.margin = margin(b = 25, t = 25, l = 25, r = 25, unit = "pt")
  ) +
  labs(
    title = "Mean Vessel Costs by Type (2023) with Standard Deviations",
    x = "Vessel Type",
    y = "Cost"
  )

# Save as PNG
out_file <- here::here("inst", "figure_4_means.png")
ggsave(filename = out_file, plot = p, width = 14, height = 10, dpi = 300, bg = "white")
message("Saved boxplot: ", out_file)

