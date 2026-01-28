library(ggplot2)

# Create feature lists
features <- c(
  "Captain",
  "Crew",
  "Fuel, lubricants",
  "Food",
  "Ice",
  "Bait",
  "Repair and maintenance",
  "Insurance",
  "Interest and financial services",
  "Purchase of permits",
  "Leasing of permits",
  "Moorage",
  "Landings taxes",
  "Enforcement",
  "Dues",
  "Freight supplies",
  "Offloading",
  "Trucking",
  "Other miscellaneous",
  "Proprietary income"
)

descriptions <- c(
  "Employee payroll",
  "Additives",
  "Custom processing",
  "Electricity",
  "Freight",
  "Insurance",
  "Natural gas",
  "Offsite storage and freezing",
  "Packaging",
  "Production supplies",
  "Propane",
  "Rental or lease of buildings",
  "Rental or lease of equipment",
  "Repair and maintenance of buildings and equipment",
  "Sewer and waste",
  "Shoreside monitor",
  "Water",
  "Fish purchases",
  "Other",
  "Proprietary income"
)

# Create a data frame for plotting
y_pos <- seq(length(features), 1, by = -1)
plot_data <- data.frame(
  y = rep(y_pos, 2),
  x = c(rep(0.05, length(features)), rep(0.55, length(features))),
  label = c(features, descriptions),
  type = c(rep("Feature", length(features)), rep("Description", length(features)))
)

# Create the plot
p <- ggplot(plot_data, aes(x = x, y = y, label = label)) +
  geom_text(hjust = 0, vjust = 0.7, size = 4.5, family = "sans", lineheight = 1.1) +
  # Add headings
  annotate("text", x = 0.05, y = length(features) + 0.8, label = "Vessel Expenditures", 
           size = 6, hjust = 0, fontface = "bold", family = "sans") +
  annotate("text", x = 0.55, y = length(features) + 0.8, label = "Processor Expenditures", 
           size = 6, hjust = 0, fontface = "bold", family = "sans") +
  # Add horizontal lines under headings
  geom_segment(aes(x = 0.05, xend = 0.5, y = length(features) + 0.3, yend = length(features) + 0.3),
               color = "black", size = 0.5, inherit.aes = FALSE) +
  geom_segment(aes(x = 0.55, xend = 0.95, y = length(features) + 0.3, yend = length(features) + 0.3),
               color = "black", size = 0.5, inherit.aes = FALSE) +
  xlim(0, 1) +
  ylim(0, length(features) + 1) +
  theme_void()

# Save as PNG
out_file <- here::here("inst", "figure_2_IOPAC_categories.png")
ggsave(filename = out_file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved figure: ", out_file)
