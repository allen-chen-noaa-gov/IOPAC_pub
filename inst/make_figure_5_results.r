library(IOPAC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(rsq)
library(here)

runiters <- FALSE

if(runiters == TRUE) {

library(foreach)
library(doParallel)
num_cores <- 6
cl <- makeCluster(num_cores)
registerDoParallel(cl)

iters <- paste0("y",2018:2023)

savebounds <- foreach(yearpull = iters, .combine = 'c',
  .packages = c("IOPAC")) %dopar% {

  drawsin <- 500
  seedin <- 42

  # All calculations for a single year remain inside the parallel loop
  costin <- clean_cost_data(
    sums = costf_V_list[[yearpull]],
    counts = costf_V_count_list[[yearpull]],
    confdel = costf_V_countv_list[[yearpull]],
    sd = costf_V_sd_list[[yearpull]],
    functype = "vessel",
    costtype = "means"
  )

  multbounds.normal <- make_mult_bounds(
    datain = costin,
    ticsin = tics_list[[yearpull]],
    markupsin = markups_list[[yearpull]],
    draws = drawsin,
    seed = seedin,
    drawtype = "normal",
    rawiters = TRUE,
    covarsin = NULL,
    countsin = costf_V_count_list[[yearpull]]
  )

  multbounds.lognormal <- make_mult_bounds(
    datain = costin,
    ticsin = tics_list[[yearpull]],
    markupsin = markups_list[[yearpull]],
    draws = drawsin,
    seed = seedin,
    drawtype = "lognormal",
    rawiters = TRUE,
    covarsin = NULL,
    countsin = costf_V_count_list[[yearpull]]
  )

  multbounds.multivariate <- make_mult_bounds(
    datain = costin,
    ticsin = tics_list[[yearpull]],
    markupsin = markups_list[[yearpull]],
    draws = drawsin,
    seed = seedin,
    drawtype = "multivariate",
    rawiters = TRUE,
    covarsin = costf_V_covars_list[[yearpull]],
    countsin = costf_V_count_list[[yearpull]]
  )

  multbounds.multivariate.lognormal <- make_mult_bounds(
    datain = costin,
    ticsin = tics_list[[yearpull]],
    markupsin = markups_list[[yearpull]],
    draws = drawsin,
    seed = seedin,
    drawtype = "multivariate.lognormal",
    rawiters = TRUE,
    covarsin = costf_V_covars_log_list[[yearpull]],
    countsin = costf_V_count_list[[yearpull]]
  ) 

  # Prepare the list of results for the current year
  year_result <- list(
    normal = multbounds.normal,
    lognormal = multbounds.lognormal,
    multivariate = multbounds.multivariate,
    multivariate.lognormal = multbounds.multivariate.lognormal
  )

  # This returns a list like list("y2018" = year_result)
  # When combined with .combine = 'c', this builds the final named list.
  return(setNames(list(year_result), yearpull))
}

output_figure5_raw_18 <- savebounds[c("y2018")]
output_figure5_raw_19 <- savebounds[c("y2019")]
output_figure5_raw_20 <- savebounds[c("y2020")]
output_figure5_raw_21 <- savebounds[c("y2021")]
output_figure5_raw_22 <- savebounds[c("y2022")]
output_figure5_raw_23 <- savebounds[c("y2023")]

save(output_figure5_raw_18, file = here("data",
  "output_figure5_raw_18.rda"))
save(output_figure5_raw_19, file = here("data",
  "output_figure5_raw_19.rda"))
save(output_figure5_raw_20, file = here("data",
  "output_figure5_raw_20.rda"))
save(output_figure5_raw_21, file = here("data",
  "output_figure5_raw_21.rda"))
save(output_figure5_raw_22, file = here("data",
  "output_figure5_raw_22.rda"))
save(output_figure5_raw_23, file = here("data",
  "output_figure5_raw_23.rda"))

# 4. Shut down the cluster to free up the cores
stopCluster(cl)
stopImplicitCluster()

} else {

load(here("data", "output_figure5_raw_18.rda"))
load(here("data", "output_figure5_raw_19.rda"))
load(here("data", "output_figure5_raw_20.rda"))
load(here("data", "output_figure5_raw_21.rda"))
load(here("data", "output_figure5_raw_22.rda"))
load(here("data", "output_figure5_raw_23.rda"))

output_figure5_raw <- c(output_figure5_raw_18, output_figure5_raw_19,
  output_figure5_raw_20, output_figure5_raw_21,
  output_figure5_raw_22, output_figure5_raw_23)

p025 <- list()
p975 <- list()
medians <- list()
mediansout <- list()
p025out <- list()
p975out <- list()
rawdat <- list()
rawout <- list()
forreg <- list()
forregout <- list()

desired_columns <- c("Region", "Name", "Vessel_output")

for (year in names(output_figure5_raw)) {
  for (method in names(output_figure5_raw[[year]])) {
    # for(i in 1:length(output_figure5_raw[[year]][[method]])) {
      p025[[year]][[method]] <- quantile(unlist(lapply(
        output_figure5_raw[[year]][[method]], `[[`, "Vessel_output")),
          0.025, na.rm = TRUE)
      p975[[year]][[method]] <- quantile(unlist(lapply(
        output_figure5_raw[[year]][[method]], `[[`, "Vessel_output")),
          0.975, na.rm = TRUE)
      medians[[year]][[method]] <- median(unlist(lapply(
        output_figure5_raw[[year]][[method]], `[[`, "Vessel_output")),
          na.rm = TRUE)

      rawdat[[year]][[method]] <- unlist(lapply(
        output_figure5_raw[[year]][[method]], `[[`, "Vessel_output"))

      forreg[[year]][[method]] <- do.call(rbind, lapply(output_figure5_raw[[year]][[method]],
        function(x) {
          x[, desired_columns, drop = FALSE]
          # Use drop = FALSE to ensure the result remains a data frame even
          # with one column selected
        }
      ))
  }
  mediansout[[year]] <- stack(medians[[year]])
  p025out[[year]] <- stack(p025[[year]])
  p975out[[year]] <- stack(p975[[year]])
  rawout[[year]] <- stack(rawdat[[year]])
  forregout[[year]] <- bind_rows(forreg[[year]], .id = "Distribution")

}

table_data <- rbind(data.frame(bind_rows(mediansout, .id = "year"),
    type = "p050"),
  data.frame(bind_rows(p025out, .id = "year"), type = "p025"),
  data.frame(bind_rows(p975out, .id = "year"), type = "p975")) %>% 
  pivot_wider(
    names_from = type,
    values_from = values
  ) %>%
  mutate(
    # Format as Median [Q1, Q3]
    `Median (IQR)` = paste0(round(p050, 2), " [", round(p025, 2), ", ",
      round(p975, 2), "]")
  ) %>%
  select(year, ind, `Median (IQR)`)

# Clean up the year column by removing the 'y' prefix
table_data$year <- sub("^y", "", table_data$year)

# Use pivot_wider to reshape the table_data
wide_table <- table_data %>%
  pivot_wider(
    names_from = ind,     # Column to take names from (the different model types)
    values_from = `Median (IQR)` # Column to take values from (the median and IQR)
  )

names(wide_table)[names(wide_table) == "year"] <- "Year"
names(wide_table)[names(wide_table) == "normal"] <- "Normal"
names(wide_table)[names(wide_table) == "lognormal"] <- "Lognormal"
names(wide_table)[names(wide_table) == "multivariate"] <- "Multivariate"
names(wide_table)[names(wide_table) == "multivariate.lognormal"] <-
  "Multivariate Lognormal"

# Print the resulting wide table
# Create table as PNG
table_grob <- tableGrob(wide_table, rows = NULL, theme = ttheme_default(
  core = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 12)),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 10, fontface = "bold")),
  padding = unit(c(5, 5), "mm")
))

p <- ggplot() +
  annotation_custom(table_grob) +
  theme_void()

ggsave(paste0(here(), "\\inst\\", "table_1_multbounds.png"), 
  plot = p, width = 8, height = 2.5, dpi = 300, bg = "white")

# View the result
forplot <- bind_rows(rawout, .id = "Year")
forplot$Year <- sub("^y", "", forplot$Year)
forplot <- forplot %>%
  mutate(
    ind = case_when(
      ind == "normal" ~ "Normal",
      ind == "lognormal" ~ "Lognormal",
      ind == "multivariate" ~ "Multivariate",
      ind == "multivariate.lognormal" ~ "Multivariate Lognormal",
      TRUE ~ ind # Keep all other values as they are
    )
  )

p <- ggplot(data = forplot, aes(x = ind, y = values, fill = ind)) + 
  geom_violin(trim = TRUE, na.rm = TRUE) + # trim=FALSE extends the violin to the full data range
  labs(title = "Vessel Output Multipliers by Distribution, Faceted by Year", 
       x = "Distribution", 
       y = "Vessel Output Multiplier") + 
  facet_wrap(~ Year) + # Facet by the 'year' variable
  theme_bw() + # Use a clean white background theme
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(paste0(here(), "\\inst\\", "figure_5_multbounds.png"), 
  plot = p, width = 8, height = 6, dpi = 300, bg = "white")

regdata <- bind_rows(forregout, .id = "Year")

Region <- list()
Year <- list()
Name <- list()
for (dist in unique(regdata$Distribution)) {

  subregdata <- regdata %>% filter(Distribution == dist) %>%
    na.omit() %>%
    droplevels()

  # Fit full and reduced models
  full_model <- glm(Vessel_output ~ as.factor(Name) + as.factor(Year) + as.factor(Region), family = gaussian,
    data = subregdata)

  reduced_model <- glm(Vessel_output ~ as.factor(Name) + as.factor(Year),
    family = gaussian,
    data = subregdata)
  # Calculate partial R-squared
  Region[[dist]] <- rsq.partial(full_model, reduced_model)$partial.rsq

  reduced_model <- glm(Vessel_output ~ as.factor(Name) + as.factor(Region),
    family = gaussian,
    data = subregdata)
  # Calculate partial R-squared
  Year[[dist]] <- rsq.partial(full_model, reduced_model)$partial.rsq

    reduced_model <- glm(Vessel_output ~ as.factor(Year) + as.factor(Region),
    family = gaussian,
    data = subregdata)
  # Calculate partial R-squared
  Name[[dist]] <- rsq.partial(full_model, reduced_model)$partial.rsq

}

r2table <- rbind(data.frame(Factor = "Fleet", bind_rows(Name,
  .id = "Distribution")),
      data.frame(Factor = "Year", bind_rows(Year, .id = "Distribution")),
      data.frame(Factor = "Region", bind_rows(Region, .id = "Distribution"))) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

names(r2table)[names(r2table) == "normal"] <- "Normal"
names(r2table)[names(r2table) == "lognormal"] <- "Lognormal"
names(r2table)[names(r2table) == "multivariate"] <- "Multivariate"
names(r2table)[names(r2table) == "multivariate.lognormal"] <-
  "Multivariate Lognormal"

# Print the resulting wide table
# Create table as PNG
table_grob <- tableGrob(r2table, rows = NULL, theme = ttheme_default(
  core = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 12)),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 10, fontface = "bold")),
  padding = unit(c(12, 12), "mm")
))

p <- ggplot() +
  annotation_custom(table_grob) +
  theme_void()

ggsave(paste0(here(), "\\inst\\", "table_2_partialr2.png"), 
  plot = p, width = 7, height = 3, dpi = 300, bg = "white")

}
