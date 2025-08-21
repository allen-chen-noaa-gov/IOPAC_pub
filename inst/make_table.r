library(IOPAC)
library(here)
library(dplyr)
library(knitr)
library(kableExtra)

# Combine means and standard deviations into a data frame, only modifying numeric columns
means <- as.data.frame(costf_V_list$y2023)
sds   <- as.data.frame(costf_V_sd_list$y2023)
counts <- as.data.frame(costf_V_count_list$y2023)

replacecounts <- counts[counts$V1 == "REV", !(names(counts) %in% "V1")]
replacecounts[is.na(replacecounts)] <- 0
counts[, !(names(counts) %in% "V1")] <- replacecounts %>% slice(rep(1:n(),
  length.out = nrow(counts)))
  
# Divide means by counts, except for the first column which is not numeric
means_div <- means

for (col in names(means)[-1]) { # skip the first column
  if (is.numeric(means[[col]]) && is.numeric(counts[[col]])) {
    means_div[[col]] <- means[[col]] / counts[[col]]
  }
}

# If you want to keep the first column as is:
means_div[[names(means)[1]]] <- means[[names(means)[1]]]

combined <- means_div

for (col in names(means_div)) {
  if (is.numeric(means_div[[col]])) {
    combined[[col]] <- ifelse(
      !is.na(means_div[[col]]) & !is.na(sds[[col]]),
      sprintf("%s<br>(%s)",
        formatC(means_div[[col]], format = "f", big.mark = ",", digits = 2),
        formatC(sds[[col]], format = "f", big.mark = ",", digits = 2)
      ),
      ifelse(!is.na(means_div[[col]]), as.character(means_div[[col]]), "")
    )
  }
}

# View the result
colnames(combined) <- colnames(costflist_template$vessel)[-20]
names(combined) <- gsub(pattern = "\\.", replacement = " ", x = names(combined))

combined$Cost <- costflist_template$vessel$Cost
combined$Cost[combined$Cost == "Proprietary income"] <- "Revenue"
combined$Cost[combined$Cost == "Output per Employee"] <- "Crew"

write.csv(combined, here("inst", "combined_costf_V_2023.csv"),
  row.names = TRUE)

# Make sure Cost is the first column for readability
combined <- combined[, c("Cost", setdiff(names(combined), "Cost"))]

# Add counts to column names (except "Cost"), assuming order matches
new_colnames <- names(combined)
for (i in seq_along(new_colnames)) {
  col <- new_colnames[i]
  if (col != "Cost") {
    # Find the corresponding column in counts by position (skip "Cost" in both)
    count_val <- counts[[i]][1]
    new_colnames[i] <- sprintf("%s<br>(n = %s)", col, formatC(count_val, format = "d", big.mark = ","))
  }
}
colnames(combined) <- new_colnames

combined <- combined[, c("Cost", "Large Groundfish Trawler<br>(n = 35)",
  "Pacific Whiting Trawler<br>(n = 8)", "Sablefish Fixed Gear<br>(n = 166)",
  "Shrimper<br>(n = 56)", "Crabber<br>(n = 361)")]

combined <- combined[!(combined$Cost %in% c("Interest",
  "Purchases of permits", "Leasing of permits")), ]

# Print a nice table for R Markdown/Quarto (HTML)
my_table <- kable(
    combined,
    align = "l",
    format = "html",
    escape = FALSE,
    row.names = FALSE
  ) %>%
  kable_styling(latex_options = "scale_down")

save_kable(my_table, file = here("inst", "combined_costf_V_2023.png"))
