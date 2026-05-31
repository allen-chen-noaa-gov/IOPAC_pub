library(ggplot2)
library(here)
library(dplyr)
library(gridExtra)
library(grid)

load(here("data", "output_figure5_raw_21.rda"))
inmat <- output_figure5_raw_21$y2021$multivariate.lognormal

poltab <- read.csv(paste0(here(), "\\inst\\extdata\\", "policyex.csv"))
poltab <- poltab[poltab$Year < 2027, ]

incmult <- list()
empmult <- list()
incout <- list()
empout <- list()
incmultout <- list()
empmultout <- list()
for (i in 1:length(inmat)) {

incmult[[i]] <- inmat[[i]]$TotInc[
  inmat[[i]]$Name == "Other Groundfish, Fixed Gear" &
  inmat[[i]]$Region == "WC"]

empmult[[i]] <- inmat[[i]]$TotEmp[
  inmat[[i]]$Name == "Other Groundfish, Fixed Gear" &
  inmat[[i]]$Region == "WC"]

#lingcod/rockfish revenue
allrev <- 20581180

incout[[i]] <- allrev*incmult[[i]] 
empout[[i]] <- allrev*empmult[[i]] 

incmultout[[i]] <- incmult[[i]] 
empmultout[[i]] <- empmult[[i]] 

}

# 20581180 rev per 21 MT rockfish see page B-43 of Appendix B
# Consideration of Changes to the Yelloweye Rockfish Rebuilding Plan
# Pacific Coast Groundfish Fishery 2019–20 Harvest Specifications,
# Yelloweye Rebuilding Plan Revisions, and Management Measures

# income per-iteration
inc_noaction_mat <- do.call(rbind, lapply(incout, function(x) (x/1000000/21) * poltab$NoAction))
inc_alt1_mat    <- do.call(rbind, lapply(incout, function(x) (x/1000000/21) * poltab$Alt1))
inc_alt2_mat    <- do.call(rbind, lapply(incout, function(x) (x/1000000/21) * poltab$Alt2))

# employment per-iteration
emp_noaction_mat <- do.call(rbind, lapply(empout, function(x) (x/21) * poltab$NoAction))
emp_alt1_mat     <- do.call(rbind, lapply(empout, function(x) (x/21) * poltab$Alt1))
emp_alt2_mat     <- do.call(rbind, lapply(empout, function(x) (x/21) * poltab$Alt2))

col_median <- function(mat) apply(mat, 2, median)
col_q025   <- function(mat) apply(mat, 2, quantile, probs = 0.025)
col_q975   <- function(mat) apply(mat, 2, quantile, probs = 0.975)

# differences per iteration
inc_diff_alt2_vs_alt1 <- inc_alt2_mat - inc_alt1_mat
inc_diff_alt1_vs_noaction <- inc_alt1_mat - inc_noaction_mat
inc_diff_alt2_vs_noaction <- inc_alt2_mat - inc_noaction_mat

emp_diff_alt2_vs_alt1 <- emp_alt2_mat - emp_alt1_mat
emp_diff_alt1_vs_noaction <- emp_alt1_mat - emp_noaction_mat
emp_diff_alt2_vs_noaction <- emp_alt2_mat - emp_noaction_mat

inc_alt2_vs_alt1_df <- data.frame(
  Year = poltab$Year,
  Diff = col_median(inc_diff_alt2_vs_alt1),
  Diff025 = col_q025(inc_diff_alt2_vs_alt1),
  Diff975 = col_q975(inc_diff_alt2_vs_alt1),
  Comparison = "Alt 2 > Alt 1"
)

inc_alt1_vs_noaction_df <- data.frame(
  Year = poltab$Year,
  Diff = col_median(inc_diff_alt1_vs_noaction),
  Diff025 = col_q025(inc_diff_alt1_vs_noaction),
  Diff975 = col_q975(inc_diff_alt1_vs_noaction),
  Comparison = "Alt 1 > No Action"
)

inc_alt2_vs_noaction_df <- data.frame(
  Year = poltab$Year,
  Diff = col_median(inc_diff_alt2_vs_noaction),
  Diff025 = col_q025(inc_diff_alt2_vs_noaction),
  Diff975 = col_q975(inc_diff_alt2_vs_noaction),
  Comparison = "Alt 2 > No Action"
)

# Employment
emp_alt2_vs_alt1_df <- data.frame(
  Year = poltab$Year,
  Diff = col_median(emp_diff_alt2_vs_alt1),
  Diff025 = col_q025(emp_diff_alt2_vs_alt1),
  Diff975 = col_q975(emp_diff_alt2_vs_alt1),
  Comparison = "Alt 2 > Alt 1"
)

emp_alt1_vs_noaction_df <- data.frame(
  Year = poltab$Year,
  Diff = col_median(emp_diff_alt1_vs_noaction),
  Diff025 = col_q025(emp_diff_alt1_vs_noaction),
  Diff975 = col_q975(emp_diff_alt1_vs_noaction),
  Comparison = "Alt 1 > No Action"
)

emp_alt2_vs_noaction_df <- data.frame(
  Year = poltab$Year,
  Diff = col_median(emp_diff_alt2_vs_noaction),
  Diff025 = col_q025(emp_diff_alt2_vs_noaction),
  Diff975 = col_q975(emp_diff_alt2_vs_noaction),
  Comparison = "Alt 2 > No Action"
)

inc_diff_all <- rbind(inc_alt2_vs_alt1_df, inc_alt1_vs_noaction_df, inc_alt2_vs_noaction_df)
emp_diff_all <- rbind(emp_alt2_vs_alt1_df, emp_alt1_vs_noaction_df, emp_alt2_vs_noaction_df)

# Add Metric column to differences dataframes
inc_diff_all$Metric <- "Dollars, millions"
emp_diff_all$Metric <- "Employment, jobs"

# Combine income and employment differences
diff_combined <- rbind(inc_diff_all, emp_diff_all)

# Plot combined differences
p_diff_combined <- ggplot(diff_combined, aes(x = Year, y = Diff, colour = Comparison, shape = Comparison)) +
  geom_ribbon(aes(ymin = Diff025, ymax = Diff975, fill = Comparison, colour = Comparison), width = 0.5,
    position = position_dodge(0.1), alpha = 0.2) +
  geom_line(position = position_dodge(0.1), size = 1) +
  geom_point(position = position_dodge(0.1), size = 3, fill = "white") +
  scale_shape_manual(values = c("Alt 2 > Alt 1" = 21, "Alt 1 > No Action" = 22, "Alt 2 > No Action" = 23)) +
  scale_colour_manual(values = c("Alt 2 > Alt 1" = "#F8766D", "Alt 1 > No Action" = "#7CAE00", "Alt 2 > No Action" = "#00BFC4")) +
  scale_fill_manual(values = c("Alt 2 > Alt 1" = "#F8766D", "Alt 1 > No Action" = "#7CAE00", "Alt 2 > No Action" = "#00BFC4")) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "Year",
       y = "Difference (Median with 95% CI)",
       colour = "Comparison",
       shape = "Comparison") +
  theme_minimal()

ggsave(paste0(here(), "\\inst\\", "figure_8_multi_differences.png"),
  plot = p_diff_combined,
  width = 20, height = 9, units = "cm", dpi = 300)

incrat <- (median(unlist(incout))/1000000)/21
emprat <- (median(unlist(empout)))/21

inc95 <- ((quantile(unlist(incout), c(.975)))/1000000)/21
inc05 <- ((quantile(unlist(incout), c(.025)))/1000000)/21

emp95 <- ((quantile(unlist(empout), c(.975))))/21
emp05 <- ((quantile(unlist(empout), c(.025))))/21

incnoaction <- data.frame(Year = poltab$Year, Income = incrat*poltab$NoAction,
  Income025 = inc05*poltab$NoAction, Income975 = inc95*poltab$NoAction, 
  Policy = "No Action")

incalt1 <- data.frame(Year = poltab$Year, Income = incrat*poltab$Alt1,
  Income025 = inc05*poltab$Alt1, Income975 = inc95*poltab$Alt1, 
  Policy = "Alt 1")

incalt2 <- data.frame(Year = poltab$Year, Income = incrat*poltab$Alt2,
  Income025 = inc05*poltab$Alt2, Income975 = inc95*poltab$Alt2, 
  Policy = "Alt 2")

# Income data with Metric column
plotinc <- rbind(incnoaction, incalt1, incalt2)
plotinc$Metric <- "Dollars, millions"
plotinc$Value <- plotinc$Income
plotinc$Value025 <- plotinc$Income025
plotinc$Value975 <- plotinc$Income975

empnoaction <- data.frame(Year = poltab$Year, Emp = emprat*poltab$NoAction,
  Emp025 = emp05*poltab$NoAction, Emp975 = emp95*poltab$NoAction, 
  Policy = "No Action")

empalt1 <- data.frame(Year = poltab$Year, Emp = emprat*poltab$Alt1,
  Emp025 = emp05*poltab$Alt1, Emp975 = emp95*poltab$Alt1, 
  Policy = "Alt 1")

empalt2 <- data.frame(Year = poltab$Year, Emp = emprat*poltab$Alt2,
  Emp025 = emp05*poltab$Alt2, Emp975 = emp95*poltab$Alt2, 
  Policy = "Alt 2")

# Employment data with Metric column
plotemp <- rbind(empnoaction, empalt1, empalt2)
plotemp$Metric <- "Employment, jobs"
plotemp$Value <- plotemp$Emp
plotemp$Value025 <- plotemp$Emp025
plotemp$Value975 <- plotemp$Emp975

# Combine income and employment data
plot_combined <- rbind(
  plotinc[, c("Year", "Policy", "Metric", "Value", "Value025", "Value975")],
  plotemp[, c("Year", "Policy", "Metric", "Value", "Value025", "Value975")]
)

pd <- position_dodge(0.1)

p <- ggplot(plot_combined, aes(x=Year, y=Value, colour=Policy, shape=Policy)) + 
    geom_ribbon(aes(ymin=Value025, ymax=Value975, fill=Policy, colour=Policy), alpha=0.2) +
    geom_line(position=pd, size = 1) +
    geom_point(position=pd, size=3, fill="white") +
    scale_shape_manual(values =
      c("No Action" = 21, "Alt 1" = 22, "Alt 2" = 23)) +
    scale_colour_manual(values = c("No Action" = "#F8766D", "Alt 1" = "#7CAE00", "Alt 2" = "#00BFC4")) +
    scale_fill_manual(values = c("No Action" = "#F8766D", "Alt 1" = "#7CAE00", "Alt 2" = "#00BFC4")) +
    facet_wrap(~Metric, scales = "free_y") +
    theme_minimal()

ggsave(paste0(here(), "\\inst\\", "figure_9_multi_lines.png"), plot = p,
  width = 20, height = 9, units = "cm")
