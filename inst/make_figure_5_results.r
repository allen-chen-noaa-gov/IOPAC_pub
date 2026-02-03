library(IOPAC)
library(ggplot2)

costflist_2023 <- costflist_template

costflist_2023$vessel <- clean_cost_data(functype = "vessel")

costflist_2023$processor <- clean_cost_data(sums = costf_P_list[["y2023"]],
  functype = "processor")
  multbounds.normal <- make_mult_bounds()

multbounds.lognormal <- make_mult_bounds(drawtype = "lognormal")

multbounds.multivariate <- make_mult_bounds(drawtype = "multivariate",
  covars = costf_V_covars_list$y2023)

multbounds.multivariate.lognormal <- make_mult_bounds(
  drawtype = "multivariate.lognormal",
  covars = costf_V_covars_log_list$y2023)