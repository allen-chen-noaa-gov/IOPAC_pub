clean_cost_data <- function(sums = costf_V_list[["y2023"]],
  counts = costf_V_count_list[["y2023"]],
  confdel = costf_V_countv_list[["y2023"]],
  sd = costf_V_sd_list[["y2023"]]) {

sumsd <- sums[, -c(1)] %>%
  mutate_all(function(x) as.numeric(as.character(x)))
countsd <- counts[, -c(1)] %>%
  mutate_all(function(x) as.numeric(as.character(x)))
confdeld <- confdel[, -c(1)] %>%
  mutate_all(function(x) as.numeric(as.character(x)))
sdd <- sd[, -c(1)] %>%
  mutate_all(function(x) as.numeric(as.character(x)))
# divide sums by counts:

sumsconf <- sumsd*confdeld
countsconf <- countsd*confdeld
sdconf <- sdd*confdeld

means <- sumsconf / countsconf
means[is.na(means)] <- 0
means$Cost <- sums[[1]]
means <- means %>%
  relocate(Cost)

sdconf$Cost <- sums[[1]]
sdout <- sdconf %>%
  relocate(Cost)

return(list(means = means, sd = sdout))

}   
