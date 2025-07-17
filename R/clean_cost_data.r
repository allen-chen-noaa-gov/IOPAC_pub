clean_cost_data <- function(sums = costf_V_list[["y2023"]],
  counts = costf_V_count_list[["y2023"]],
  confdel = costf_V_countv_list[["y2023"]],
  sd = costf_V_sd_list[["y2023"]],
  functype = "vessel",
  costtype = "percs") {

  if (functype == "processor") {
    
    colnames(sums)[grepl("Xn", colnames(sums))] <- "Value"

    # Divide all rows by the row where Type == "Revenue", except for the row where Type == "Output per Employee"
    rev_row <- which(sums$Type == "Revenue")
    ope_row <- which(sums$Type == "Output per Employee")
    sums$ShareC <- sums$Value

    for (i in seq_len(nrow(sums))) {
      if (i != ope_row) {
        sums[i, "ShareC"] <- sums[i, "ShareC"] / sums[rev_row, "ShareC"]
      }
    }

    return(sums)

  } else if (functype == "vessel") {
  # vessel costs
  
  # Replace counts with counts from REV row
  replacecounts <- counts[counts$V1 == "REV", !(names(counts) %in% "V1")]
  replacecounts[is.na(replacecounts)] <- 0
  counts[, !(names(counts) %in% "V1")] <- replacecounts %>% slice(rep(1:n(),
    length.out = nrow(counts)))

  sumsd <- sums[, -c(1)] %>%
    mutate_all(function(x) as.numeric(as.character(x)))
  countsd <- counts[, -c(1)] %>%
    mutate_all(function(x) as.numeric(as.character(x)))
  confdeld <- confdel[, -c(1)] %>%
    mutate_all(function(x) as.numeric(as.character(x)))
  sdd <- sd[, -c(1)] %>%
    mutate_all(function(x) as.numeric(as.character(x)))

  # divide sums by counts
  sumsconf <- sumsd*confdeld
  countsconf <- countsd*confdeld
  sdconf <- sdd*confdeld

  means <- sumsconf / countsconf
  means[is.na(means)] <- 0
  # probably should call this
  means$Cost <- costflist_template$vessel$Cost
  means$Cost[means$Cost %in% c("Proprietary income",
    "Output per Employee")] <- c("REV", "CREW")
  means <- means %>%
    relocate(Cost)

  sdconf$Cost <- costflist_template$vessel$Cost
  sdconf$Cost[sdconf$Cost %in% c("Proprietary income",
    "Output per Employee")] <- c("REV", "CREW")
  sdout <- sdconf %>%
    relocate(Cost)

  if (costtype == "percs") {

    percs <- means
    # Divide all values except for first column by the values in row REV, except for rows REV and CREW
    rev_row <- which(percs$Cost == "REV")
    crew_row <- which(percs$Cost == "CREW")
    cols_to_divide <- setdiff(names(percs), "Cost")

    for (i in seq_len(nrow(percs))) {
      if (!(i %in% c(rev_row, crew_row))) {
        percs[i, cols_to_divide] <- percs[i, cols_to_divide] / percs[rev_row, cols_to_divide]
      }
    }

    percs[percs$Cost %in% c("CREW"),
      !(names(percs) %in% "Cost")] <- 
        percs[percs$Cost %in% c("REV"), !(names(percs) %in% "Cost")]/
        percs[percs$Cost %in% c("CREW"), !(names(percs) %in% "Cost")]

    percs[percs$Cost %in% c("REV"),
      !(names(percs) %in% "Cost")] <- 
        1 -
        colSums(percs[!(percs$Cost %in% c("REV", "CREW")),
          !(names(percs) %in% "Cost")])

    percs$Cost <- costflist_template$vessel$Cost
    percs$Type <- costflist_template$vessel$Cost
    percs[is.na(percs)] <- 0

    # Set column names from costflist_template$vessel
    colnames(percs) <- colnames(costflist_template$vessel)

    return(percs)

  } else if (costtype == "means") {

    return(list(means = means, sd = sdout))

  }

  }

}
