make_mult_bounds <- function(
  datain = clean_cost_data(functype="vessel", costtype="means"),
  ticsin = tics_list$y2023,
  markupsin = markups_list$y2023,
  draws = 300,
  seed = 42) {

    set.seed(seed)

    means <- datain$means
    colnames(means) <-  colnames(costflist_template$vessel)[
      -length(colnames(costflist_template$vessel))]
    rownames(means) <- means[, 1]
    means[, 1] <- NULL

    sds <- datain$sd
    colnames(sds) <-  colnames(costflist_template$vessel)[
      -length(colnames(costflist_template$vessel))]
    rownames(sds) <- sds[, 1]
    sds[, 1] <- NULL

    draw.mats <- list()
    output <- list()
    no.data <- colnames(means)[which(apply(means, 2, sum) == 0)]
    # probably should call this
    iopac.costs <- costflist_template

    for (j in 1:draws) {
      draw.mats[[j]] <- matrix(NA, nrow = nrow(means), ncol = ncol(means))
      colnames(draw.mats[[j]]) <- colnames(means)
      rownames(draw.mats[[j]]) <- rownames(means)

      for (k in colnames(means)) {
        draw.mats[[j]][, k] <- rnorm(nrow(means), mean = means[, k],
          sd = sds[, k])
        # draw.mats[[j]][,k] = rnorm(nrow(means),mean=means[,k],sd=0)

        #can't have a negative cost
        draw.mats[[j]][, k] <- ifelse(draw.mats[[j]][, k] < 0, 0,
          draw.mats[[j]][, k])
        draw.mats[[j]][is.na(draw.mats[[j]])] <- 0
      }

      dataout <- draw.mats[[j]]
      dataout["REV", ] <- as.numeric(means["REV", ])
      dataout[, no.data] <- apply(dataout[, !colnames(dataout) %in% no.data], 1,
        mean)
      dataout <- rbind(dataout, prop.income = dataout["REV", ] -
        colSums(dataout[grep("COST",rownames(dataout)), ]))
      output.per.employee <- abs(dataout["REV", ]/dataout["CREW", ])
      dataout <- dataout[!(rownames(dataout) %in% c("REV", "CREW")), ]
      dataout <- prop.table(dataout, 2)
      dataout <- as.data.frame(rbind(dataout, output.per.employee))
      dataout <- replace(dataout, is.na(dataout), 0)
      iopac.costs$vessel[, colnames(dataout)] <- dataout
      iopac.costs$processor <- clean_cost_data(sums = costf_P_list[["y2023"]],
        functype = "processor")
      output[[j]] <- iopac_wrap(costfin=iopac.costs, ticsin=ticsin,
        markupsin=markupsin)
    }

    #could be TotOut, TotInc, TotEmp
    vessel_output <- lapply(output, function(x) x %>%
      select(Vessel_output))
    vessel_income <- lapply(output, function(x) x %>%
      select(Vessel_income))
    vessel_employment <- lapply(output, function(x) x %>%
      select(Vessel_employment))
    vessel_output <- do.call(cbind, vessel_output)
    vessel_income <- do.call(cbind, vessel_income)
    vessel_employment <- do.call(cbind, vessel_employment)

    # Calculate the 25th, 50th (median), and 75th percentiles for each row
    probs <- c(0.025, 0.5, 0.975)
    # If all values in a row are NaN, return NaN for those percentiles; otherwise
    # compute quantiles ignoring NaNs
    row_percentiles_output <- t(apply(vessel_output, MARGIN = 1, FUN = function(x) {
      if (all(is.nan(x))) {
        return(rep(NaN, length(probs)))
      }
      quantile(x, probs = probs, na.rm = TRUE)
    }))
    colnames(row_percentiles_output) <- c("Perc_025", "Perc_500",
      "Perc_975")

    row_percentiles_income <- t(apply(vessel_income, MARGIN = 1, FUN = function(x) {
      if (all(is.nan(x))) {
        return(rep(NaN, length(probs)))
      }
      quantile(x, probs = probs, na.rm = TRUE)
    }))
    colnames(row_percentiles_income) <- c("Perc_025", "Perc_500",
      "Perc_975")

    row_percentiles_employment <- t(apply(vessel_employment, MARGIN = 1, FUN = function(x) {
      if (all(is.nan(x))) {
        return(rep(NaN, length(probs)))
      }
      quantile(x, probs = probs, na.rm = TRUE)
    }))
    colnames(row_percentiles_employment) <- c("Perc_025", "Perc_500",
      "Perc_975")

    return(rbind(
      data.frame(output[[1]][, 1:3], row_percentiles_output,
        MultType = "Output"),
      data.frame(output[[1]][, 1:3], row_percentiles_income,
        MultType = "Income"),
        data.frame(output[[1]][, 1:3], row_percentiles_employment,
      MultType = "Employment")))

  }