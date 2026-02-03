make_mult_bounds <- function(
  datain = clean_cost_data(functype="vessel", costtype="means"),
  ticsin = tics_list$y2023,
  markupsin = markups_list$y2023,
  draws = 300,
  seed = 42,
  drawtype = "normal",
  rawiters = FALSE,
  covarsin = NULL,
  countsin = costf_V_count_list$y2023) {

    set.seed(seed)

    if (is.null(covarsin) == TRUE && (drawtype == "multivariate" ||
      drawtype == "multivariate.lognormal")) {
      stop("Covariance matrices must be provided for multivariate draws.")
    } else if (drawtype == "multivariate" ||
      drawtype == "multivariate.lognormal") {
      covars <- covarsin
    }

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

        if (drawtype == "lognormal") {
          meanlogin <- log(means[,k]^2 / sqrt(sds[,k]^2 + means[,k]^2))
          meanlogin[is.nan(meanlogin)] <- 0
          meanlogin[is.infinite(meanlogin)] <- 0
          sdlogin <- sqrt(log(1 + (sds[,k]^2 / means[,k]^2)))
          sdlogin[is.nan(sdlogin)] <- 0
          sdlogin[is.infinite(sdlogin)] <- 0

          draw.mats[[j]][,k] = rlnorm(nrow(means),
            meanlog = meanlogin,
            sdlog = sdlogin)

          draw.mats[[j]][,k] = ifelse(draw.mats[[j]][,k]>3*sds[,k],3*sds[,k],
            draw.mats[[j]][,k])

        } else if (drawtype == "multivariate") {
          # If there is no covariance matrix for this column `k`, set draws to 0
          if (!(k %in% names(covars))) {
            draw.mats[[j]][, k] <- 0
          } else {
            covk <- covars[[k]]
            common_rows <- intersect(rownames(means), rownames(covk))
            if (length(common_rows) == 0) {
              draw.mats[[j]][, k] <- 0
            } else {
              mu_sub <- as.numeric(means[common_rows, k])
              names(mu_sub) <- common_rows
              Sigma_sub <- covk[common_rows, common_rows, drop = FALSE]
              draws_sub <- as.numeric(mvrnorm(1, mu = mu_sub,
                Sigma = Sigma_sub))

              # put draws for matching rows, zero for others
              draw.mats[[j]][, k] <- 0
              draw.mats[[j]][common_rows, k] <- draws_sub

              # enforce non-negativity and replace NA
              draw.mats[[j]][,k] <- ifelse(draw.mats[[j]][,k] < 0, 0, draw.mats[[j]][,k])
              draw.mats[[j]][is.na(draw.mats[[j]])] <- 0
            }
          }

        } else if (drawtype == "multivariate.lognormal") {

          # If there is no covariance matrix for this column `k`, set draws to 0
          if (!(k %in% names(covars))) {
            draw.mats[[j]][, k] <- 0
          } else {
            covk <- sing_cov_drop_zero(covars[[k]])

            common_rows <- intersect(rownames(means), rownames(covk))
            if (length(common_rows) == 0) {
              draw.mats[[j]][, k] <- 0
            } else {

            mu_sub <- as.numeric(means[common_rows, k])
            names(mu_sub) <- common_rows
            Sigma_sub <- covk[common_rows, common_rows, drop = FALSE]

            checkcounts <- change_names(countsin[countsin$V1 == "REV",
              !(names(countsin) %in% "V1")])

            if (checkcounts[[k]] <= dim(covk)[1]) {
              draw.mats[[j]][, k] <- 0
            } else {
            draws_sub <- as.numeric(compositions::rnorm.aplus(1,
              mean = mu_sub, var = Sigma_sub))

            # put draws for matching rows, zero for others
            draw.mats[[j]][, k] <- 0
            draw.mats[[j]][common_rows, k] <- draws_sub

            # cap extreme values and replace NA
            draw.mats[[j]][,k] <- ifelse(draw.mats[[j]][,k] > 5*sds[,k],
              5*sds[,k], draw.mats[[j]][,k])
            draw.mats[[j]][is.na(draw.mats[[j]])] <- 0
            }
          }
          }

        } else {

        draw.mats[[j]][, k] <- rnorm(nrow(means), mean = means[, k],
          sd = sds[, k])
        # draw.mats[[j]][,k] = rnorm(nrow(means),mean=means[,k],sd=0)

        #can't have a negative cost
        draw.mats[[j]][, k] <- ifelse(draw.mats[[j]][, k] < 0, 0,
          draw.mats[[j]][, k])

      }
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

    if (rawiters == TRUE) {
      return(output)
    } else {
    return(rbind(
      data.frame(output[[1]][, 1:3], row_percentiles_output,
        MultType = "Output"),
      data.frame(output[[1]][, 1:3], row_percentiles_income,
        MultType = "Income"),
        data.frame(output[[1]][, 1:3], row_percentiles_employment,
      MultType = "Employment")))
    }
  }