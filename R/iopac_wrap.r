iopac_wrap <- function(impbridgein = impbridgelist,
  costfin = NULL,
  multsin = mults,
  ticsin = tics_list$y2023,
  ecpiin = ecpi,
  taxesin = taxes,
  prodflowin = prodflow,
  markupsin = markups_list$y2023,
  fskeyin = fskey,
  comnamesin = comnames) {
  #' Create all IOPAC multipliers
  #'
  #' A high-level wrapper function to create impact multipliers
  #'
  #' @param impbridgein Commodity flows data from IMPLAN. This is a list
  #'      containing data for the "vessel" and "processor". Each list element
  #'      is then a data frame with columns for the Commodity code, Type of
  #'      commodity, and Share, denoting the share each commodity holds for
  #'      their given type. Note that some shares may not sum to unity because
  #'      they may be further weighted by additional data.
  #' @param costfin Cost data from NWFSC surveys. This is a list containing
  #'      data for the "vessel" and "processor" in each element. Each list
  #'      additional columns for the cost share for each vessel type.
  #' @param multsin Output, income, and employment multipliers from IMPLAN.
  #'      This is a list containing three data frames, with a column for the
  #'      Commodity Code, and additional columns for each study area region.
  #' @param ticsin Fish tickets by revenue from PACFIN. This is a list with a
  #'      number of elements equal to the number of study area regions in
  #'      mults. Each element is then a data frame with the first column
  #'      denoting commodity types, and additional columns representing
  #'      vessel types, which must match those in costf.
  #' @param ecpiin Multipliers for employee compensation and proprietary
  #'      income from IMPLAN. This is a data frame with a column Type that
  #'      denotes the type of multiplier (of 6 variants), and additional
  #'      columns corresponding to study area regions from mults and ticsin.
  #' @param taxesin Multipliers for induced effects from taxes from IMPLAN.
  #'      This is a data frame with rows for output, income, and employment,
  #'      and columns for each region. Only data for states (WA, OR, and CA)
  #'      and the West Coast are included.
  #' @param prodflowin A three column data frame denoting product flow. The
  #'      first column Region denotes the study area regions matching mults
  #'      and ticsin. The other two columns denote total supply and processor
  #'      demand for each region.
  #' @param marksupsin Processor markups from NWFSC data. This is a data frame
  #'      with the first column Markups corresponding to commodity types from
  #'      ticsin. Then the column Value denotes processor markups.
  #' @param fskeyin A data frame denoting the relationship between species
  #'      (Markups), commodity types (Sector), and various gear types.
  #' @param comnamesin A data frame denoting the relationship between
  #'      commodity types and corresponding commodity type codes, if used.
  #' @return A (n*m) by 12 data frame, where the first three columns
  #'      correspond to the geographic region, sector, and name of sector for
  #'      a multiplier, where there are *n* regions and *m* sectors.
  #' @export

results <- lapply(names(ticsin), function(i) {

  Vessel <- lapply(c("Output", "Income", "Employment"), function(type) {
    make_v_mults(impbridge = impbridgein[["vessel"]],
      costf = costfin[["vessel"]],
      mults = multsin[[type]],
      type = type,
      sector = i,
      ticsin = ticsin[[i]],
      ecpi = ecpiin,
      taxes = taxesin)
  })
  names(Vessel) <- paste0("Vessel_", c("output", "income", "employment"))

  Processor <- lapply(c("Output", "Income", "Employment"), function(type) {
    make_p_mults(impbridge = impbridgein[["processor"]],
      costf = costfin[["processor"]],
      mults = multsin[[type]],
      type = type,
      sector = i,
      ticsin = ticsin[[i]],
      ecpi = ecpiin,
      taxes = taxesin,
      prodflow = prodflowin,
      markups = markupsin,
      fskey = fskeyin)
  })
  names(Processor) <- paste0("Processor_", c("output", "income", "employment"))

  comnames <- if (is.null(comnamesin)) data.frame(Name = ticsin[[i]][, 1])
    else comnamesin

  outmults <- cbind(comnames, do.call(cbind, Vessel), do.call(cbind, Processor))
  outmults$TotOut <- outmults$Vessel_output + outmults$Processor_output
  outmults$TotInc <- outmults$Vessel_income + outmults$Processor_income
  outmults$TotEmp <- outmults$Vessel_employment + outmults$Processor_employment

  rownames(outmults) <- NULL
  return(outmults)

})

names(results) <- names(ticsin)

return(dplyr::bind_rows(results, .id = "Region"))

}