iopac_wrap <- function(impbridgein = impbridgelist, costfin = costflist_2020,
    multsin = mults, ticsin = ticslist_2020, ecpiin = ecpi, taxesin = taxes,
    prodflowin = prodflow, markupsin = markups_2020, fskeyin = fskey,
    comnamesin = comnames, procrecin = procrec) {
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
    #' @param procrecin A data frame with weights for commodity flows data from
    #'      IMPLAN (impbridgein), if used.
    #' @return A (n*m) by 12 data frame, where the first three columns
    #'      correspond to the geographic region, sector, and name of sector for
    #'      a multiplier, where there are *n* regions and *m* sectors.
    #' @export

if (is.null(procrecin) == TRUE) {
    procrecin <- rep(1, dim(impbridgein[["processor"]])[1])
}

# Change this loop one day so the kids don't make fun of you at school.
# Do you want to generalize so only produces provided data categories, e.g.
# only vessel multipliers, or only output and income multipliers? Run time not
# really an issue however.
results <- list()
for (i in names(ticsin)) {

Vessel_output <- make_v_mults(impbridge = impbridgein[["vessel"]],
    costf = costfin[["vessel"]], mults = multsin[["Output"]], type = "Output",
    sector = i, ticsin = ticsin[[i]], ecpi = ecpiin, taxes = taxesin)
Vessel_income <- make_v_mults(impbridge = impbridgein[["vessel"]],
    costf = costfin[["vessel"]], mults = multsin[["Income"]], type = "Income",
    sector = i, ticsin = ticsin[[i]], ecpi = ecpiin, taxes = taxesin)
Vessel_employment <- make_v_mults(impbridge = impbridgein[["vessel"]],
    costf = costfin[["vessel"]], mults = multsin[["Employment"]],
    type = "Employment", sector = i, ticsin = ticsin[[i]], ecpi = ecpiin,
    taxes = taxesin)

Processor_output <- make_p_mults(impbridge = impbridgein[["processor"]],
    costf = costfin[["processor"]], mults = multsin[["Output"]],
    type = "Output", sector = i, ticsin = ticsin[[i]], ecpi = ecpiin,
    taxes = taxesin, procrec = procrecin, prodflow = prodflowin,
    markups = markupsin, fskey = fskeyin)
Processor_income <- make_p_mults(impbridge = impbridgein[["processor"]],
    costf = costfin[["processor"]], mults = multsin[["Income"]],
    type = "Income", sector = i, ticsin = ticsin[[i]], ecpi = ecpiin,
    taxes = taxesin, procrec = procrecin, prodflow = prodflowin,
    markups = markupsin, fskey = fskeyin)
Processor_employment <- make_p_mults(impbridge = impbridgein[["processor"]],
    costf = costfin[["processor"]], mults = multsin[["Employment"]],
    type = "Employment", sector = i, ticsin = ticsin[[i]], ecpi = ecpiin,
    taxes = taxesin, procrec = procrecin, prodflow = prodflowin,
    markups = markupsin, fskey = fskeyin)

if (is.null(comnames) == TRUE) {
    comnames <- ticsin[[i]][1]
    names(comnames) <- "Name"
}

outmults <- cbind(comnames, Vessel_output, Vessel_income, Vessel_employment,
    Processor_output, Processor_income, Processor_employment)

outmults$TotOut <- outmults$Vessel_output + outmults$Processor_output
outmults$TotInc <- outmults$Vessel_income + outmults$Processor_income
outmults$TotEmp <- outmults$Vessel_employment + outmults$Processor_employment

results[[i]] <- outmults

}

return(dplyr::bind_rows(results, .id = "Region"))

}
