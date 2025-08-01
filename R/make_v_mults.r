make_v_mults <- function(impbridge, costf, mults, type, sector, ticsin, ecpi,
    taxes, output = NULL) {
    #' Create IOPAC vessel multipliers
    #'
    #' A function that creates impact multipliers for vessels
    #'
    #' @param impbridge Commodity flows data from IMPLAN. This is a data frame
    #'      containing data for the "vessel". The data frame has columns for the
    #'      Commodity code, Type of commodity, and Share, denoting the share
    #'      each commodity holds for their given type. Note that some shares
    #'      may not sum to unity because they may be further weighted by
    #'      additional data.
    #' @param costf Cost data from NWFSC surveys. This is a data frame for the
    #'      "vessel". The data frame contains columns for the Type of cost, and
    #'      additional columns for the cost share for each vessel type.
    #' @param mults Output, income, and employment multipliers from IMPLAN. This
    #'      is a data frames corresponding to the chosen type, with a column for
    #'      the Commodity Code, and additional columns for each study area
    #'      region.
    #' @param type String. Type of output ("output", "employment", or "income").
    #' @param sector String. Commodity type, corresponding to those in ticsin.
    #' @param ticsin Fish tickets by revenue from PACFIN. This is a data frame
    #'      corresponding to the study area chosen in sector. Each data frame
    #'      has the first column denoting commodity types, and additional
    #'      columns representing vessel types, which must match those in costf.
    #' @param ecpi Multipliers for employee compensation and proprietary income
    #'      from IMPLAN. This is a data frame with a column Type that denotes
    #'      the type of multiplier (of 6 variants), and additional columns
    #'      corresponding to study area regions from mults and ticsin.
    #' @param taxes Multipliers for induced effects from taxes from IMPLAN. This
    #'      is a data frame with rows for output, income, and employment, and
    #'      columns for each region. Only data for states (WA, OR, and CA) and
    #'      the west coast are included.
    #' @param output Type of output from the function. Default output are vessel
    #' level multipliers. "mults" gives industry category specific multipliers.
    #' "diremp" outputs direct employment. "dirinc" outputs direct income. 
    #' "propinc" outputs proprietary income. "indiremp" outputs indirect
    #' employment. "indirinc" outputs indirect income.
    #' @return vesoutcoef is a 1x1 multiplier.
    #' @export

# A. IMPLAN Bridge
impbridge$ID <- seq_len(nrow(impbridge))

# costf is loaded directly into the function
# B. Costs mapped to IMPLAN categories
costmult <- merge(impbridge[c("ID", "Type")], costf, by = c("Type"),
  all.x = TRUE, all.y = FALSE)
costmult <- costmult[order(costmult$ID), ]
mprod <- costmult[, !names(costmult) %in% c("ID", "Type", "Cost")]*
  impbridge$Share

# C. Hypothetical impact that gets cancelled out at the end
impact <- 1
# D. Lookup to join multipliers by category
lookmult <- merge(impbridge[c("ID", "CommodityCode")], mults,
    by = c("CommodityCode"), all.x=TRUE, all.y = FALSE, sort = FALSE)
lookmult <- lookmult[order(lookmult$ID), ]

# E. Impacts generated by all expenditures (except wages/prop inc)
# Grab correct region (Sector) here
inteff <- colSums(mprod*impact*as.matrix(lookmult[sector]), na.rm = TRUE)
  # 3520 noncomparable imports is 0

# F. Calculate indirect impacts from EC
empcomp <- colSums(as.data.frame(costf[costf$Type %in% c("Captain", "Crew"),
    !names(costf) %in% c("ID", "Type", "Cost")]))*impact*
    ecpi[ecpi$Type == paste0("EmpComp", type), c(sector)]

# G. Calculate indirect impacts from PI
propinc <- colSums(as.data.frame(costf[costf$Type %in% c("Purchases of permits",
        "Leasing of permits", "Proprietary income"),
    !names(costf) %in% c("ID", "Type", "Cost")]))*impact*
    ecpi[ecpi$Type == paste0("PropInc", type), c(sector)]

# H. Calculate indirect impacts from taxes/license fees
looktaxes <- colSums(as.data.frame(costf[costf$Type %in% c("Landings Taxes"),
    !names(costf) %in% c("ID", "Type", "Cost")]))*impact*
    taxes[taxes$Type == type, sector]
if (length(looktaxes) == 0) {
    looktaxes <- 0
}

if (type == "Output") {

# I. Calculate output impacts by adding together expenditures + employee wages +
# prop income + taxes + direct output
sumdirect <- inteff + empcomp + propinc + looktaxes + impact

} else if (type == "Income") {

# J. Calculate income impacts by adding together expenditures + employee wages +
# prop income + taxes + direct income
# note ecpi gives you the region-specific multiplier for income
sumdirect <- inteff + empcomp + propinc + looktaxes +
  colSums(as.data.frame(costf[costf$Type %in% c("Captain", "Crew",
    "Purchases of permits", "Leasing of permits", "Proprietary income"),
  !names(costf) %in% c("ID", "Type", "Cost")]))*impact

dirinc <- colSums(as.data.frame(costf[costf$Type %in% c("Captain", "Crew",
  "Purchases of permits", "Leasing of permits", "Proprietary income"),
    !names(costf) %in% c("ID", "Type", "Cost")]))*impact
indirinc <- inteff + empcomp + propinc + looktaxes
} else if (type == "Employment") {

# K. Calculate income impacts by adding together expenditures + employee wages +
# prop income + taxes + direct employment
# note ecpi gives you the region-specific multiplier for employment
diremp <- as.numeric(impact/(as.data.frame(costf[costf$Type %in% c(
  "Output per Employee"), !names(costf) %in% c("ID", "Type", "Cost")])))
diremp[is.infinite(diremp)] <- NaN

sumdirect <- inteff + empcomp + propinc + looktaxes + diremp

indiremp <- inteff + empcomp + propinc + looktaxes
# if some vessels don't have an output per employee, how do you handle that?
}

# L. Cancelling out the initial impact to get per dollar
mults <- sumdirect/impact

# M. Get fleet-level multipliers based on fish tickets
ticsprop <- ticsin[-1]/replicate(nrow(ticsin[-1]), rowSums(ticsin[-1]))

# Replace NaN with 0 only in rows where there are non-NaN values, otherwise keep
# all NaN if the whole row is NaN
dfout <- ticsprop*t(replicate(nrow(ticsprop), mults))
  for (i in seq_len(nrow(dfout))) {
    row_vals <- dfout[i, ]
    if (all(is.na(row_vals))) {
      # leave as is
      next
    } else {
      dfout[i, is.na(row_vals)] <- 0
    }
  }

vesoutcoef <- rowSums(dfout)

if (is.null(output)) {
  return(vesoutcoef)
} else if (output == "mults") {
  return(mults)
} else if (output == "diremp") {
  return(diremp)
} else if (output == "dirinc") {
  return(dirinc)
} else if (output == "propinc") {
  return(propinc)
} else if (output == "indiremp") {
  return(indiremp)
} else if (output == "indirinc") {
  return(indirinc)
} else {
  return(vesoutcoef)
}

}
