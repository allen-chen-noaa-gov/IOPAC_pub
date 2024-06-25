make_p_mults <- function(impbridge, costf, mults, type, sector, ticsin, ecpi,
    taxes, procrec, prodflow, markups, fskey) {
    #' Create IOPAC processor multipliers
    #'
    #' A high-level wrapper function create impact multipliers
    #'
    #' @param impbridge Commodity flows data from IMPLAN. This is a data frame
    #'      containing data for the "processor". The data frame has columns for
    #'      the Commodity code, Type of commodity, and Share, denoting the share
    #'      each commodity holds for their given type. Note that some shares
    #'      may not sum to unity because they may be further weighted by
    #'      additional data.
    #' @param costf Cost data from NWFSC surveys. This is a data frame for the
    #'      "processor". The data frame contains columns for the Type of cost,
    #'      and additional columns for the cost share for each vessel type.
    #' @param mults Output, income, and employment multipliers from IMPLAN. This
    #'      is a data frames corresponding to the chosen type, with a column for
    #'      for the Commodity Code, and additional columns for each study area
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
    #' @param procrec A data frame with weights for commodity flows data from
    #'      IMPLAN (impbridge), if used.
    #' @param prodflow A three column data frame denoting product flow. The
    #'      first column Region denotes the study area regions matching mults
    #'      and ticsin. The other two columns denote total supply and processor
    #'      demand for each region.
    #' @param marksups Processor markups from NWFSC data. This is a data frame
    #'      with the first column Markups corresponding to commodity types from
    #'      ticsin. Then the column Value denotes processor markups.
    #' @param fskey A data frame denoting the relationship between species
    #'      (Markups), commodity types (Sector), and various gear types.
    #' @return procoutcoef is a 1x1 multiplier.
    #' @export

# A. IMPLAN Bridge
impbridge$ID <- seq_len(nrow(impbridge))
options(stringsAsFactors = FALSE)
# B. Cost Function
# costf is loaded directly into the function

# C. Costs mapped to IMPLAN categories
prodf <- merge(impbridge[c("ID", "Type", "Share")],
    costf[c("Type", "Value", "ShareC")], by = c("Type"), all.x = TRUE)

# There's a multiplier for rental/lease, but Jerry would manually zero out the cost
prodf[prodf$Type == 'Rental or lease of buildings, job-site trailers, and other structures',
    c("Value", "ShareC")] <- 0
prodf <- prodf[order(prodf$ID), ]

# F. Lookup to join multipliers by category
lookmult <- merge(impbridge[c("ID", "CommodityCode")], mults,
    by = c("CommodityCode"), all.x = TRUE, all.y = FALSE, sort = FALSE)
lookmult <- lookmult[order(lookmult$ID), ]

# D. Cost categories weighted by share of total cost, as well as within category
# weights
mapprod <- prodf$Share*prodf$ShareC*procrec

# E. Hypothetical impact that gets cancelled out at the end
impact <- 1

if (sector == "WC" && type == "Employment") {

#Jerry's mod only sums up to row 117, no interest, repairs, other (mistake?)
#only for WC
# FROM ERIN: I'm guessing you are right that it is a mistake?
# G. Impacts generated by all expenditures (except wages/prop inc)
wcjermod <- mapprod*impact*as.matrix(lookmult[sector])
inteff <- colSums(mapprod*impact*as.matrix(lookmult[sector])) -
    sum(wcjermod[118:136, ])

} else {
    inteff <- colSums(mapprod*impact*as.matrix(lookmult[sector]), na.rm = T)
}

# H. Calculate indirect impacts from EC
empcomp <- impact*
    costf$ShareC[costf$Type == "Employee and Worker Payroll"]*
    ecpi[ecpi$Type == paste0("EmpComp", type), c(sector)]

# I. Calculate indirect impacts from PI
propinc <- impact*
    costf$ShareC[costf$Type == "Proprietary Income"]*
    ecpi[ecpi$Type == paste0("PropInc", type), c(sector)]

# J. Calculate indirect impacts from taxes/license fees
looktaxes <- impact*
    sum(costf$ShareC[costf$Type %in% c("Licensing Fees", "Taxes")])*
    taxes[taxes$Type == type, sector]
if (length(looktaxes) == 0) {
    looktaxes <- 0
}

if (type == "Output") {

# K. Calculate output impacts by adding together expenditures + employee wages +
# prop income + taxes + direct output
sumdirect <- inteff + empcomp + propinc + looktaxes + impact

} else if (type == "Income") {
# L. Calculate income impacts by adding together expenditures + employee wages +
# prop income + taxes + direct income
# note ecpi gives you the region-specific multiplier for income
sumdirect <- inteff + empcomp + propinc + looktaxes +
    (sum(costf$ShareC[costf$Type %in%
    c("Employee and Worker Payroll", "Proprietary Income")])*impact)

} else if (type == "Employment") {
# M. Calculate income impacts by adding together expenditures + employee wages +
# prop income + taxes + direct employment
# note ecpi gives you the region-specific multiplier for employment
sumdirect <- inteff + empcomp + propinc + looktaxes +
    (impact/costf$Value[costf$Type %in%
    c("Output per Employee")])

}

# N. Cancelling out the initial impact to get per dollar
mults <- sumdirect/impact

# O. Use different processor demand for different regions
# how much seafood goes directly to processors
# for WC, value is 1
# prodflow - need to look into where this comes from
prodflow$Share <- prodflow$Processor.demand/prodflow$Total.Supply
prodflow$Share[prodflow$Share > 1] <- 1.00

fskey$ID <- seq_len(nrow(fskey))
ticsprop <- ticsin[-1]/replicate(nrow(ticsin[-1]), rowSums(ticsin[-1]))

# P. Markups to rescale the fish purchase info -> production value
markups$procrev <- markups$Value*
    impact*
    c(rep(prodflow$Share[prodflow$Region == sector], 10))

markups$procrevspecial <- markups$procrev
markups$procrevspecial[markups$Markups %in% 
    c("Whiting", "Sablefish", "Dover/thornyhead", "Other groundfish")] <-
  markups$Value[markups$Markups %in% 
    c("Whiting", "Sablefish", "Dover/thornyhead", "Other groundfish")]*impact*1

# Q. Production revenue for a given impact
procrev <- merge(fskey, markups, by = c("Markups"), all.x = TRUE, sort = FALSE)
procrev <- procrev[order(procrev$ID), ]

# #are there always only 10 species?
if (sector == "WC" || sector == "Washington" || sector == "California" ||
    sector == "Oregon") {

# R. Get fleet-level multipliers based on fish tickets
procrevmult <- cbind(replicate(4, (procrev$procrevspecial*mults)/impact),
    replicate((dim(ticsprop)[2] - 4), (procrev$procrev*mults)/impact))

} else {

procrevmult <- replicate(dim(ticsprop)[2], (procrev$procrev*mults)/impact)

}

# all(dim(ticsprop[1:(nrow(ticsprop)-4),]) ==
    # dim(replicate(dim(ticsprop)[2], (procrev$procrev*mults)/impact)))
#drop last four rows because they're calculated below, different aggregation
procoutcoef <- rowSums(ticsprop[1:(nrow(ticsprop) - 4), ]*procrevmult)

fskey <- cbind(fskey, procoutcoef)

#calculate last four rows (Erin can ignore this step)
procoutcoef <- c(procoutcoef,
    sum(fskey$procoutcoef[fskey$Trawl == 1], na.rm = TRUE)/
        length(fskey$procoutcoef[fskey$Trawl == 1]),
    sum(fskey$procoutcoef[fskey$FG == 1], na.rm = TRUE)/
        length(fskey$procoutcoef[fskey$FG == 1]),
    sum(fskey$procoutcoef[fskey$Net == 1], na.rm = TRUE)/
        length(fskey$procoutcoef[fskey$Net == 1]),
    sum(fskey$procoutcoef[fskey$Other == 1], na.rm = TRUE)/
        length(fskey$procoutcoef[fskey$Other == 1])
    )

return(procoutcoef)

}
