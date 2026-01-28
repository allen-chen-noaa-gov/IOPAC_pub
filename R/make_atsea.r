make_atsea <- function(cpmscostsf = cpmscosts$y2023,
  ticsindat = tics_list$y2023,
  cpabs = impbridgelist$cpms,
  multsin = mults,
  ecpiin = ecpi,
  costfin = NULL) {

cpcosts <- cpmscostsf$CP

newabs <- merge(cpabs, cpcosts[c("Type", "ShareC")],
    by=c("Type"), all.x=TRUE)

newabs$Abs <- newabs$ShareC*newabs$Share

incomeeff <- merge(newabs, multsin$Income[c("CommodityCode","WC")],
    by = c("CommodityCode"))

sector <- "WC"
type <- "Income"

revencp <- cpcosts$COST[cpcosts$Type == "Revenue"]
cpcatch <- cpcosts$COST[cpcosts$Type == "Catch"]
crewn <- cpcosts$COST[cpcosts$Type == "Crew"]

inteff <- sum(incomeeff$Abs*revencp*incomeeff$WC)

empcomp <- revencp*
    sum(cpcosts$ShareC[cpcosts$Type %in% 
      c("Non-processing crew", "Processing crew")])*
    ecpiin[ecpiin$Type == paste0("EmpComp", type), c(sector)]

propinc <- revencp*
    (1-sum(cpcosts$ShareC[!cpcosts$Type %in% c("Revenue", "Crew", "Catch")]))*
    ecpiin[ecpiin$Type == paste0("PropInc", type), c(sector)]

totinc <- ((sum(cpcosts$ShareC[cpcosts$Type %in% 
        c("Non-processing crew", "Processing crew")]) + 
        (1-sum(cpcosts$ShareC[!cpcosts$Type %in%
          c("Revenue", "Crew", "Catch")])))*revencp) +
        inteff +
        empcomp +
        propinc

CP_pounds_income_mult <- totinc/sum(cpcatch)

empeff <- merge(newabs, multsin$Employment[c("CommodityCode","WC")], 
    by = c("CommodityCode"))

sector <- "WC" 
type <- "Employment"

empeff <- sum(empeff$Abs*revencp*empeff$WC)

empcompemp <- revencp*
    sum(cpcosts$ShareC[cpcosts$Type %in% 
        c("Non-processing crew", "Processing crew")])*
    ecpiin[ecpiin$Type == paste0("EmpComp", type), c(sector)]

propemp <- revencp*
    (1-sum(cpcosts$ShareC))*
    ecpiin[ecpiin$Type == paste0("PropInc", type), c(sector)]

totemp <- (revencp/(revencp/crewn)) +
        empeff +
        empcompemp +
        propemp
  
CP_pounds_employ_mult <- totemp/sum(cpcatch)

msabs <- cpabs[!cpabs$Type %in% c("Marine Council Fees", "Sea state"), ]

mscosts <- cpmscostsf$MS

newabs <- merge(msabs, mscosts[c("Type", "ShareC")], 
    by=c("Type"), all.x=TRUE)      
    
newabs$Abs <- newabs$ShareC*newabs$Share

incomeeff <- merge(newabs, multsin$Income[c("CommodityCode","WC")], 
    by = c("CommodityCode"))

sector <- "WC" 
type <- "Income"

reven <- mscosts$COST[mscosts$Type == "Revenue"]
mscatch <- mscosts$COST[mscosts$Type == "Catch"]
mscrewn <- mscosts$COST[mscosts$Type == "Crew"]
costofpurchasejerryformat2 <- mscosts$ShareC[mscosts$Type ==
  "Cost of purchase 2"]
costofpurchasejerryformat3 <- mscosts$ShareC[mscosts$Type ==
  "Cost of purchase 3"]

inteff <- sum(incomeeff$Abs*reven*incomeeff$WC)

empcomp <- reven*
    sum(mscosts$ShareC[mscosts$Type %in% 
        c("Non-processing crew", "Processing crew")])*
    ecpiin[ecpiin$Type == paste0("EmpComp", type), c(sector)]

propinc <- reven*
  (mscosts$ShareC[mscosts$Type %in% 
    c("Lease or charter of vessel")] + (1-sum(
      mscosts$ShareC[!mscosts$Type %in% c("Revenue", "Crew", "Catch",
        "Cost of purchase 2", "Cost of purchase 3")])))*
  ecpiin[ecpiin$Type == paste0("PropInc", type), c(sector)]

totinc <- ((sum(mscosts$ShareC[mscosts$Type %in% 
        c("Non-processing crew", "Processing crew", 
        "Lease or charter of vessel")]) + 
        (1-sum(mscosts$ShareC[!mscosts$Type %in% c("Revenue", "Crew", "Catch",
        "Cost of purchase 2", "Cost of purchase 3")])))*reven) +
        inteff +
        empcomp +
        propinc

i <- "WC"
Vessel_income <- make_v_mults(impbridge=impbridgelist[["vessel"]], 
    costf=costfin$vessel, mults=multsin[["Income"]], type = "Income", 
    sector = i, ticsin = ticsindat[[i]], ecpi=ecpiin, taxes=taxes,
    output = "mults")
    
#cv purchases in revlbsdas not updated yet? only to 2018, do it by hand
MS_pounds_income_mult <- as.numeric((((
    max(costofpurchasejerryformat2, 0)*reven*
        Vessel_income["Pacific.Whiting.Trawler"] + 
    max(costofpurchasejerryformat3, 0)*reven*
        Vessel_income["Large.Groundfish.Trawler"] + 
    totinc)/
    reven)*reven)/
    sum(mscatch))

empeff <- merge(newabs, multsin$Employment[c("CommodityCode","WC")], 
    by = c("CommodityCode"))

sector <- "WC" 
type <- "Employment"

empeff <- sum(empeff$Abs*reven*empeff$WC)

empcompemp <- reven*
    sum(mscosts$ShareC[mscosts$Type %in% 
        c("Non-processing crew", "Processing crew")])*
    ecpiin[ecpiin$Type == paste0("EmpComp", type), c(sector)]

propemp <- reven*
    (mscosts$ShareC[mscosts$Type %in% 
        c("Lease or charter of vessel")] + (1-sum(
      mscosts$ShareC[!mscosts$Type %in% c("Revenue", "Crew", "Catch",
        "Cost of purchase 2", "Cost of purchase 3")])))*
    ecpiin[ecpiin$Type == paste0("PropInc", type), c(sector)]

totemp <- (reven/(reven/crewn)) +
        empeff +
        empcompemp +
        propemp

Vessel_emp <- make_v_mults(impbridge=impbridgelist[["vessel"]], 
    costf=costfin$vessel, mults=multsin[["Employment"]],
    type = "Employment", 
    sector = i, ticsin = ticsindat[[i]], ecpi=ecpiin, taxes=taxes,
    output = "mults")
    
MS_pounds_employ_mult <- as.numeric((((
    max(costofpurchasejerryformat2, 0)*reven*
        Vessel_emp["Pacific.Whiting.Trawler"] + 
    max(costofpurchasejerryformat3, 0)*reven*
        Vessel_emp["Large.Groundfish.Trawler"] + 
    totemp)/
    reven)*reven)/
    sum(mscatch))

  return(data.frame(CP_pounds_income_mult, CP_pounds_employ_mult,
    MS_pounds_income_mult, MS_pounds_employ_mult))

  }