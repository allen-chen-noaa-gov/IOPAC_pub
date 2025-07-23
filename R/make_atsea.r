make_atsea <- function(recdata = rec_survey_data,
  recmult = rec_multipliers) {




















cpabs <- read.csv(paste0("U:\\NWFSC_data_code\\IOPAC\\implan_params\\", 
    "CPabs.csv"), header=FALSE)
cpabs$Type <- cpabs$V1

newabs <- merge(cpabs, cpcosts[c("Type", "ShareC")], 
    by=c("Type"), all.x=TRUE)
    
newabs$Abs <- newabs$ShareC*newabs$V4
newabs$CommodityCode <- newabs$V2

incomeeff <- merge(newabs, mults$Income[c("CommodityCode","WC")],
    by = c("CommodityCode"))

sector <- "WC" 
type <- "Income"

inteff <- sum(incomeeff$Abs*revencp*incomeeff$WC)

empcomp <- revencp*
    sum(cpcosts$ShareC[cpcosts$Type %in% 
        c("Non-processing crew", "Processing crew")])*
    ecpi[ecpi$Type == paste0("EmpComp", type), c(sector)]

propinc <- revencp*
    (1-sum(cpcosts$ShareC))*
    ecpi[ecpi$Type == paste0("PropInc", type), c(sector)]

totinc <- ((sum(cpcosts$ShareC[cpcosts$Type %in% 
        c("Non-processing crew", "Processing crew")]) + 
        (1-sum(cpcosts$ShareC)))*revencp) +
        inteff +
        empcomp +
        propinc

CP_pounds_income_mult <- totinc/sum(cpcatch)

empeff <- merge(newabs, mults$Employment[c("CommodityCode","WC")], 
    by = c("CommodityCode"))

sector <- "WC" 
type <- "Employment"

empeff <- sum(empeff$Abs*revencp*empeff$WC)

empcompemp <- revencp*
    sum(cpcosts$ShareC[cpcosts$Type %in% 
        c("Non-processing crew", "Processing crew")])*
    ecpi[ecpi$Type == paste0("EmpComp", type), c(sector)]

propemp <- revencp*
    (1-sum(cpcosts$ShareC))*
    ecpi[ecpi$Type == paste0("PropInc", type), c(sector)]

totemp <- (revencp/(revencp/crewn)) +
        empeff +
        empcompemp +
        propemp
  
CP_pounds_employ_mult <- totemp/sum(cpcatch)





msabs <- read.csv(paste0("U:\\NWFSC_data_code\\IOPAC\\implan_params\\", 
    "MSabs.csv"), header=FALSE)
msabs$Type <- msabs$V1    

newabs <- merge(msabs, mscosts[c("Type", "ShareC")], 
    by=c("Type"), all.x=TRUE)      
    
newabs$Abs <- newabs$ShareC*newabs$V5
newabs$CommodityCode <- newabs$V2

incomeeff <- merge(newabs, mults$Income[c("CommodityCode","WC")], 
    by = c("CommodityCode"))

sector <- "WC" 
type <- "Income"

inteff <- sum(incomeeff$Abs*reven*incomeeff$WC)

empcomp <- reven*
    sum(mscosts$ShareC[mscosts$Type %in% 
        c("Non-processing crew", "Processing crew")])*
    ecpi[ecpi$Type == paste0("EmpComp", type), c(sector)]

propinc <- reven*
    (mscosts$ShareC[mscosts$Type %in% 
        c("Lease or charter of vessel")] + (1-sum(mscosts$ShareC)))*
    ecpi[ecpi$Type == paste0("PropInc", type), c(sector)]

totinc <- ((sum(mscosts$ShareC[mscosts$Type %in% 
        c("Non-processing crew", "Processing crew", 
        "Lease or charter of vessel")]) + 
        (1-sum(mscosts$ShareC)))*reven) +
        inteff +
        empcomp +
        propinc

i <- "WC"
Vessel_income <- make_v_mults_outm(impbridge=impbridgelist[["vessel"]], 
    costf=costflist_2022[["vessel"]], mults=mults[["Income"]], type = "Income", 
    sector = i, ticsin = ticslist_2022[[i]], ecpi=ecpi, taxes=taxes)
    
#cv purchases in revlbsdas not updated yet? only to 2018, do it by hand
MS_pounds_income_mult <- as.numeric((((
    costofpurchasejerryformat$Perc[1]*reven*
        Vessel_income["Pacific.Whiting.Trawler"] + 
    costofpurchasejerryformat$Perc[2]*reven*
        Vessel_income["Large.Groundfish.Trawler"] + 
    totinc)/
    reven)*reven)/
    sum(mscatch))

empeff <- merge(newabs, mults$Employment[c("CommodityCode","WC")], 
    by = c("CommodityCode"))

sector <- "WC" 
type <- "Employment"

empeff <- sum(empeff$Abs*reven*empeff$WC)

empcompemp <- reven*
    sum(mscosts$ShareC[mscosts$Type %in% 
        c("Non-processing crew", "Processing crew")])*
    ecpi[ecpi$Type == paste0("EmpComp", type), c(sector)]

propemp <- reven*
    (mscosts$ShareC[mscosts$Type %in% 
        c("Lease or charter of vessel")] + (1-sum(mscosts$ShareC)))*
    ecpi[ecpi$Type == paste0("PropInc", type), c(sector)]

totemp <- (reven/(reven/crewn)) +
        empeff +
        empcompemp +
        propemp

Vessel_emp <- make_v_mults_outm(impbridge=impbridgelist[["vessel"]], 
    costf=costflist_2022[["vessel"]], mults=mults[["Employment"]], 
    type = "Employment", 
    sector = i, ticsin = ticslist_2022[[i]], ecpi=ecpi, taxes=taxes)
    
MS_pounds_employ_mult <- as.numeric((((
    costofpurchasejerryformat$Perc[1]*reven*
        Vessel_emp["Pacific.Whiting.Trawler"] + 
    costofpurchasejerryformat$Perc[2]*reven*
        Vessel_emp["Large.Groundfish.Trawler"] + 
    totemp)/
    reven)*reven)/
    sum(mscatch))

data.frame(CP_pounds_income_mult, CP_pounds_employ_mult, MS_pounds_income_mult, 
    MS_pounds_employ_mult)

  return()

  }
