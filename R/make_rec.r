make_rec <- function(recdata = rec_survey_data,
  recmult = rec_multipliers,
  ticsdatin = tics_list$y2023,
  inflationin = 1) {

  recdata <- do.call(rbind, 
    lapply(names(rec_survey_data), function(name) {
      cbind(rec_survey_data[[name]], Source = name)
    })
  )

  rectypes <- do.call(rbind, strsplit(recdata$Source, "_"))
  colnames(rectypes) <- c("State", "TripType")

  recdata <- data.frame(recdata, rectypes)

  recdataexp <- recdata %>%
    group_by(State, TripType) %>%
    filter(Cat != "Trips") %>%
    summarise(TotExpenses = sum(Total, na.rm = TRUE))

  recmult$TripType <- str_match(recmult$Region, "_([^_]+)_")[,2]
  recmult$State <- sub("_.*", "", recmult$Region)
  recmult$Region <- gsub("^[^_]*_[^_]*_", "", recmult$Region)

  #do PRI first, FH needs to include charter contributions
  dataout <- merge(recmult[recmult$TripType == "PRI", ], recdataexp,
    by = c("State", "TripType"))
  dataout$OutputPerExpense <- dataout$Output / dataout$TotExpenses
  dataout$IncomePerExpense <- dataout$Income / dataout$TotExpenses
  dataout$EmploymentPerExpense <- dataout$Employment / dataout$TotExpenses

  inflation <- inflationin

  pridataout <- merge(dataout, recdata[recdata$Cat == "Trips",
    c("State", "TripType", "Total")], by = c("State", "TripType"))
  pridataout$DolPerTrip <- (pridataout$TotExpenses*inflation) / pridataout$Total

  pridataout$Output <- pridataout$OutputPerExpense * pridataout$DolPerTrip
  pridataout$Income <- pridataout$IncomePerExpense * pridataout$DolPerTrip
  pridataout$Employment <- pridataout$EmploymentPerExpense *
    pridataout$DolPerTrip

  #now do FH
  dataout <- merge(recmult[recmult$TripType == "FH", ], recdataexp,
    by = c("State", "TripType"))

  inflation <- inflationin

  fhdataout <- merge(dataout, recdata[recdata$Cat == "Trips",
    c("State", "TripType", "Total")], by = c("State", "TripType"))
  fhdataout$DolPerTrip <- (fhdataout$TotExpenses*inflation) / fhdataout$Total

  costf_charter$WA <- costf_charter$WA.and.OR
  costf_charter$OR <- costf_charter$WA.and.OR

  results <- setNames(lapply((unique(fhdataout$State)), function(j) {
    
    if (j == "CA") {
      i <- "California"
    } else if (j == "OR") {
      i <- "Oregon"
    } else if (j == "WA") {
      i <- "Washington"
    }

    Vessel <- lapply(c("Output", "Income", "Employment"), function(type) {
      vals <- make_v_mults(impbridge = impbridgelist[["charter"]],
        costf = costf_charter[(colnames(costf_charter) %in% c("Type", j))],
        mults = mults[[type]],
        type = type,
        sector = i,
        ticsin = ticsdatin[[i]],
        ecpi = ecpi,
        taxes = taxes)
      # If all values are NA/NaN, return NaN rather than -Inf; otherwise take max ignoring NAs
      if (all(is.na(vals))) {
        return(NaN)
      }
      max(vals, na.rm = TRUE)
    })
  
  names(Vessel) <- paste0("Charter_", c("output", "income", "employment"))

  return(data.frame(do.call(cbind, Vessel)))

  }), unique(fhdataout$State))

  chartermults <- bind_rows(results, .id = "State")

  fhdataoutwchart <- merge(fhdataout, chartermults, by = c("State"))

  guideexp <- recdata[recdata$Cat == c("Guide fees", "Crew tips"), ] %>%
    group_by(State,, TripType) %>%
    summarise(GuideExp = sum(Total))

  fhdataoutwchart <- merge(fhdataoutwchart, guideexp,
    by = c("State", "TripType")) %>%
    mutate(FHoutput = Charter_output*GuideExp,
           FHincome = Charter_income*GuideExp,
           FHemployment = Charter_employment*GuideExp) %>%
    mutate(TotOutput = Output + FHoutput,
           TotIncome = Income + FHincome,
           TotEmployment = Employment + FHemployment)

  fhdataoutwchart$OutputPerExpense <- fhdataoutwchart$TotOutput /
    fhdataoutwchart$TotExpenses
  fhdataoutwchart$IncomePerExpense <- fhdataoutwchart$TotIncome /
    fhdataoutwchart$TotExpenses
  fhdataoutwchart$EmploymentPerExpense <- fhdataoutwchart$TotEmployment /
    fhdataoutwchart$TotExpenses
  fhdataoutwchart$Output <- fhdataoutwchart$OutputPerExpense *
    fhdataoutwchart$DolPerTrip
  fhdataoutwchart$Income <- fhdataoutwchart$IncomePerExpense *
    fhdataoutwchart$DolPerTrip
  fhdataoutwchart$Employment <- fhdataoutwchart$EmploymentPerExpense *
    fhdataoutwchart$DolPerTrip

  return(rbind(pridataout[, c("Region", "State", "TripType", "Output", "Income",
    "Employment")],
    fhdataoutwchart[, c("Region", "State", "TripType", "Output", "Income",
    "Employment")]))

  }