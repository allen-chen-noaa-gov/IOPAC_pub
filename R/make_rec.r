make_rec <- function(recdata = rec_survey_data,
  recmult = rec_multipliers) {

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

  #FH needs to include charter contributions
  dataout <- merge(recmult[recmult$TripType == "PRI", ], recdataexp,
    by = c("State", "TripType"))
  dataout$OutputPerExpense <- dataout$Output / dataout$TotExpenses
  dataout$IncomePerExpense <- dataout$Income / dataout$TotExpenses
  dataout$EmploymentPerExpense <- dataout$Employment / dataout$TotExpenses

  inflation <- 1

  dataout <- merge(dataout, recdata[recdata$Cat == "Trips",
    c("State", "TripType", "Total")], by = c("State", "TripType"))
  dataout$DolPerTrip <- (dataout$TotExpenses*inflation) / dataout$Total

  dataout$Output <- dataout$OutputPerExpense * dataout$DolPerTrip
  dataout$Income <- dataout$IncomePerExpense * dataout$DolPerTrip
  dataout$Employment <- dataout$EmploymentPerExpense * dataout$DolPerTrip

  return(dataout[, c("Region", "State", "TripType", "Output", "Income",
    "Employment")])

  }