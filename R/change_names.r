change_names <- function(namesin = NULL) {

if (is.null(namesin)) {
  stop("Names vector must be provided")
} 

  mapping <- c(
    "Alaska.fisheries.vessel"      = "Alaska.Vessel",
    "Pacific.whiting.trawler"      = "Pacific.Whiting.Trawler",
    "Large.groundfish.trawler"     = "Large.Groundfish.Trawler",
    "Small.goundfish.trawler"      = "Small.Groundfish.Trawler",
    "Sablefish.fixed.gear"         = "Sablefish.Fixed.Gear",
    "Other.groundfish.fixed.gear"  = "Other.Groundfish.Fixed.Gear",
    "Pelagic.netter"               = "Pelagic.Netter",
    "Migratory.netter"             = "Migratory.Netter",
    "Migratory.liner"              = "Migratory.Liner",
    "Shrimper"                     = "Shrimper",
    "Crabber"                      = "Crabber",
    "Salmon.troller"               = "Salmon.Troller",
    "Salmon.netter"                = "Salmon.Netter",
    "Other.netter"                 = "Other.Netter",
    "Lobster.vessel"               = "Lobster",
    "Diver.vessel"                 = "Diver",
    "Other..more.than.15K"         = "OtherG15",
    "Other..less.than.15K"         = "OtherL15"
  )

 names(namesin) <- mapping[names(namesin)]

  return(namesin)
}



