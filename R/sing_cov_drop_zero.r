sing_cov_drop_zero <- function(covarsin = NULL) {

if (is.null(covarsin)) {
  stop("Covariance matrix must be provided")
} 

fleetmat <- covarsin

if (length(which(colSums(fleetmat == 0) == nrow(fleetmat))) != 0) {
fleetmat <- fleetmat[-which(colSums(fleetmat == 0) ==
  nrow(fleetmat)), -which(colSums(fleetmat == 0) == nrow(fleetmat))]
}
  return(fleetmat)
}
