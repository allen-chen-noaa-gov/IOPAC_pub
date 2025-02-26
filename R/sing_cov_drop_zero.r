library(matrixcalc)

#your covariance .rds file path
inmat <- readRDS()

for (i in 1:length(inmat)) {

fleetmat <- inmat[[i]]

print("")
# print(is.positive.definite(fleetmat))

if (length(which(colSums(fleetmat == 0) == nrow(fleetmat))) != 0) {
fleetmat <- fleetmat[-which(colSums(fleetmat == 0) ==
  nrow(fleetmat)), -which(colSums(fleetmat == 0) == nrow(fleetmat))]
}

print(is.positive.definite(fleetmat))

}
