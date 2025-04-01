library(ggplot2)

#your output data .rds file path
inmat <- readRDS()

incmult <- list()
empmult <- list()
incout <- list()
empout <- list()
incmultout <- list()
empmultout <- list()
for (i in 1:length(inmat)) {

incmult[[i]] <- inmat[[i]]$TotInc[
  inmat[[i]]$Name == "Other Groundfish, Fixed Gear" &
  inmat[[i]]$Region == "WC"]

empmult[[i]] <- inmat[[i]]$TotEmp[
  inmat[[i]]$Name == "Other Groundfish, Fixed Gear" &
  inmat[[i]]$Region == "WC"]

#lingcod/rockfish revenue
allrev <- 20581180

incout[[i]] <- allrev*incmult[[i]] 
empout[[i]] <- allrev*empmult[[i]] 

incmultout[[i]] <- incmult[[i]] 
empmultout[[i]] <- empmult[[i]] 

}

summary(unlist(incmultout))
summary(unlist(empmultout)*1000)

#mean(unlist(empmult))
incrat <- (median(unlist(incout))/1000000)/21
emprat <- (median(unlist(empout)))/21

inc95 <- ((quantile(unlist(incout), c(.975)))/1000000)/21
inc05 <- ((quantile(unlist(incout), c(.025)))/1000000)/21

emp95 <- ((quantile(unlist(empout), c(.975))))/21
emp05 <- ((quantile(unlist(empout), c(.025))))/21

poltab <- read.csv(paste0(here(), "\\inst\\extdata\\", "policyex.csv"))
poltab <- poltab[poltab$Year < 2027, ]

incnoaction <- data.frame(Year = poltab$Year, Income = incrat*poltab$NoAction,
  Income025 = inc05*poltab$NoAction, Income975 = inc95*poltab$NoAction, 
  Policy = "No Action")

incalt1 <- data.frame(Year = poltab$Year, Income = incrat*poltab$Alt1,
  Income025 = inc05*poltab$Alt1, Income975 = inc95*poltab$Alt1, 
  Policy = "Alt 1")

incalt2 <- data.frame(Year = poltab$Year, Income = incrat*poltab$Alt2,
  Income025 = inc05*poltab$Alt2, Income975 = inc95*poltab$Alt2, 
  Policy = "Alt 2")

plotinc <- rbind(incnoaction, incalt1, incalt2)

pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(plotinc, aes(x=Year, y=Income, colour=Policy)) + 
    geom_errorbar(aes(ymin=Income025, ymax=Income975), width=.5, position=pd) +
    geom_line(position=pd, size = 1) +
    geom_point(position=pd, shape=21, size=3, fill="white")

ggsave(paste0(here(), "\\inst\\", "incomeex.png"), width = 16, height = 9,
  units = "cm")

empnoaction <- data.frame(Year = poltab$Year, Emp = emprat*poltab$NoAction,
  Emp025 = emp05*poltab$NoAction, Emp975 = emp95*poltab$NoAction, 
  Policy = "No Action")

empalt1 <- data.frame(Year = poltab$Year, Emp = emprat*poltab$Alt1,
  Emp025 = emp05*poltab$Alt1, Emp975 = emp95*poltab$Alt1, 
  Policy = "Alt 1")

empalt2 <- data.frame(Year = poltab$Year, Emp = emprat*poltab$Alt2,
  Emp025 = emp05*poltab$Alt2, Emp975 = emp95*poltab$Alt2, 
  Policy = "Alt 2")

plotemp <- rbind(empnoaction, empalt1, empalt2)

ggplot(plotemp, aes(x=Year, y=Emp, colour=Policy)) + 
    geom_errorbar(aes(ymin=Emp025, ymax=Emp975), width=.5, position=pd) +
    geom_line(position=pd, size = 1) +
    geom_point(position=pd, shape=21, size=3, fill="white")

ggsave(paste0(here(), "\\inst\\", "empex.png", width = 16, height = 9,
  units = "cm")

outtable <- data.frame(poltab, NoAction_Income = incrat*poltab$NoAction, 
  Alt1_Income = incrat*poltab$Alt1, Alt2_Income = incrat*poltab$Alt2,
  NoAction_Emp = emprat*poltab$NoAction, Alt1_Emp = emprat*poltab$Alt1, 
  Alt2_Emp = emprat*poltab$Alt2)

write.table(outtable, file = paste0(here(), "\\inst\\", "policyextable.txt"),
  col.names = TRUE, row.names = FALSE, sep = "\t")
