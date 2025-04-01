################################################################################
library(IOPAC);require(here);require(tidyverse)
################################################################################

datadir <- file.path("..", "IOPAC_data")
file_list <- list.files(datadir)
for (i in 1:length(file_list)){
  load(paste0(file.path("..", "IOPAC_data"), "/",  file_list[i]))
}

# Process raw values (rawvals_032725) to create shares:
datadir <- "U:\\NWFSC_data_code\\IOPAC\\rawvals_032725"

temp <- setwd(datadir)
temp <- list.files(pattern="*.csv")
files <- lapply(temp, read_csv)
Filenames <- gsub(".csv", "", as.character(temp))
names(files) <- Filenames

rownames <- as.character(as.data.frame(files[[1]])[,1])

# COSTS and SD
# files2<- lapply(files, function(x){data.frame(t(as.matrix(x))) })
# files2<-  lapply(files2, function(x){x[-c(1,2), ] })

#there was a transpose above that was messing me up
files2<- lapply(files, function(x){data.frame((as.matrix(x))) })
files2<-  lapply(files2, function(x){x[, -c(1)] })
files2<-  lapply(files2, function(x){x<- x %>% mutate(Cost= costflist_2018$vessel$Cost, Type=Cost)})
# files2<-lapply(files2, function(x) x %>% relocate(Cost, .before = 'X1'))

#No X1 in my data
files2<-lapply(files2, function(x) x %>% relocate(Cost))
colnames<- colnames(costflist_2022[[1]]) 
files2<- lapply(files2, setNames, colnames)

sums<- files2[1:5]
counts<- files2[6:10]
confdel <- files2[11:15]
sd<- files2[16:20]

confdel<- lapply(confdel, function(x){
  x[-c(1,20) ] 
})
confdel<- lapply(confdel, function(y){
  mutate_all(y, function(x) as.numeric(as.character(x)))
})
# confdel <- lapply(confdel, function(x) 
#   replace(x, is.na(x), 0)
# )
# confdel <- lapply(confdel, function(x) 
#   replace(x, x < 3, 0)
# )
# confdel <- lapply(confdel, function(x) 
#   replace(x, x > 2, 1)
# )

for (i in 1:length(counts)) {

replacecounts <- counts[[i]][counts[[i]]$Cost == "Proprietary income", 2:19]
replacecounts[is.na(replacecounts)] <- 0

counts[[i]][1:23, 2:19] <- replacecounts %>% slice(rep(1:n(), each=23))

counts[[i]][counts[[i]]$Cost == "Proprietary income", 1] <- "REV"

}

# divide sums by counts:
sums<- lapply(sums, function(x){
  x[-c(1,20) ] 
})
sums<- lapply(sums, function(y){
  mutate_all(y, function(x) as.numeric(as.character(x)))
})

counts<- lapply(counts, function(x){
  x[-c(1,20) ] 
})
counts<- lapply(counts, function(y){
  mutate_all(y, function(x) as.numeric(as.character(x)))
})

sd<- lapply(sd, function(x){
  x[-c(1,20) ] 
})
sd<- lapply(sd, function(y){
  mutate_all(y, function(x) as.numeric(as.character(x)))
})

sumsconf = mapply(FUN = `*`, sums, confdel, SIMPLIFY = FALSE) 
countsconf = mapply(FUN = `*`, counts, confdel, SIMPLIFY = FALSE) 
sdconf = mapply(FUN = `*`, sd, confdel, SIMPLIFY = FALSE) 

means = mapply(FUN = `/`, sumsconf, countsconf, SIMPLIFY = FALSE) # divide sums by counts of vessels
means<- rapply(means, f=function(x) ifelse(is.na(x),0,x), how="replace" )
means <-  lapply(means, function(x){x<- x %>% mutate(Cost=(rownames))})

sd <-  lapply(sdconf, function(x){x<- x %>% mutate(Cost= (rownames))}) 

for (i in 1:length(sd)) {
means[[i]] <- means[[i]] %>% relocate(Cost)
sd[[i]] <- sd[[i]] %>% relocate(Cost)
}

lapply(1:length(means), function(i) write.csv(means[[i]], 
  file = paste0(here(), "\\data\\", names(means[i]), ".csv"),
  row.names = FALSE)) # write 5 csv files for mean values
lapply(1:length(sd), function(i) write.csv(sd[[i]], 
  file = paste0(here(), "\\data\\", names(sd[i]), ".csv"),
  row.names = FALSE)) # write 5 csv files for sd- plug into below
