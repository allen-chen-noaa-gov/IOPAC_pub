# Read in data
library(IOPAC);require(tidyverse);require(MASS)
file_list <- list.files("/Users/scheld/Desktop/Current Research/NWFSC EIA/IOPAC_files/IOPAC_data")
for (i in 1:length(file_list)){load(paste0("/Users/scheld/Desktop/Current Research/NWFSC EIA/IOPAC_files/IOPAC_data/",file_list[i]))}

# Generate multivariate normal draws, produce multipliers for each year
draws <- 100
set.seed(123)
for(i in 2018:2021){
  means <- read.csv(paste0("/Users/scheld/Desktop/Current Research/NWFSC EIA/Data/means and sds/means",i,".csv"),row.names=1)
  cov <- readRDS(paste0("/Users/scheld/Desktop/Current Research/NWFSC EIA/Data/covars/covars",i,".RDS"))
  names(cov) <- c("Crabber","Alaska.Vessel","Migratory.Liner",
                  "Large.Groundfish.Trawler","Pelagic.Netter","Sablefish.Fixed.Gear",
                  "OtherG15","Salmon.Netter","Shrimper",
                  "OtherL15","Other.Groundfish.Fixed.Gear","Salmon.Troller",
                  "Small.Groundfish.Trawler","Pacific.Whiting.Trawler","Other.Netter")
  for(j in 1:length(cov)){
    cov[[j]] <- as.data.frame(cov[[j]])
    cov[[j]]["COST_INT",] <- 0
    cov[[j]][,"COST_INT"] <- 0
    cov[[j]] <- cov[[j]][rownames(means),rownames(means)]
  }
  means <- means[,!is.na(match(colnames(means),names(cov)))]
  draw.mats <- list(); output <- list()
  no.data <- colnames(means)[which(apply(means,2,sum)==0)]
  iopac.costs <- get(paste0("costflist_",i))
  for(k in 1:draws){
    draw.mats[[k]] <- matrix(NA,nrow=nrow(means),ncol=ncol(means))
    colnames(draw.mats[[k]]) <- colnames(means)
    rownames(draw.mats[[k]]) <- rownames(means)
    for(l in colnames(means)){
      draw.mats[[k]][,l] = mvrnorm(1,mu=means[,l],Sigma=cov[[l]]) 
      draw.mats[[k]][,l] = ifelse(draw.mats[[k]][,l]<0,0,draw.mats[[k]][,l])
      draw.mats[[k]][is.na(draw.mats[[k]])] <- 0
    }
    data <- draw.mats[[k]]
    data["REV",] <- as.numeric(means["REV",])
    data[,no.data] <- apply(data[,!colnames(data)%in%no.data],1,mean)
    data <- rbind(data,prop.income=data["REV",]-colSums(data[grep("COST",rownames(data)),]))
    output.per.employee <- abs(data["REV",]/data["CREW",])
    data <- data[!(rownames(data)%in%c("REV","CREW")),]
    data <- prop.table(data, 2) 
    data <- as.data.frame(rbind(data, output.per.employee))
    data <- replace(data, is.na(data), 0)
    iopac.costs$vessel[,colnames(data)] <- data
    output[[k]] <- iopac_wrap(costfin=iopac.costs,ticsin=get(paste0("ticslist_",i)),markupsin=get(paste0("markups_",i)))  
  }
  assign(paste0("draw.mats.multnormal.",i),draw.mats)
  assign(paste0("output.multnormal.",i),output)
  rm(means,cov,draw.mats,output,iopac.costs,data,output.per.employee)
}
