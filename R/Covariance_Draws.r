################################################################################
library(IOPAC);require(tidyverse); require(MASS)
################################################################################
# covariance draws: 
options(scipen=999)
########################################################################################
########################################################################################
########################################################################################
# some info/notes:

# -note cov mats have 15 fleets, data has 18 fleets---- no cov for migatory netter, lobster, & diver- additionally 
# -no "COST_INT" in cov matrices

# -idea with method is that you take the mean values and covariance matrix- take 1 draw from the multivariate normal distribution
# and output the results into a matrix of all mean values across fleets- do this 1000 times - should verify this method of draws is correct
# and accounts for covariance as intended
#########################################################################
# 1/23/25- updates- for cov
# read in updated mean values here
datadir.2<-  # to revised  mean files files (based on last iteration of saved means)
datadir.3<- # to cov files- saved as rds files

#file_list <- list.files(paste0("")) # not sure what is supposed to go here fir list files? #Allen 012725 - I think maybe this used to load the other IOPAC data?
datadir <- file.path("..", "IOPAC_data")
file_list <- list.files(datadir)
for (i in 1:length(file_list)){
  load(paste0(file.path("..", "IOPAC_data"), "/",  file_list[i]))
}

#costflist_2017$processor$Value <- costflist_2017$processor$Xn2017
costflist_2018$processor$Value <- costflist_2018$processor$Xn2018
costflist_2019$processor$Value <- costflist_2019$processor$Xn2019
costflist_2020$processor$Value <- costflist_2020$processor$Xn2020
costflist_2021$processor$Value <- costflist_2021$processor$Xn2021
costflist_2022$processor$Value <- costflist_2022$processor$Xn2022

#costflist_2017$processor$ShareC <- costflist_2017$processor$Xn2017/
# costflist_2017$processor$Xn2017[costflist_2017$processor$Type=="Revenue"] # dont have means for 2017
costflist_2018$processor$ShareC <- costflist_2018$processor$Xn2018/
  costflist_2018$processor$Xn2018[costflist_2018$processor$Type=="Revenue"]
costflist_2019$processor$ShareC <- costflist_2019$processor$Xn2019/
  costflist_2019$processor$Xn2019[costflist_2019$processor$Type=="Revenue"]
costflist_2020$processor$ShareC <- costflist_2020$processor$Xn2020/
  costflist_2020$processor$Xn2020[costflist_2020$processor$Type=="Revenue"]
costflist_2021$processor$ShareC <- costflist_2021$processor$Xn2020/
  costflist_2021$processor$Xn2020[costflist_2021$processor$Type=="Revenue"] # issues with 2021 -- typo- should be xn2021 but is xn2020 (within file)
costflist_2022$processor$ShareC <- costflist_2022$processor$Xn2022/
  costflist_2022$processor$Xn2022[costflist_2022$processor$Type=="Revenue"]


############


start.time <- Sys.time()
draws <- 100
set.seed(123)
for(i in 2018:2020){ # avail years based on means and cov 
  means <- read.csv(paste0(datadir.2, "revised.costf_", i, "_rev.csv"),row.names=1)
  cov <- readRDS(paste0(datadir.3, "covars", i,".rds"))
  means<- as.data.frame(t(means))
  means<- means %>% 
    relocate("CREW", .after="COST_TRAV" )
  means <- means %>% mutate(fleet= rownames(means))
  means_list <- split(means[ ,c(1:7,9:23)], list(Group = means$fleet))
  means_list<- means_list[-c(3,5,7)] # remove the fleets with 0 costs- as they are not included in cov matrices
  
  # section to rename cov matrices to match means- and put in same order as means_list
  names(cov)[2] <- "Alaska.Vessel"
  names(cov)[3] <- "Migratory.Liner"
  names(cov)[4] <- "Large.Groundfish.Trawler"
  names(cov)[5] <- "Pelagic.Netter"
  names(cov)[6] <- "Sablefish.Fixed.Gear"
  names(cov)[7] <- "OtherG15"
  names(cov)[8] <- "Salmon.Netter"
  names(cov)[9] <- "Shrimper"
  names(cov)[10] <- "OtherL15"
  names(cov)[11] <- "Other.Groundfish.Fixed.Gear"
  names(cov)[12] <- "Salmon.Troller"
  names(cov)[13] <- "Small.Groundfish.Trawler"
  names(cov)[14] <- "Pacific.Whiting.Trawler"
  names(cov)[15] <- "Other.Netter"
  cov<- cov[ order(match(names(cov), names(means_list))) ]  # reorder cov-matrix to have same row/column order as means
  
  draw.mats <- list(); output <- list()
  no.data <- rownames(means)[which(apply(means[,c(1:23)],1,sum)==0)] 
  iopac.costs <- get(paste0("costflist_",i))
  
  for(j in 1:draws){
    draw.mats[[j]] <- matrix(NA, 22,15)
    colnames(draw.mats[[j]]) <- names(means_list)
    rownames(draw.mats[[j]]) <- rownames(cov[[1]])
    
    for(k in names(means_list)){
      Test_mean<- as.numeric(t(means_list[[k]]))
      Test_mean<- Test_mean[ order(match(colnames(means_list[[k]]), rownames(cov[[k]]))) ] # added this- think orders of means and cov matrix didnt match!
      draw.mats[[j]][,k] = mvrnorm(n=1, mu = Test_mean, Sigma = cov[[k]]) # MVT Normal draw
      draw.mats[[j]][,k] = ifelse(draw.mats[[j]][,k]<0,0, draw.mats[[j]][,k]) # functions retained from sd version - sets values less than 0 to 0
      draw.mats[[j]][is.na(draw.mats[[j]])] <- 0 # functions retained from sd version - sets NA values to 0
    }
    
    data <- draw.mats[[j]]
    data<- as.data.frame(data)
    data["COST_INT",] <- 0 # add row of all 0 values and call it "COST_INT" b/c this row was removed to fit cov mat.
    data[,no.data]<- apply(data[,!colnames(data)%in%no.data],1,mean)  
    raw_mean_rev<- as.data.frame((t(means["REV"]))) # set rev based on mean value (w/o draws)
    raw_mean_rev <- raw_mean_rev[ order(match(colnames(raw_mean_rev), names(means_list))) ] 

    no.data.means <- colnames(raw_mean_rev)[which(apply(raw_mean_rev,2,sum)==0)] 
    raw_mean_rev[,no.data]<- apply(raw_mean_rev[,!colnames(raw_mean_rev)%in%no.data.means],1,mean)  
    raw_mean_rev <- raw_mean_rev[ order(match(names(raw_mean_rev), (colnames(data)))) ] 

    data["REV",] <- as.numeric(raw_mean_rev)
    data<- as.matrix(data)
    data <- rbind(data,prop.income=data["REV",]-colSums(data[grep("COST",rownames(data)),]))
    output.per.employee <- abs(data["REV",]/data["CREW",])

    no.data.emp <- which(is.infinite(output.per.employee) == TRUE)
    output.per.employee[no.data.emp ]<- mean(output.per.employee[-(no.data.emp)])  

    data <- data[!(rownames(data)%in%c("REV","CREW")),]
    data <- prop.table(data, 2) 
    data <- as.data.frame(rbind(data, output.per.employee))
    data <- replace(data, is.na(data), NA)
    data[as.matrix(data) == Inf]  <- NA
    data[as.matrix(data) == -Inf]  <- NA
    
    rownames2 <- c("COST_CAPTAIN", "COST_CREW", "COST_FUEL", "COST_FOOD", "COST_ICE", "COST_BAIT", "COST_RMI", "COST_INS", "COST_INT", "COST_PUR", "COST_LEA", "COST_MOORAGE", "COST_LANDINGSTAX", "COST_ENFORCE", "COST_DUES", "COST_FREIGHT", "COST_OFFLOAD", "COST_TRUCK", "COST_OTHER", "COST_COMM", "COST_TRAV", "prop.income", "output.per.employee")

    # reorder rows to fit IOPAC
    # rownames2<- (as.data.frame(rownames(iopac.costs$vessel)))
    # rownames2$rownames2[rownames2$rownames2 =="REV"] <- "prop.income" 
    # rownames2$rownames2[rownames2$rownames2 =="CREW"] <- "output.per.employee"
    data <- data[ order(match(rownames(data), rownames2)), ] # reorder rows to fit IOPAC
    
    iopac.costs$vessel[,colnames(data)] <- data
    output[[j]] <-  iopac_wrap(costfin=iopac.costs, ticsin=get(paste0("ticslist_",i)), markupsin=get(paste0("markups_",i)))  # issues with emplyment this way but output works here
    
  }
  
  assign(paste0("draw.mats.cov.",i),draw.mats)
  assign(paste0("output.normal.cov.",i),output)
  rm(means,cov,draw.mats,output,iopac.costs,data,output.per.employee)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 

