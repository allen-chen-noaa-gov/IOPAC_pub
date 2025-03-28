################################################################################
library(IOPAC);require(here);require(tidyverse)
# IOPAC with normal and log-normal draws:
# Read in IOPAC data
datadir <- file.path("..", "IOPAC_data") #in parent folder of package
file_list <- list.files(datadir)
for (i in 1:length(file_list)){
  load(paste0(datadir, "/",  file_list[i]))
}

costflist_2017$processor$Value <- costflist_2017$processor$Xn2017
costflist_2018$processor$Value <- costflist_2018$processor$Xn2018
costflist_2019$processor$Value <- costflist_2019$processor$Xn2019
costflist_2020$processor$Value <- costflist_2020$processor$Xn2020
costflist_2021$processor$Value <- costflist_2021$processor$Xn2021
costflist_2022$processor$Value <- costflist_2022$processor$Xn2022

# note output per employee is taken from value in make_p_mults
costflist_2017$processor$ShareC <- costflist_2017$processor$Xn2017/
  costflist_2017$processor$Xn2017[costflist_2017$processor$Type=="Revenue"]
costflist_2018$processor$ShareC <- costflist_2018$processor$Xn2018/
  costflist_2018$processor$Xn2018[costflist_2018$processor$Type=="Revenue"]
costflist_2019$processor$ShareC <- costflist_2019$processor$Xn2019/
  costflist_2019$processor$Xn2019[costflist_2019$processor$Type=="Revenue"]
costflist_2020$processor$ShareC <- costflist_2020$processor$Xn2020/
  costflist_2020$processor$Xn2020[costflist_2020$processor$Type=="Revenue"]
costflist_2021$processor$ShareC <- costflist_2021$processor$Xn2021/
  costflist_2021$processor$Xn2021[costflist_2021$processor$Type=="Revenue"]
costflist_2022$processor$ShareC <- costflist_2022$processor$Xn2022/
  costflist_2022$processor$Xn2022[costflist_2022$processor$Type=="Revenue"]

# Generate normal draws, produce multipliers for each year
draws <- 1000
set.seed(123)
for(i in 2018:2021){
  means <- read.csv(paste0(here(), "\\data\\", "costf_", i, ".csv"),
    row.names = 1)
  sds <- read.csv(paste0(here(), "\\data\\", "costf_sd_", i, ".csv"),
    row.names = 1)
  draw.mats <- list(); output <- list()
  no.data <- colnames(means)[which(apply(means,2,sum)==0)]
  iopac.costs <- get(paste0("costflist_",i))

  for(j in 1:draws){
    draw.mats[[j]] <- matrix(NA,nrow=nrow(means),ncol=ncol(means))
    colnames(draw.mats[[j]]) <- colnames(means)
    rownames(draw.mats[[j]]) <- rownames(means)

    for(k in colnames(means)){
      draw.mats[[j]][,k] = rnorm(nrow(means),mean=means[,k],sd=sds[,k])
      # draw.mats[[j]][,k] = rnorm(nrow(means),mean=means[,k],sd=0)

      draw.mats[[j]][,k] = ifelse(draw.mats[[j]][,k]<0,0,draw.mats[[j]][,k])
      draw.mats[[j]][is.na(draw.mats[[j]])] <- 0
    }

    data <- draw.mats[[j]]
    data["REV",] <- as.numeric(means["REV",])
    data[,no.data] <- apply(data[,!colnames(data)%in%no.data],1,mean)
    data <- rbind(data,prop.income=data["REV",]-
      colSums(data[grep("COST",rownames(data)),]))
    output.per.employee <- abs(data["REV",]/data["CREW",])
    data <- data[!(rownames(data)%in%c("REV","CREW")),]
    data <- prop.table(data, 2) 
    data <- as.data.frame(rbind(data, output.per.employee))
    data <- replace(data, is.na(data), 0)
    iopac.costs$vessel[,colnames(data)] <- data
    output[[j]] <- iopac_wrap(costfin=iopac.costs,
      ticsin=get(paste0("ticslist_",i)), markupsin=get(paste0("markups_",i)))
  }

  assign(paste0("draw.mats.",i),draw.mats)
  assign(paste0("output.normal.",i),output)
  rm(means,sds,draw.mats,output,iopac.costs,data,output.per.employee)
}

# Generate lognormal draws, produce multipliers for each year
set.seed(123)
for(i in 2018:2021){
  means <- read.csv(paste0(here(), "\\data\\", "costf_", i, ".csv"),
    row.names=1)
  sds <- read.csv(paste0(here(), "\\data\\", "costf_sd_", i, ".csv"),
    row.names=1)
  draw.mats <- list(); output <- list()
  no.data <- colnames(means)[which(apply(means,2,sum)==0)]
  iopac.costs <- get(paste0("costflist_",i))

  for(j in 1:draws){
    draw.mats[[j]] <- matrix(NA,nrow=nrow(means),ncol=ncol(means))
    colnames(draw.mats[[j]]) <- colnames(means)
    rownames(draw.mats[[j]]) <- rownames(means)

    for(k in colnames(means)){
      draw.mats[[j]][,k] = rlnorm(nrow(means),
      meanlog=log(means[,k]^2 / sqrt(sds[,k]^2 + means[,k]^2)),
      sdlog=sqrt(log(1 + (sds[,k]^2 / means[,k]^2))))

      draw.mats[[j]][,k] = ifelse(draw.mats[[j]][,k]>3*sds[,k],3*sds[,k],
        draw.mats[[j]][,k])
      draw.mats[[j]][is.na(draw.mats[[j]])] <- 0
    }

    data <- draw.mats[[j]]
    data["REV",] <- as.numeric(means["REV",])
    data[,no.data] <- apply(data[,!colnames(data)%in%no.data],1,mean)
    data <- rbind(data,prop.income=data["REV",]-
      colSums(data[grep("COST",rownames(data)),]))
    output.per.employee <- abs(data["REV",]/data["CREW",])
    data <- data[!(rownames(data)%in%c("REV","CREW")),]
    data <- prop.table(data, 2) 
    data <- as.data.frame(rbind(data, output.per.employee))
    data <- replace(data, is.na(data), 0)
    iopac.costs$vessel[,colnames(data)] <- data
    output[[j]] <- iopac_wrap(costfin=iopac.costs,
      ticsin=get(paste0("ticslist_",i)),markupsin=get(paste0("markups_",i)))
  }
  
  assign(paste0("draw.mats.",i),draw.mats)
  assign(paste0("output.lognormal.",i),output)
  rm(means,sds,draw.mats,output,iopac.costs,data,output.per.employee)
}

## Explore output: plots
vessel_output.hists <- list()
vessel_income.hists <- list()
vessel_employment.hists <- list()
ct <- 1
for(i in c("normal.","lognormal.")){
  for(j in 2018:2021){
    data <- get(paste0("output.",i,j))
    vessel_output <- lapply(data, function(x) x%>% select(Vessel_output))
    vessel_income <- lapply(data, function(x) x%>% select(Vessel_income))
    vessel_employment <- lapply(data, function(x) x%>% select(Vessel_employment))
    vessel_output <- unlist(vessel_output)
    vessel_income <- unlist(vessel_income)
    vessel_employment <- unlist(vessel_employment)
    vessel_output <- vessel_output[which(vessel_output>=quantile(vessel_output,probs=0.005,na.rm=T)&vessel_output<=quantile(vessel_output,probs=0.995,na.rm=T))]
    vessel_income <- vessel_income[which(vessel_income>=quantile(vessel_income,probs=0.005,na.rm=T)&vessel_income<=quantile(vessel_income,probs=0.995,na.rm=T))]
    vessel_employment <- vessel_employment[which(vessel_employment>=quantile(vessel_employment,probs=0.005,na.rm=T)&vessel_employment<=quantile(vessel_employment,probs=0.995,na.rm=T))]
  
    vessel_output.hists[[ct]] <- hist(vessel_output,plot=F)
    vessel_income.hists[[ct]] <- hist(vessel_income,plot=F)
    vessel_employment.hists[[ct]] <- hist(vessel_employment,plot=F)
    rm(vessel_output,vessel_income,vessel_employment)
    ct <- ct+1
  }
}

plot(vessel_output.hists[[1]],xlab="Vessel Output, 2018; Normal",main="");abline(v=1,lty=2)
plot(vessel_output.hists[[2]],xlab="Vessel Output, 2018; Lognormal",main="");abline(v=1,lty=2)
plot(vessel_output.hists[[3]],xlab="Vessel Output, 2019; Normal",main="");abline(v=1,lty=2)
plot(vessel_output.hists[[4]],xlab="Vessel Output, 2019; Lognormal",main="");abline(v=1,lty=2)
plot(vessel_output.hists[[5]],xlab="Vessel Output, 2020; Normal",main="");abline(v=1,lty=2)
plot(vessel_output.hists[[6]],xlab="Vessel Output, 2020; Lognormal",main="");abline(v=1,lty=2)
plot(vessel_output.hists[[7]],xlab="Vessel Output, 2021; Normal",main="");abline(v=1,lty=2)
plot(vessel_output.hists[[8]],xlab="Vessel Output, 2021; Lognormal",main="");abline(v=1,lty=2)

plot(vessel_income.hists[[1]],xlab="Vessel Income, 2018; Normal",main="");abline(v=0,lty=2)
plot(vessel_income.hists[[2]],xlab="Vessel Income, 2018; Lognormal",main="");abline(v=0,lty=2)
plot(vessel_income.hists[[3]],xlab="Vessel Income, 2019; Normal",main="");abline(v=0,lty=2)
plot(vessel_income.hists[[4]],xlab="Vessel Income, 2019; Lognormal",main="");abline(v=0,lty=2)
plot(vessel_income.hists[[5]],xlab="Vessel Income, 2020; Normal",main="");abline(v=0,lty=2)
plot(vessel_income.hists[[6]],xlab="Vessel Income, 2020; Lognormal",main="");abline(v=0,lty=2)
plot(vessel_income.hists[[7]],xlab="Vessel Income, 2021; Normal",main="");abline(v=0,lty=2)
plot(vessel_income.hists[[8]],xlab="Vessel Income, 2021; Lognormal",main="");abline(v=0,lty=2)

plot(vessel_employment.hists[[1]],xlab="Vessel Employment, 2018; Normal",main="");abline(v=1,lty=2)
plot(vessel_employment.hists[[2]],xlab="Vessel Employment, 2018; Lognormal",main="");abline(v=1,lty=2)
plot(vessel_employment.hists[[3]],xlab="Vessel Employment, 2019; Normal",main="");abline(v=1,lty=2)
plot(vessel_employment.hists[[4]],xlab="Vessel Employment, 2019; Lognormal",main="");abline(v=1,lty=2)
plot(vessel_employment.hists[[5]],xlab="Vessel Employment, 2020; Normal",main="");abline(v=1,lty=2)
plot(vessel_employment.hists[[6]],xlab="Vessel Employment, 2020; Lognormal",main="");abline(v=1,lty=2)
plot(vessel_employment.hists[[7]],xlab="Vessel Employment, 2021; Normal",main="");abline(v=1,lty=2)
plot(vessel_employment.hists[[8]],xlab="Vessel Employment, 2021; Lognormal",main="");abline(v=1,lty=2)


## Explore output: ANOVAS
source("/Users/scheld/Desktop/Current Research/NWFSC EIA/analysis/mults_summary.R")
summary.output.normal <- list()
summary.income.normal <- list()
summary.employment.normal <- list()
summary.output.lognormal <- list()
summary.income.lognormal <- list()
summary.employment.lognormal <- list()
for(i in 2018:2021){
  base.comp <- iopac_wrap(costfin = get(paste0("costflist_",i)), ticsin = get(paste0("ticslist_",i)), markupsin = get(paste0("markups_",i)))
  
  summary.output.normal[[i-2017]] <- mults_summary(get(paste0("output.normal.",i)),"Vessel_output",base.comp)
  summary.output.normal[[i-2017]]$year <- i
  summary.output.normal[[i-2017]]$draws <- "normal"
  summary.income.normal[[i-2017]] <- mults_summary(get(paste0("output.normal.",i)),"Vessel_income",base.comp)
  summary.income.normal[[i-2017]]$year <- i
  summary.income.normal[[i-2017]]$draws <- "normal"
  summary.employment.normal[[i-2017]] <- mults_summary(get(paste0("output.normal.",i)),"Vessel_employment",base.comp)
  summary.employment.normal[[i-2017]]$year <- i
  summary.employment.normal[[i-2017]]$draws <- "normal"
  
  summary.output.lognormal[[i-2017]] <- mults_summary(get(paste0("output.lognormal.",i)),"Vessel_output",base.comp)
  summary.output.lognormal[[i-2017]]$year <- i
  summary.output.lognormal[[i-2017]]$draws <- "lognormal"
  summary.income.lognormal[[i-2017]] <- mults_summary(get(paste0("output.lognormal.",i)),"Vessel_income",base.comp)
  summary.income.lognormal[[i-2017]]$year <- i
  summary.income.lognormal[[i-2017]]$draws <- "lognormal"
  summary.employment.lognormal[[i-2017]] <- mults_summary(get(paste0("output.lognormal.",i)),"Vessel_employment",base.comp)
  summary.employment.lognormal[[i-2017]]$year <- i
  summary.employment.lognormal[[i-2017]]$draws <- "lognormal"
}

data.output.normal <- rbind(summary.output.normal[[1]],summary.output.normal[[2]],summary.output.normal[[3]],summary.output.normal[[4]])
data.output.lognormal <- rbind(summary.output.lognormal[[1]],summary.output.lognormal[[2]],summary.output.lognormal[[3]],summary.output.lognormal[[4]])
data.output <- rbind(data.output.normal,data.output.lognormal)

data.income.normal <- rbind(summary.income.normal[[1]],summary.income.normal[[2]],summary.income.normal[[3]],summary.income.normal[[4]])
data.income.lognormal <- rbind(summary.income.lognormal[[1]],summary.income.lognormal[[2]],summary.income.lognormal[[3]],summary.income.lognormal[[4]])
data.income <- rbind(data.income.normal,data.income.lognormal)

data.employment.normal <- rbind(summary.employment.normal[[1]],summary.employment.normal[[2]],summary.employment.normal[[3]],summary.employment.normal[[4]])
data.employment.lognormal <- rbind(summary.employment.lognormal[[1]],summary.employment.lognormal[[2]],summary.employment.lognormal[[3]],summary.employment.lognormal[[4]])
data.employment <- rbind(data.employment.normal,data.employment.lognormal)

# Descriptive analysis
hist(data.output.normal$med)
hist(data.output.lognormal$med)
summary(lm(med ~ Region + Name + factor(year) + factor(draws), data.output))
summary(lm(cv ~ Region + Name + factor(year) + factor(draws), data.output))
# Normal draws w/ zero truncation have higher median values & more variability (output)

hist(data.income.normal$med)
hist(data.income.lognormal$med)
summary(lm(med ~ Region + Name + factor(year) + factor(draws), data.income))
summary(lm(cv ~ Region + Name + factor(year) + factor(draws), data.income))
# Normal draws w/ zero truncation have lower median values (income)

hist(data.employment.normal$med)
hist(data.employment.lognormal$med)
summary(lm(med ~ Region + Name + factor(year) + factor(draws), data.employment))
summary(lm(cv ~ Region + Name + factor(year) + factor(draws), data.employment))
# Normal draws w/ zero truncation have higher median values & more variability (employment)
