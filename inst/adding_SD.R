setwd()
library(IOPAC)
library(tidyverse)
#datadir <- file.path("..", "sddata_costs")
#file_list <- list.files(datadir)

temp <- setwd() 
temp = list.files(pattern="*.csv")
files<- lapply(temp, read_csv)
Filenames<- gsub(".csv","",as.character(temp))
names(files) <- Filenames

# note also load IOPAC_data - needed for IOPAC_wrap (e.g., costflist_20XX, mults, etc)
###################################################
# COSTS and SD
# Put all files in same format as previously used costs (costflist):
# can all of this be put into 1 function? (if so how?

files2<- lapply(files, function(x){
  data.frame(t(as.matrix(x))) 

})

 files2<-  lapply(files2, function(x){
   x[-c(1,2), ] 
 })
 
 files2<-  lapply(files2, function(x){
   x<- x %>% 
     mutate(Cost= rownames(x), Type=rownames(x))
 })

 files2<-  lapply(files2, function(x){
   x<- x%>% 
     relocate(Cost, .before=X1) %>% 
     remove_rownames() 
 })
 
files2<-  lapply(files2, function(x){
  x %>% 
    mutate(across(-c('Cost', 'Type'), as.numeric))
})
 
colnames<- colnames(costflist_2022[[1]]) 
files2<- lapply(files2, setNames, colnames)

# IOPAC for vessels:
# using newest data (sddata_costs)
# can only calculate 2018:2021 because of the other files needed by IOPAC wrap- however, we also have 2017 data here
###############################################################################################################################################
###############################################################################################################################################
# 2018
proc_2018<- costflist_2018$processor
proc_2018<- replace(proc_2018,3:11, 0)

cost_list_2018<- list()
cost_list_2018[["vessel"]]<- files2$costf_2018
cost_list_2018[["processor"]]<- proc_2018

cost_list_2018[["vessel"]][is.na(cost_list_2018[["vessel"]])]<- 0
cost_list_2018$vessel[nrow(cost_list_2018$vessel) + 1,] = c(0)

cost_list_2018$vessel$Cost<-costflist_2018$vessel$Cost
cost_list_2018$vessel$Type<-costflist_2018$vessel$Type

sd18<- files2$costf_sd_2018
sd18[is.na(sd18)]<- 0 # the NA's in sd was throwing off multiplication in IOPAC wrap

n   = 22
k   = 18
mu2018    = cost_list_2018$vessel[2:19]
sigma2018 = sd18[2:19]
rep     = matrix(data=NA, nrow=n, ncol=k)

output.list2018T <- list()
reps <- 1000
for(z in 1:reps){
  for(j in 1:k){
    for(i in 1:n){
      rep[i,j] = rnorm(1, mu2018[i,j], sigma2018[i,j])
      data2018<- cost_list_2018
      data2018$vessel[2:18]<- rep
    }
  }
  output.list2018T[[z]]<- iopac_wrap(costfin = data2018, ticsin = ticslist_2018, markupsin = markups_2018) # change data when year is specified
}


Base_2018<- iopac_wrap(costfin = cost_list_2018,ticsin = ticslist_2018, markupsin = markups_2018)

###############################################################################################################################################
###############################################################################################################################################
# 2019

proc_2019<- costflist_2019$processor
proc_2019<- replace(proc_2019,3:11, 0)

cost_list_2019<- list()
cost_list_2019[["vessel"]]<- files2$costf_2019
cost_list_2019[["processor"]]<- proc_2019

cost_list_2019[["vessel"]][is.na(cost_list_2019[["vessel"]])]<- 0
cost_list_2019$vessel[nrow(cost_list_2019$vessel) + 1,] = c(0)

cost_list_2019$vessel$Cost<-costflist_2019$vessel$Cost
cost_list_2019$vessel$Type<-costflist_2019$vessel$Type

sd19<- files2$costf_sd_2019
sd19[is.na(sd19)]<- 0 # the NA's in sd was throwing off multiplication in IOPAC wrap

n   = 22
k   = 18
mu2019   = cost_list_2019$vessel[2:19]
sigma2019 = sd19[2:19]
rep     = matrix(data=NA, nrow=n, ncol=k)


output.list2019T <- list()
reps <- 1000
for(z in 1:reps){
  for(j in 1:k){
    for(i in 1:n){
      rep[i,j] = rnorm(1, mu2019[i,j], sigma2019[i,j])
      data2019<- cost_list_2019
      data2019$vessel[2:18]<- rep
    }
  }
  output.list2019T[[z]]<- iopac_wrap(costfin = data2019, ticsin = ticslist_2019, markupsin = markups_2019) # change data when year is specified
}


Base_2019<- iopac_wrap(costfin = cost_list_2019,ticsin = ticslist_2019, markupsin = markups_2019)


###############################################################################################################################################
###############################################################################################################################################
# 2020
proc_2020<- costflist_2020$processor
proc_2020<- replace(proc_2020,3:11, 0)

cost_list_2020<- list()
cost_list_2020[["vessel"]]<- files2$costf_2020
cost_list_2020[["processor"]]<- proc_2020

cost_list_2020[["vessel"]][is.na(cost_list_2020[["vessel"]])]<- 0
cost_list_2020$vessel[nrow(cost_list_2020$vessel) + 1,] = c(0)

cost_list_2020$vessel$Cost<-costflist_2020$vessel$Cost
cost_list_2020$vessel$Type<-costflist_2020$vessel$Type

sd20<- files2$costf_sd_2020
sd20[is.na(sd20)]<- 0 # the NA's in sd was throwing off multiplication in IOPAC wrap

n   = 22
k   = 18
mu2020    = cost_list_2020$vessel[2:19]
sigma2020 = sd20[2:19]
rep     = matrix(data=NA, nrow=n, ncol=k)

output.list2020T <- list()
reps <- 1000
for(z in 1:reps){
  for(j in 1:k){
    for(i in 1:n){
      rep[i,j] = rnorm(1, mu2020[i,j], sigma2020[i,j])
      data2020<- cost_list_2020
      data2020$vessel[2:18]<- rep
    }
  }
  output.list2020T[[z]]<- iopac_wrap(costfin = data2020, ticsin = ticslist_2020, markupsin = markups_2020) # change data when year is specified
}


Base_2020<- iopac_wrap(costfin = cost_list_2020, ticsin = ticslist_2020, markupsin = markups_2020)


###############################################################################################################################################
#### 2021
# make own version of costf_list with vessel & processor data
proc_2021<- costflist_2021$processor
proc_2021<- replace(proc_2021,3:11, 0)

cost_list_2021<- list()
cost_list_2021[["vessel"]]<- files2$costf_2021
cost_list_2021[["processor"]]<- proc_2021

cost_list_2021[["vessel"]][is.na(cost_list_2021[["vessel"]])]<- 0
cost_list_2021$vessel[nrow(cost_list_2021$vessel) + 1,] = c(0)

cost_list_2021$vessel$Cost<-costflist_2021$vessel$Cost
cost_list_2021$vessel$Type<-costflist_2021$vessel$Type

sd21<- files2$costf_sd_2021
sd21[is.na(sd21)]<- 0 # the NA's in sd was throwing off multiplication in IOPAC wrap

n   = 22
k   = 18
mu2021    = cost_list_2021$vessel[2:19]
sigma2021 = sd21[2:19]
rep     = matrix(data=NA, nrow=n, ncol=k)

output.list2021T <- list()
reps <- 1000
for(z in 1:reps){
  for(j in 1:k){
    for(i in 1:n){
      rep[i,j] = rnorm(1, mu2021[i,j], sigma2021[i,j])
      data2021<- cost_list_2021
      data2021$vessel[2:18]<- rep
    }
  }
  output.list2021T[[z]]<- iopac_wrap(costfin = data2021, ticsin = ticslist_2021, markupsin = markups_2021) # change data when year is specified
}

Base_2021<- iopac_wrap(costfin = cost_list_2021, ticsin = ticslist_2021, markupsin = markups_2021)

##########################################
##########################################

# call mults_summary, which takes as input three arguments: 
# 1) a list of multiplier outputs created through multiple calls of iopac_wrap, where each list element is associated with a call of the iopac_wrap function
# 2) the multipliers that should be summarized, from among: Vessel_output, Vessel_income, Vessel_employment, Processor_output, Processor_income, Processor_employment, TotOut, TotInc, and TotEmp
# 3) a base run of iopac_wrap based on average values without consideration of variability

#source() # call mults_summary function

# issues with NA values - replace ALL NA with 0
#output.list20187<- lapply(output.list2017T, function(x) replace(x, is.na(x), 0))
output.list2018T<- lapply(output.list2018T, function(x) replace(x, is.na(x), 0))
output.list2019T<- lapply(output.list2019T, function(x) replace(x, is.na(x), 0))
output.list2020T<- lapply(output.list2020T, function(x) replace(x, is.na(x), 0))
output.list2021T<- lapply(output.list2021T, function(x) replace(x, is.na(x), 0))

#Base_2017[is.na(Base_2017)]<- 0
Base_2018[is.na(Base_2018)]<- 0
Base_2019[is.na(Base_2019)]<- 0
Base_2020[is.na(Base_2020)]<- 0
Base_2021[is.na(Base_2021)]<- 0


#mults.out2017 <- mults_summary(output.list2017T,"Vessel_output",Base_2017)
mults.out2018 <- mults_summary(output.list2018T,"Vessel_output",Base_2018)
mults.out2019 <- mults_summary(output.list2019T,"Vessel_output",Base_2019)
mults.out2020 <- mults_summary(output.list2020T,"Vessel_output",Base_2020)
mults.out2021 <- mults_summary(output.list2021T,"Vessel_output",Base_2021)

#head(mults.out2017)
head(mults.out2018)
head(mults.out2019)
head(mults.out2020)
head(mults.out2021) 

# things work but why does 2021 have some rows of NA whereas others dont- same holds true using iopac with base levels
# what is driving this issue ^^^^ (if it is an issue- all should be same)
    # what numbers are the other variables in IOPAC_wrap and is this what is driving the difference? - maybe 2021 is different than other years?

# mults_summary returns lower (upper) 95% distributional bounds, coefficient of variation, and the difference between the estimate with no variability and the median of estimate when incorporating variability

# summary stats:
summary(mults.out2018)
summary(mults.out2019)
summary(mults.out2020)
summary(mults.out2021)

##############################################################################################################################
##############################################################################################################################
##### 
# EXAMPLE OF NORMALIZED costs (dividing by column sum):
proc_2018<- costflist_2018$processor
proc_2018<- replace(proc_2018,3:11, 0)

cost_list_2018<- list()
cost_list_2018[["vessel"]]<- files2$costf_2018
cost_list_2018[["processor"]]<- proc_2018

cost_list_2018[["vessel"]][is.na(cost_list_2018[["vessel"]])]<- 0
cost_list_2018$vessel[nrow(cost_list_2018$vessel) + 1,] = c(0)

cost_list_2018$vessel$Cost<-costflist_2018$vessel$Cost
cost_list_2018$vessel$Type<-costflist_2018$vessel$Type

sd18<- files2$costf_sd_2018
sd18[is.na(sd18)]<- 0 # the NA's in sd was throwing off multiplication in IOPAC wrap

# NORMALIZE:
# divide each by sum of col
Normalized_mu2018<- as.matrix(apply(cost_list_2018$vessel[, 2:19], 2, function(x) {x/sum(x, na.rm=TRUE)}))
Normalized_sd2018<- as.matrix(apply(sd18[, 2:19], 2, function(x) {x/sum(x, na.rm=TRUE)})) # ALSO Normalized SD here to match the mean values

norm_cost_list2018<- cost_list_2018
norm_cost_list2018$vessel[2:19]<- Normalized_mu2018

norm_cost_list2018[["vessel"]][is.na(norm_cost_list2018[["vessel"]])]<- 0
Normalized_sd2018[is.na(Normalized_sd2018)]<- 0

n   = 22
k   = 18
mu2018    = norm_cost_list2018$vessel[2:19]
sigma2018 = Normalized_sd2018
rep     = matrix(data=NA, nrow=n, ncol=k)

output.list2018N <- list()
reps <- 1000
for(z in 1:reps){
  for(j in 1:k){
    for(i in 1:n){
      rep[i,j] = rnorm(1, mu2018[i,j], sigma2018[i,j])
      data2018<- cost_list_2018
      data2018$vessel[2:18]<- rep
    }
  }
  output.list2018N[[z]]<- iopac_wrap(costfin = data2018, ticsin = ticslist_2018, markupsin = markups_2018) # change data when year is specified
}


##############################################################################################################################
# analysis of normalized values in comparasion to base values (w/out sd)
# New comparison using normalized values:
Base_2018N<- iopac_wrap(costfin = norm_cost_list2018,ticsin = ticslist_2018, markupsin = markups_2018)
mults.out2018N <- mults_summary(output.list2018N,"Vessel_output",Base_2018N)

head(mults.out2018N)

summary(Base_2018N) 
summary(mults.out2018N) 
# issues is when using sd's, base seems to have correct bound whereas with sd there is issues where vessel output < 1 & negative in some cases
# possible because using normal dist -- but mean and median is below 0

summary(Base_2018) # min of 1
summary(Base_2019) # has 0 for output- could be due to a blank row
summary(Base_2020) # min of 1
summary(Base_2021) # min of 0 -- likley blank row


##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


