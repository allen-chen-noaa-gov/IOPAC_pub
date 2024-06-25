<!-- README.md is generated from README.Rmd. Please edit that file -->

# IOPAC

This is a repository containing the Input-Output model for Pacific Coast
Fisheries (IOPAC), created by Jerry Leonard. This model estimates gross
changes in economic contributions and impacts due to changes in fishery
harvests, for example from environmental or policy changes. This readme
documents the repository and provides a minimal reproducible example.

## Running the model

Running the model can be done through the high-level wrapper function
`iopac_wrap`. Installing the package requires `devtools`.

``` r
library(devtools)
library(dplyr)
library(here)
here()
#The working directory should be the top level of the package.
#You can setwd("..") to install it if you want but then it might be a good idea
#to reload the project
#install("IOPAC")

#I'm trying something a little funky here so we can keep the data separate
#(allowing us to push things to a public Git).
datadir <- file.path("..", "IOPAC_data")
file_list <- list.files(datadir)
for (i in 1:length(file_list)){
  load(paste0(file.path("..", "IOPAC_data"), "/",  file_list[i]))
}

#I forgot to create a column when I was repulling the data, please excuse the
#ugly code
costflist_2017$processor$Value <- costflist_2017$processor$Xn2017
costflist_2018$processor$Value <- costflist_2018$processor$Xn2018
costflist_2019$processor$Value <- costflist_2019$processor$Xn2019
costflist_2020$processor$Value <- costflist_2020$processor$Xn2020
costflist_2021$processor$Value <- costflist_2021$processor$Xn2021
costflist_2022$processor$Value <- costflist_2022$processor$Xn2022

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
```

The wrapper function calls on eleven total inputs, two of which are
optional. The output is a (n\*m) by 12 data frame, where the first three
columns correspond to the geographic region, sector, and name of sector
for a multiplier, where there are *n* regions and *m* sectors. Then, the
remaining 9 columns correspond to the output, income, and employment
multipliers for vessels, processors, and their aggregate (total), for
each region-sector. A missing value denotes that there is an absence of
data (e.g. fish tickets) for that region-sector.

``` r
library(IOPAC)
#multres <- iopac_wrap()
#head(multres)
```

The wrapper function creates multipliers for the vessel and processor
through the functions `make_v_mults` and `make_p_mults` respectively.
These can be used to create individual multipliers instead if desired.
Documentation for all functions can be found by typing ?function, for
example:

``` r
?iopac_wrap
```

Therefore, `make_v_mults` and `make_p_mults` create the multipliers,
while `iopac_wrap` passes the relevant data to the functions.

The package also includes a number of data sets that can produce
multipliers for the West Coast region, and documentation for all
included data can also be found similarily (typing ?data). The default
data used are the most most recent, but the years 2018, 2019, and 2020
are included for cost, markup, and fish ticket data where available.

``` r
#?ticslist_2020
```

Therefore the user can create multipliers calibrating the model with
costs or catches from different years, or use their own data if desired,
as long as the data matches the same format. For example, calibrating
the model with cost, fish ticket, and processor markup data from 2019
below.

``` r
#multres <- iopac_wrap(costfin = costflist_2020, ticsin = ticslist_2020, 
#   markupsin = markups_2020)

#subset(multres, Region == 'WC' & Name == 'Whiting, Trawl')$Processor_income
```

# Adding uncertainty

I can think of at least three areas where we have a distribution for the
data but only rely on means - fish tickets, costs, and processor
markups. As an example the costs are currently the proportions each
vessel spends on a category:

``` r
#head(costflist_2020[[1]])
```

These are the averages *n* vessels. We could pull from a normally
distributed random variable with these means and an associated standard
devation? Repeat for some number of iterations and average the results.

Fish ticket data is treated similarly although I think the processing
into proportions is done in make_v\_mults currently. The same intuition
would apply although I’m unsure of the interaction between the two
random variables.

``` r
#head(ticslist_2020[[1]])
```

For both of these we could pull new data in iopac_wrap, and pass the
pulled data into make_v\_mults (instead of the current static data).