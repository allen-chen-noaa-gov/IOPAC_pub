<!-- README.md is generated from README.Rmd. Please edit that file -->

# IOPAC

This is a repository containing the Input-Output model for Pacific Coast
Fisheries (IOPAC), created by [Jerry
Leonard](https://github.com/allen-chen-noaa-gov/IOPAC_pub/blob/main/inst/leonard_TM.pdf).
This model estimates gross changes in economic contributions due to
changes in fishery harvests, for example from environmental or policy
changes. This readme documents the repository and provides a minimal
reproducible example.

## Data submodule

Note that the data folder is in a private repository. Please contact
<allen.chen@noaa.gov> for permissions. To push and pull from the data
submodule, you can first work inside data submodule:

``` git
cd data # nolint: error.
# make changes, add, commit, push
git add .
git commit -m "Your message for data submodule"
git push
```

Then update the submodule reference in your main repo:

``` git
cd ..
git add data
git commit -m "Update data submodule reference"
git push
```

## Running the model

Running the model can be done through the high-level wrapper function
`iopac_wrap`. Installing the package requires `devtools`.

``` r
library(devtools)
library(here)
here()
#The working directory should be the top level of the package. Go up one to install.
install(here("..", "IOPAC_pub"))
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
#Note package name is IOPAC
library(IOPAC)
library(ggplot2)

costflist_2023 <- costflist_template

costflist_2023$vessel <- clean_cost_data(functype = "vessel")

costflist_2023$processor <- clean_cost_data(sums = costf_P_list[["y2023"]],
  functype = "processor")

multres <- iopac_wrap(costfin = costflist_2023)
head(multres)
```

    ##    Region                    Name Sector Vessel_output Vessel_income
    ## 1 Astoria         Whiting, At Sea    529      0.000000     0.0000000
    ## 2 Astoria          Whiting, Trawl    530      1.669010     0.8102007
    ## 3 Astoria     Whiting, Fixed Gear    531      0.000000     0.0000000
    ## 4 Astoria        Sablefish, Trawl    532      1.703540     0.8918385
    ## 5 Astoria   Sablefish, Fixed Gear    533      1.735935     0.8651723
    ## 6 Astoria Dover/Thornyhead, Trawl    534      1.705272     0.8982394
    ##   Vessel_employment Processor_output Processor_income Processor_employment
    ## 1      0.000000e+00         0.000000        0.0000000         0.000000e+00
    ## 2      7.367387e-06         4.355045        1.2202148         1.563905e-05
    ## 3      0.000000e+00         0.000000        0.0000000         0.000000e+00
    ## 4      8.396986e-06         1.736018        0.4864047         6.234073e-06
    ## 5      2.140190e-05         1.736018        0.4864047         6.234073e-06
    ## 6      8.491922e-06         1.850036        0.5183508         6.643515e-06
    ##     TotOut   TotInc       TotEmp
    ## 1 0.000000 0.000000 0.000000e+00
    ## 2 6.024055 2.030415 2.300644e-05
    ## 3 0.000000 0.000000 0.000000e+00
    ## 4 3.439558 1.378243 1.463106e-05
    ## 5 3.471953 1.351577 2.763597e-05
    ## 6 3.555308 1.416590 1.513544e-05

There is also a bounds function, depicting the central tendency and
uncertainty of the multipliers. The bounds are calculated using the
2.5th, 50th, and 97.5th percentiles of the multipliers, which can be
used to visualize bounds for the multipliers. An example for specific
regions and sectors is shown below.

``` r
multbounds <- make_mult_bounds()
plotres <- multbounds[multbounds$Region %in% c("WC", "Oregon",
  "Newport"), ]
plotres <- plotres[plotres$Name %in% c("Whiting, Trawl",
  "Sablefish, Trawl", "Crab, Fixed Gear", "Salmon, Fixed Gear"), ]

cols_to_check <- c("Perc_025", "Perc_500", "Perc_975")

plotres <- plotres[rowSums(plotres[, cols_to_check] == 0) == 0, ]

ggplot(plotres, aes(x = Name, y = Perc_500, color = Region)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Perc_025, ymax = Perc_975), width = 0.2,
    linetype = "dashed", position=position_dodge(width=0.5)) +
  facet_wrap(~MultType, scales = "free") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Multipliers by Region and Sector",
    x = "Sector",
    y = "Multiplier") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```

![](man/figures/README-unnamed-chunk-5-1.png)

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

The individual `make_v_cat_mults` and `make_p_cat_mults` functions can
also be used to create commodity-level multipliers for vessels and
processors respectively, which can be used to understand the
contributions of specific commodities to the overall multipliers:

``` r
catsv <- make_v_cat_mults(impbridge = impbridgelist,
  costf = costflist_2023,
  mults = mults,
  type = "Output",
  sector = "Oregon",
  ticsin = tics_list$y2023,
  ecpi = ecpi,
  taxes = taxes,
  comnamesin = comnames,
  output = NULL,
  zeroprop = TRUE,
  zeropropdir = FALSE)
head(catsv, n = c(6, 3))
```

    ##                                 3002         3003         3004
    ## Whiting, At Sea                   NA           NA           NA
    ## Whiting, Trawl          2.302501e-06 7.564304e-05 8.220881e-05
    ## Whiting, Fixed Gear               NA           NA           NA
    ## Sablefish, Trawl        2.391489e-06 7.856653e-05 8.538605e-05
    ## Sablefish, Fixed Gear   4.798651e-06 1.576479e-04 1.713317e-04
    ## Dover/Thornyhead, Trawl 2.405043e-06 7.901181e-05 8.586999e-05

``` r
catsp <- make_p_cat_mults(impbridge = impbridgelist,
  costf = costflist_2023,
  mults = mults,
  type = "Output",
  sector = "Oregon",
  ticsin = tics_list$y2023,
  ecpi = ecpi,
  taxes = taxes,
  comnamesin = comnames,
  prodflow = prodflow,
  fskey = fskey,
  markups = markups_list$y2023)
head(catsp, n = c(6, 3))
```

    ##                                 3014 3017 3021
    ## Whiting, At Sea         0.000000e+00    0    0
    ## Whiting, Trawl          1.278451e-04    0    0
    ## Whiting, Fixed Gear     0.000000e+00    0    0
    ## Sablefish, Trawl        5.096191e-05    0    0
    ## Sablefish, Fixed Gear   5.096191e-05    0    0
    ## Dover/Thornyhead, Trawl 5.430899e-05    0    0

The package also includes a number of data sets that can produce
multipliers for the West Coast region, and documentation for all
included data can also be found similarily (typing ?data). The default
data used are the most most recent, but the years 2017 through 2023 are
included for cost, markup, and fish ticket data where available.

``` r
?prodflow
```

Therefore the user can create multipliers calibrating the model with
costs or catches from different years, or use their own data if desired,
as long as the data matches the same format. For example, calibrating
the model with cost, fish ticket, and processor markup data from 2017
below.

``` r
costflist_2017 <- costflist_template
costflist_2017$vessel <- clean_cost_data(sums = costf_V_list[["y2017"]],
  functype = "vessel")
costflist_2017$processor <- clean_cost_data(sums = costf_P_list[["y2017"]],
  functype = "processor")

multres <- iopac_wrap(costfin = costflist_2017, ticsin = tics_list$y2017,
    markupsin = markups_list$y2017)

subset(multres, Region == 'WC' & Name == 'Whiting, Trawl')$Processor_income
```

    ## [1] 1.660434

## Recreational multipliers

Recreational multipliers can now also be called within the package. Note
that while the commercial multipliers are contributions per dollar of
catch revenue, the recreational multipliers are dollars per trip.

``` r
head(make_rec())
```

    ##              Region State TripType   Output   Income  Employment
    ## 1 California Whole     CA      PRI 387.9740 181.3116 0.001501865
    ## 2   NCC MendSonoma     CA      PRI 226.4653 111.4851 0.001297157
    ## 3      NCC SanFran     CA      PRI 294.6259 145.8750 0.001030079
    ## 4      North Coast     CA      PRI 212.9839 102.8976 0.001287104
    ## 5              SCC     CA      PRI 253.7623 122.7201 0.001327235
    ## 6      South Coast     CA      PRI 366.2508 170.3181 0.001474214

## At-sea multipliers

At-sea multipliers can now also be called within the package. Note that
the at-sea multipliers are contributions per pound of catch, different
than the previous outputs.

``` r
make_atsea(costfin = costflist_2023)
```

    ##   CP_pounds_income_mult CP_pounds_employ_mult MS_pounds_income_mult
    ## 1             0.3860072           1.08127e-06             0.2955617
    ##   MS_pounds_employ_mult
    ## 1          1.751115e-05

## DISCLAIMER

“This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.”
