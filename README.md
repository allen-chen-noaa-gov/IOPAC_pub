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
cd data
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
    ## 1 Astoria         Whiting, At Sea    529           NaN           NaN
    ## 2 Astoria          Whiting, Trawl    530      1.655076     0.8249935
    ## 3 Astoria     Whiting, Fixed Gear    531           NaN           NaN
    ## 4 Astoria        Sablefish, Trawl    532      1.703540     0.8918385
    ## 5 Astoria   Sablefish, Fixed Gear    533      1.630345     0.9718993
    ## 6 Astoria Dover/Thornyhead, Trawl    534      1.705272     0.8982394
    ##   Vessel_employment Processor_output Processor_income Processor_employment
    ## 1               NaN              NaN              NaN                  NaN
    ## 2      7.321257e-06         7.974554         2.790273         1.150247e-04
    ## 3               NaN              NaN              NaN                  NaN
    ## 4      8.396983e-06         3.178835         1.112265         4.585141e-05
    ## 5      2.104503e-05         3.178835         1.112265         4.585141e-05
    ## 6      8.491918e-06         3.387614         1.185316         4.886284e-05
    ##     TotOut   TotInc       TotEmp
    ## 1      NaN      NaN          NaN
    ## 2 9.629630 3.615267 1.223460e-04
    ## 3      NaN      NaN          NaN
    ## 4 4.882375 2.004103 5.424839e-05
    ## 5 4.809180 2.084164 6.689643e-05
    ## 6 5.092887 2.083556 5.735476e-05

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

    ## [1] 2.888361

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
    ## 1             0.3860072          1.081268e-06             0.2955617
    ##   MS_pounds_employ_mult
    ## 1          1.751114e-05
