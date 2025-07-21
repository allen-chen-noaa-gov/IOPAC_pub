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
    ## 1 Astoria         Whiting, At Sea    529      0.000000     0.0000000
    ## 2 Astoria          Whiting, Trawl    530      1.662108     0.8278427
    ## 3 Astoria     Whiting, Fixed Gear    531      0.000000     0.0000000
    ## 4 Astoria        Sablefish, Trawl    532      1.707345     0.8933801
    ## 5 Astoria   Sablefish, Fixed Gear    533      1.631968     0.9725567
    ## 6 Astoria Dover/Thornyhead, Trawl    534      1.708839     0.8996846
    ##   Vessel_employment Processor_output Processor_income Processor_employment
    ## 1      0.000000e+00              NaN              NaN                  NaN
    ## 2      7.363272e-06         7.974554         2.790273         1.150247e-04
    ## 3      0.000000e+00              NaN              NaN                  NaN
    ## 4      8.419717e-06         3.178835         1.112265         4.585141e-05
    ## 5      2.105472e-05         3.178835         1.112265         4.585141e-05
    ## 6      8.513231e-06         3.387614         1.185316         4.886284e-05
    ##     TotOut   TotInc       TotEmp
    ## 1      NaN      NaN          NaN
    ## 2 9.636662 3.618116 1.223880e-04
    ## 3      NaN      NaN          NaN
    ## 4 4.886179 2.005645 5.427112e-05
    ## 5 4.810802 2.084822 6.690613e-05
    ## 6 5.096454 2.085001 5.737607e-05

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
make_rec()
```

    ##                     Region State TripType    Output    Income   Employment
    ## 1        California Whole     CA      PRI  391.8962 183.36900 0.0015324098
    ## 2          NCC MendSonoma     CA      PRI  225.9484 111.17791 0.0013148139
    ## 3             NCC SanFran     CA      PRI  296.4262 146.82627 0.0010485342
    ## 4             North Coast     CA      PRI  212.0076 102.40103 0.0013022233
    ## 5                     SCC     CA      PRI  254.1711 122.87852 0.0013468058
    ## 6             South Coast     CA      PRI  367.6591 170.98867 0.0014970812
    ## 7                 Astoria     OR      PRI  243.6394 128.68060 0.0017037278
    ## 8               Brookings     OR      PRI  224.0572  97.64068 0.0018908185
    ## 9                Coos Bay     OR      PRI  284.0518 141.54578 0.0019941460
    ## 10                Newport     OR      PRI  210.5362  99.79653 0.0017958974
    ## 11           Oregon Whole     OR      PRI  294.3288 155.33293 0.0019596400
    ## 12              Tillamook     OR      PRI  233.5345 110.03726 0.0017449388
    ## 13 North Washington Coast     WA      PRI  164.7069  86.36141 0.0010724934
    ## 14            Puget Sound     WA      PRI  341.0212 167.91170 0.0011626563
    ## 15 South Washington Coast     WA      PRI  188.6170  95.31901 0.0009755918
    ## 16       Washington Whole     WA      PRI  345.4777 164.88984 0.0012203439
    ## 17       California Whole     CA       FH  911.9700 408.21694 0.0055984149
    ## 18         NCC MendSonoma     CA       FH  821.4397 365.13457 0.0054516733
    ## 19            NCC SanFran     CA       FH  852.8501 386.24346 0.0052615416
    ## 20            North Coast     CA       FH  812.1951 356.20438 0.0054301091
    ## 21                    SCC     CA       FH  830.9278 366.91158 0.0054754541
    ## 22            South Coast     CA       FH  895.8375 400.29496 0.0055781658
    ## 23                Astoria     OR       FH 1117.8724 589.10169 0.0115814517
    ## 24              Brookings     OR       FH 1091.2710 542.10505 0.0119537133
    ## 25               Coos Bay     OR       FH 1193.0169 611.77733 0.0121596243
    ## 26                Newport     OR       FH 1094.6659 554.40527 0.0117735668
    ## 27           Oregon Whole     OR       FH 1227.0460 643.78418 0.0121179941
    ## 28              Tillamook     OR       FH 1100.7517 559.26805 0.0117268534
    ## 29 North Washington Coast     WA       FH 1398.3154 683.42176 0.0139047432
    ## 30            Puget Sound     WA       FH 1599.1417 837.06613 0.0138287060
    ## 31 South Washington Coast     WA       FH 1404.4821 703.86734 0.0136355197
    ## 32       Washington Whole     WA       FH 1605.7880 827.09760 0.0139995778
