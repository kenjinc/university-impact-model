Data Cleaning and Aggregation
================
Last updated: April 19, 2024

## Required Packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.1      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggpubr)
```

## Data Loading

Because the shape-file data we will be using is local to the `tidyverse`
collection, we only need to load in the two remaining data sources.

Here, we assign the dietary-footprint and university-enrollment data as
objects within our environment by reading in the two data files that
were previously loaded into the `parent-files` folder of our repository.

They include the publication data from Kim and colleagues (2021), which
was retrieved via the Digital Commons using this [project
link](https://data.mendeley.com/datasets/g8n8w8snmj/3), and the
user-generated indicator report from the [EdStats
database](https://databank.worldbank.org/reports.aspx?source=Education%20Statistics).

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/dietary_footprints_by_country.csv")
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/education_data_by_country.csv")
```

## Adjusting the Shape-File Data

Before we can consolidate the relevant variables from the two parent
versions of the data files loaded in above, we will first need to update
the shape-file data local to the `maps` package within the `tidyverse`
collection.

``` r
shapefile_data <- map_data("world")
shapefile_data %>% head(6)
```

    ##        long      lat group order region subregion
    ## 1 -69.89912 12.45200     1     1  Aruba      <NA>
    ## 2 -69.89571 12.42300     1     2  Aruba      <NA>
    ## 3 -69.94219 12.43853     1     3  Aruba      <NA>
    ## 4 -70.00415 12.50049     1     4  Aruba      <NA>
    ## 5 -70.06612 12.54697     1     5  Aruba      <NA>
    ## 6 -70.05088 12.59707     1     6  Aruba      <NA>

More specifically, we will need to align the directory of nation-states
in this package to the conventions outlined by the [International
Organizaation for Standardization
(ISO)](https://www.iso.org/iso-3166-country-codes.html).

``` r
impact_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/dietary_footprints_by_country.csv")
dietary_footprint_data %>% head(6)
```

    ##   country_code country      diet                 attribute centile_up
    ## 1            1 Armenia 2/3_vegan      kg_co2_luc_feed_palm     0.0000
    ## 2            1 Armenia 2/3_vegan       kg_co2_luc_feed_soy     0.0000
    ## 3            1 Armenia 2/3_vegan kg_co2_luc_human_palm_soy     0.0000
    ## 4            1 Armenia 2/3_vegan        kg_co2_luc_pasture     0.0000
    ## 5            1 Armenia 2/3_vegan          kg_co2e_excl_luc   289.7267
    ## 6            1 Armenia 2/3_vegan             kg_co2e_total   289.7267
    ##         value centile_down value_baseline diff_baseline X._diff_baseline
    ## 1   0.3180732         0.00      0.9548163 -6.367431e-01    -0.6668749579
    ## 2  12.2508407         0.00     36.7755021 -2.452466e+01    -0.6668749579
    ## 3   0.2436078         0.00      0.2437601 -1.523193e-04    -0.0006248737
    ## 4   1.1439146         0.00      3.4338897 -2.289975e+00    -0.6668749579
    ## 5 784.8360925       136.86   1604.2125238 -8.193764e+02    -0.5107655121
    ## 6 798.7925288       136.86   1645.6204919 -8.468280e+02    -0.5145949308
    ##   value_baseline_adj diff_baseline_adj X._diff_baseline_adj
    ## 1          0.9542196        -0.6361464           -0.6666667
    ## 2         36.7525221       -24.5016814           -0.6666667
    ## 3          0.2436078         0.0000000            0.0000000
    ## 4          3.4317439        -2.2878293           -0.6666667
    ## 5       1603.2100936      -818.3740011           -0.5104596
    ## 6       1644.5921870      -845.7996582           -0.5142914

``` r
write.csv(impact_data,"~/github/university-impact-model/data/model-output/impact-data.csv")
```
