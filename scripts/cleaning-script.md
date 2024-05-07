Data Cleaning and Aggregation
================
Last updated: April 29, 2024

## Package Loading

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

There are three principal data inputs needed to generate our impact
model:

- Shape-File Data
- Dietary-Footprint Data
- University-Enrollment Data

The shape-file data we will be using, which is local to the `maps`
package within the `tidyverse` collection, can be written into the
`parent-files` folder of our respository with the following:

``` r
shapefile_data <- map_data("world")
write.csv(shapefile_data,file="/Users/kenjinchang/github/university-impact-model/data/parent-files/shapefile_data_by_country.csv")
```

With the shape-file data now archived, we can move on to loading in the
remaining two data inputs. These include (1) the publication data from
“Country-specific dietary shifts to mitigate climate and water crises”
(Kim et al., 2020), which was retrieved via the Digital Commons using
this [link](https://data.mendeley.com/datasets/g8n8w8snmj/3), and (2)
the customized indicator report created on April 19, 2024 from the
[EdStats
database](https://databank.worldbank.org/reports.aspx?source=Education%20Statistics).

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/dietary_footprints_by_country.csv")
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/education_data_by_country.csv")
```

Before synthesizing the relevant information from these individual
inputs into a single data file, we will first need to make a series of
adjustments in preparation for our spatial join. In the following
sections, we document these steps, their motivations, and the code
chunks used to transform them.

## Adjusting the Shape-File Data

Because the directory of nation-states used in the `maps` package does
not correspond with the country designations used in the remaining two
data sources, we will need to adjust how these areas are geographically
and politically represented within our shape-file data.

To ensure consistency in naming, as well as the spatial boundaries used
to represent these areas, across three leveraged data sources, we will
follow the conventions outlined by the [International Organizaation for
Standardization (ISO)](https://www.iso.org/iso-3166-country-codes.html).

Following the instructions provided by Thomas Haslam in his 2021 [RPubs
entry](https://rpubs.com/Thom_JH/798825), we will begin this process by
first addressing what he refers to as the “easy cases.”

These cases refer to the 13 instances where the names of the
nation-states listed under the `region` variable in the `maps` package
are misaligned with the names of the nation-states listed under the
`country` variable in the `Gapminder` dataset—a commonly used data
source that happens to be synchronized with [ISO-3166 naming
standards](https://www.iso.org/iso-3166-country-codes.html).

``` r
shapefile_data <- shapefile_data %>% 
  rename(country=region) %>%
  mutate(country=case_when(country=="Macedonia"~"North Macedonia",
                           country=="Ivory Coast"~"Cote d'Ivoire",
                           country=="Democratic Republic of the Congo"~"Congo, Dem. Rep.",
                           country=="Republic of Congo"~"Congo, Rep.",
                           country=="UK"~"United Kingdom",
                           country=="USA"~"United States",
                           country=="Laos"~"Lao",
                           country=="Slovakia"~"Slovak Republic",
                           country=="Saint Lucia"~"St. Lucia",
                           country=="Kyrgyzstan"~"Krygyz Republic",
                           country=="Micronesia"~"Micronesia, Fed. Sts.",
                           country=="Swaziland"~"Eswatini",
                           country=="Virgin Islands"~"Virgin Islands (U.S.)",
                        TRUE~country))
```

For the next group of cases, which Haslam refers to as the “island
nations,” we will need to aggregate the previously separated regions of
Antigua and Barbuda, St. Kitts and Nevis, Trinidad and Tobago, and
St. Vincent and the Grenadines using the following:

``` r
island_nations <- c("Antigua","Barbuda","Nevis", 
                 "Saint Kitts","Trinidad",
                 "Tobago","Grenadines","Saint Vincent")
island_nations_match <- shapefile_data %>% 
  filter(country %in% island_nations)
island_nations_match %>% distinct(country)
```

    ##         country
    ## 1       Antigua
    ## 2       Barbuda
    ## 3         Nevis
    ## 4   Saint Kitts
    ## 5      Trinidad
    ## 6        Tobago
    ## 7    Grenadines
    ## 8 Saint Vincent

``` r
ant_bar <- c(137,138 )
kit_nev <- c(930,931)
tri_tog <- c(1425,1426)
vin_gre <- c(1575,1576,1577)
island_nation_names <- c("Antigua and Barbuda","St. Kitts and Nevis","Trinidad and Tobago","St. Vincent and the Grenadines")
island_nations_match <- island_nations_match %>% 
  mutate(country=case_when(group %in% ant_bar~"Antigua and Barbuda",
                           group %in% kit_nev~"St. Kitts and Nevis",
                           group %in% tri_tog~"Trinidad and Tobago",
                           group %in% vin_gre~"St. Vincent and the Grenadines")) %>% 
  tibble()
island_nations_match %>%
  distinct(country) 
```

    ## # A tibble: 4 × 1
    ##   country                       
    ##   <chr>                         
    ## 1 Antigua and Barbuda           
    ## 2 St. Kitts and Nevis           
    ## 3 Trinidad and Tobago           
    ## 4 St. Vincent and the Grenadines

``` r
shapefile_data <- shapefile_data %>%
  filter(!country %in% island_nation_names)
shapefile_data <- shapefile_data %>% 
  bind_rows(island_nations_match) %>%
  arrange(country) %>%
  tibble()
```

Now, we move on to the final two cases pertinent to our impact model,
which Haslam refers to as “subregion promotion.” In the case of the
special administrative regions of Hong Kong and Macao, the `maps`
package organizes these states as subregions within its
country-classification scheme. While this is politically correct,
economic and health data from these regions, as Haslam (2021) points
out, are often collected and presented separately from that of mainland
China.

In following this practice, we therefore need to adjust this
organizational framework to reflect our desire to treat these regions as
country-level entities using the naming conventions prescribed by the
ISO. We accomplish this using the following:

``` r
sra_names <- c("Hong Kong","Macao")
hk_mc <- shapefile_data %>% 
  filter(subregion %in% sra_names)
hk_mc <- hk_mc %>%
  mutate(country = case_when(subregion=="Hong Kong"~"Hong Kong, China",
                             subregion=="Macao"~"Macao, China"))
shapefile_data <- shapefile_data %>%
  filter(!subregion %in% sra_names)
shapefile_data <- shapefile_data %>% 
  bind_rows(hk_mc) %>%
  select(-subregion) %>% 
  tibble()
```

With these steps complete, we can now review the countries currently
accounted for in our shape-file data.

``` r
shapefile_data %>% distinct(country)
```

    ## # A tibble: 258 × 1
    ##    country            
    ##    <chr>              
    ##  1 Afghanistan        
    ##  2 Albania            
    ##  3 Algeria            
    ##  4 American Samoa     
    ##  5 Andorra            
    ##  6 Angola             
    ##  7 Anguilla           
    ##  8 Antarctica         
    ##  9 Antigua            
    ## 10 Antigua and Barbuda
    ## # … with 248 more rows

## Adjusting the Dietary-Footprint Data

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
