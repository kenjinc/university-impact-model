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

In preparation for our spatial join, we will need to reformat the
dietary-footprint data to suit the needs of our analysis.

We’ll begin, first, by selecting the following variables for extraction:
`country`, `diet`, `attribute`, `value`.

``` r
dietary_footprint_data <- dietary_footprint_data %>%
  select(country,diet,attribute,value,centile_down,centile_up)
```

With this accomplished, we can now continue the wrangling process by
pivoting the tibble to a wider format, allowing each row to correspond
to a unique country and each column to map on to the indicator-specific
values and centiles for each dietary pattern.

``` r
dietary_footprint_data <- dietary_footprint_data %>%
  pivot_wider(names_from=c(diet,attribute),
              values_from=c(value,centile_up,centile_down))
```

Now that the data is organized in this way, we can sort through the
newly generated columns and limit the dataset to the specific
combinations of dietary patterns and sustainability indicators that are
appropraite for our analyses. More specifically, we want to extract the
`kg_co2e_excl_luc`, `kg_co2e_total`, `l_blue_green_wf`, and
`l_blue_wf_total` indicators for the baseline, meatless day, low red
meat, no dairy, no red meat, vegetarian, and vegan diets.

``` r
dietary_footprint_data <- dietary_footprint_data %>% select(country,
                                                            centile_down_baseline_kg_co2e_excl_luc,value_baseline_kg_co2e_excl_luc,centile_up_baseline_kg_co2e_excl_luc,centile_down_baseline_kg_co2e_total,value_baseline_kg_co2e_total,centile_up_baseline_kg_co2e_total,centile_down_baseline_l_blue_green_wf,value_baseline_l_blue_green_wf,centile_up_baseline_l_blue_green_wf,centile_down_baseline_l_blue_wf_total,value_baseline_l_blue_wf_total,centile_up_baseline_l_blue_wf_total,
                                                            centile_down_meatless_day_kg_co2e_excl_luc,value_meatless_day_kg_co2e_excl_luc,centile_up_meatless_day_kg_co2e_excl_luc,centile_down_meatless_day_kg_co2e_total,value_meatless_day_kg_co2e_total,centile_up_meatless_day_kg_co2e_total,centile_down_meatless_day_l_blue_green_wf,value_meatless_day_l_blue_green_wf,centile_up_meatless_day_l_blue_green_wf,centile_down_meatless_day_l_blue_wf_total,value_meatless_day_l_blue_wf_total,centile_up_meatless_day_l_blue_wf_total,
                                                            centile_down_low_red_meat_kg_co2e_excl_luc,value_low_red_meat_kg_co2e_excl_luc,centile_up_low_red_meat_kg_co2e_excl_luc,centile_down_low_red_meat_kg_co2e_total,value_low_red_meat_kg_co2e_total,centile_up_low_red_meat_kg_co2e_total,centile_down_low_red_meat_l_blue_green_wf,value_low_red_meat_l_blue_green_wf,centile_up_low_red_meat_l_blue_green_wf,centile_down_low_red_meat_l_blue_wf_total,value_low_red_meat_l_blue_wf_total,centile_up_low_red_meat_l_blue_wf_total,
                                                            centile_down_no_dairy_kg_co2e_excl_luc,value_no_dairy_kg_co2e_excl_luc,centile_up_no_dairy_kg_co2e_excl_luc,centile_down_no_dairy_kg_co2e_total,value_no_dairy_kg_co2e_total,centile_up_no_dairy_kg_co2e_total,centile_down_no_dairy_l_blue_green_wf,value_no_dairy_l_blue_green_wf,centile_up_no_dairy_l_blue_green_wf,centile_down_no_dairy_l_blue_wf_total,value_no_dairy_l_blue_wf_total,centile_up_no_dairy_l_blue_wf_total,
                                                            centile_down_no_red_meat_kg_co2e_excl_luc,value_no_red_meat_kg_co2e_excl_luc,centile_up_no_red_meat_kg_co2e_excl_luc,centile_down_no_red_meat_kg_co2e_total,value_no_red_meat_kg_co2e_total,centile_up_no_red_meat_kg_co2e_total,centile_down_no_red_meat_l_blue_green_wf,value_no_red_meat_l_blue_green_wf,centile_up_no_red_meat_l_blue_green_wf,centile_down_no_red_meat_l_blue_wf_total,value_no_red_meat_l_blue_wf_total,centile_up_no_red_meat_l_blue_wf_total,
                                                            centile_down_lacto_ovo_vegetarian_kg_co2e_excl_luc,value_lacto_ovo_vegetarian_kg_co2e_excl_luc,centile_up_lacto_ovo_vegetarian_kg_co2e_excl_luc,centile_down_lacto_ovo_vegetarian_kg_co2e_total,value_lacto_ovo_vegetarian_kg_co2e_total,centile_up_lacto_ovo_vegetarian_kg_co2e_total,centile_down_lacto_ovo_vegetarian_l_blue_green_wf,value_lacto_ovo_vegetarian_l_blue_green_wf,centile_up_lacto_ovo_vegetarian_l_blue_green_wf,centile_down_lacto_ovo_vegetarian_l_blue_wf_total,value_lacto_ovo_vegetarian_l_blue_wf_total,centile_up_lacto_ovo_vegetarian_l_blue_wf_total,
                                                            centile_down_vegan_kg_co2e_excl_luc,value_vegan_kg_co2e_excl_luc,centile_up_vegan_kg_co2e_excl_luc,centile_down_vegan_kg_co2e_total,value_vegan_kg_co2e_total,centile_up_vegan_kg_co2e_total,centile_down_vegan_l_blue_green_wf,value_vegan_l_blue_green_wf,centile_up_vegan_l_blue_green_wf,centile_down_vegan_l_blue_wf_total,value_vegan_l_blue_wf_total,centile_up_vegan_l_blue_wf_total)
```

## Adjusting the University-Enrollment Data

``` r
impact_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/dietary_footprints_by_country.csv")
dietary_footprint_data %>% head(6)
```

    ## # A tibble: 6 × 85
    ##   country        centi…¹ value…² centi…³ centi…⁴ value…⁵ centi…⁶ centi…⁷ value…⁸
    ##   <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Armenia          137.    1604.   289.    137.    1646.   289.   3107.   1.44e6
    ## 2 Afghanistan       50.3    895.    60.6    50.3    898.    60.6    97.7  1.02e6
    ## 3 Albania          125.    1952.   213.    125.    2013.   213.   1003.   1.44e6
    ## 4 Algeria           98.6    983.   193.     98.6   1108.   193.    356.   1.30e6
    ## 5 Antigua and B…   121.    1310.   204.    121.    1400.   204.   2791.   1.19e6
    ## 6 Argentina         87.9   2952.   142.     87.9   3517.   142.    350.   1.02e6
    ## # … with 76 more variables: centile_up_baseline_l_blue_green_wf <dbl>,
    ## #   centile_down_baseline_l_blue_wf_total <dbl>,
    ## #   value_baseline_l_blue_wf_total <dbl>,
    ## #   centile_up_baseline_l_blue_wf_total <dbl>,
    ## #   centile_down_meatless_day_kg_co2e_excl_luc <dbl>,
    ## #   value_meatless_day_kg_co2e_excl_luc <dbl>,
    ## #   centile_up_meatless_day_kg_co2e_excl_luc <dbl>, …

``` r
write.csv(impact_data,"~/github/university-impact-model/data/model-output/impact-data.csv")
```
