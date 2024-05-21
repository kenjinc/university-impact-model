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

``` r
climate_footprint_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/dietary_footprints_by_country.csv")
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
meat, no dairy, no red meat, vegetarian, and vegan diets. \### would be
good to add EAT-Lancet diet.

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

### RUNNNING THIS BACK WITH EAT-LANCET/NO WATER

Here, in the interest of adding the annual per capita emissions
estimates associated with the EAT-Lancet diet, we need to align the
existing dietary-footprint data from Kim and colleagues (2020) with the
more recently reworked data from Semba et al (2020).

In order to accomplish this, we will need to make a couple of changes
before joining the two datasets. The object, here, is to match the
language of the two data sources, such that they resemble structure and
use the same language to represent the same constructs.

The data associated with the latter publication focuses only on the
total annual per capita greenhouse gas emissions (in kilograms) and
represents this indicator within the `attribute` column as
`per_capita_kg_co2e`. This same indicator is represented in the original
source under the `attribute` column as `kg_co2e_total`.

We can therefore transform the original data source from Kim and
colleagues (2020) to force alignment on this convention in preparation
for our join.

``` r
climate_footprint_data <- climate_footprint_data %>% mutate(attribute=str_replace_all(attribute,"kg_co2e_total","per_capita_kg_co2e"))
```

Now that this is complete, we can select out the relevant columns for
our analyses. Because there is no centile data available in the
associated data for “Adoption of the ‘planetary health diet’ has
different impacts on countries’ greenhouse gas emissions,” we no longer
need to extract the `centile_up` and `centile_down` columns.

``` r
climate_footprint_data <- climate_footprint_data %>%
  select(country_code,country,diet,attribute,value)
```

Now that the relevant columns are represented in the same way across the
two dietary-footprint data sources, we can read in the companion data
source and extract the relevant columns, as we did before.

``` r
planetary_health_emissions_data <- read.csv("/Users/kenjinchang/github/university-impact-model/data/parent-files/planetary_health_emissions_by_country.csv") %>% select(country_code,country,diet,attribute,value)
```

We can see that, while the column names are the same across the two data
sets, the `value` columns are represented differently, with one being
represented as a character string and the other represented as a double
vector.

This difference is a product of the 1,000-separating commas used in the
updated dataset. To remove this, and convert the column structure, we do
the following:

``` r
planetary_health_emissions_data <- planetary_health_emissions_data %>% 
  mutate_all(~sub(",", "", .)) %>% mutate(value=as.double(value)) %>% mutate(country_code=as.integer(country_code))
```

Before performing our bind, there are still some checks that need to be
done. We know that there are 140 countries represented in
`climate_footprint_data` to the 151 in
`planetary_health_emissions_data`.

``` r
climate_footprint_data %>% distinct(country) %>% count()
```

    ##     n
    ## 1 140

``` r
planetary_health_emissions_data %>% distinct(country) %>% count()
```

    ##     n
    ## 1 151

Using filtering joins, we can quickly find out (1) the countries that
are represented in `climate_footprint_data` without a one-to-one match
in `planetary_health_emissions_data`, as well as (2) the countries that
are represented in `planetary_health_emissions_data` without a
one-to-one match in `climate_footprint_data`.

``` r
anti_join(climate_footprint_data,planetary_health_emissions_data,by="country") %>%
  distinct(country)
```

    ##                                      country
    ## 1                                    Bermuda
    ## 2           Bolivia (Plurinational State of)
    ## 3                          Brunei Darussalam
    ## 4                            China, mainland
    ## 5                       China, Hong Kong SAR
    ## 6                 Iran (Islamic Republic of)
    ## 7                             CÃ´te d'Ivoire
    ## 8                          Republic of Korea
    ## 9                           China, Macao SAR
    ## 10 The former Yugoslav Republic of Macedonia
    ## 11                        Russian Federation
    ## 12                 China, Taiwan Province of
    ## 13               United Republic of Tanzania
    ## 14                  United States of America
    ## 15        Venezuela (Bolivarian Republic of)

``` r
anti_join(planetary_health_emissions_data,climate_footprint_data,by="country") %>% distinct(country)
```

    ##                             country
    ## 1                          Mongolia
    ## 2                           Bolivia
    ## 3                     Hong Kong SAR
    ## 4                         Venezuela
    ## 5                     United States
    ## 6                   China Macao SAR
    ## 7                       South Korea
    ## 8                            Russia
    ## 9                    China mainland
    ## 10                             Cuba
    ## 11             United Arab Emirates
    ## 12                           Guinea
    ## 13                           Taiwan
    ## 14                         Djibouti
    ## 15                            Gabon
    ## 16              Trinidad and Tobago
    ## 17            Saint Kitts and Nevis
    ## 18                         Tanzania
    ## 19 Saint Vincent and the Grenadines
    ## 20                             Iran
    ## 21                      Saint Lucia
    ## 22                     Sierra Leone
    ## 23                       Bangladesh
    ## 24                          Grenada
    ## 25                          Nigeria
    ## 26                    Cote d'Ivoire

We will first deal with the dozen cases where there are differences in
how the countries are represented by name. More specifically, we will
address these instances by defaulting to how the countries are
referenced in the original data set from Kim and colleagues (2020).

``` r
planetary_health_emissions_data <- planetary_health_emissions_data %>%
  mutate(across(country,str_replace,"Venezuela","Venezuela (Bolivarian Republic of)")) %>%
  mutate(across(country,str_replace,"United States","United States of America")) %>%
  mutate(across(country,str_replace,"Tanzania","United Republic of Tanzania")) %>%
  mutate(across(country,str_replace,"Taiwan","China, Taiwan Province of")) %>%
  mutate(across(country,str_replace,"Russia","Russian Federation")) %>%
  mutate(across(country,str_replace,"China Macao SAR","China, Macao SAR")) %>%
  mutate(across(country,str_replace,"South Korea","Republic of Korea")) %>%
  mutate(across(country,str_replace,"Cote d'Ivoire","CÃ´te d'Ivoire")) %>%
  mutate(across(country,str_replace,"Iran","Iran (Islamic Republic of)")) %>%
  mutate(across(country,str_replace,"Hong Kong SAR","China, Hong Kong SAR")) %>%
  mutate(across(country,str_replace,"China mainland","China, mainland")) %>%
  mutate(across(country,str_replace,"Bolivia","Bolivia (Plurinational State of)")) 
```

This leaves three countries that are listed in `climate_footprint_data`
but not in `planetary_health_emissions_data`, meaning that EAT Lancet
data will be unavailable for these three areas.

``` r
anti_join(climate_footprint_data,planetary_health_emissions_data,by="country") %>%
  distinct(country)
```

    ##                                     country
    ## 1                                   Bermuda
    ## 2                         Brunei Darussalam
    ## 3 The former Yugoslav Republic of Macedonia

We can now turn to the 14 remaining countries that are listed in
`planetary_health_emissions_data` but not `climate_footprint_data`.

``` r
anti_join(planetary_health_emissions_data,climate_footprint_data,by="country") %>% distinct(country)
```

    ##                             country
    ## 1                          Mongolia
    ## 2                              Cuba
    ## 3              United Arab Emirates
    ## 4                            Guinea
    ## 5                          Djibouti
    ## 6                             Gabon
    ## 7               Trinidad and Tobago
    ## 8             Saint Kitts and Nevis
    ## 9  Saint Vincent and the Grenadines
    ## 10                      Saint Lucia
    ## 11                     Sierra Leone
    ## 12                       Bangladesh
    ## 13                          Grenada
    ## 14                          Nigeria

With these alignments complete, we can now bind the rows together.

``` r
impact_model_data <- bind_rows(planetary_health_emissions_data,climate_footprint_data)
```

Because of how the columns will be concatenated once we pivot the
tibble, we will preemptively modify how the “EAT Lancet” and “2/3_vegan”
patterns are represented under the `diet` string to avoid possible
issues stemming from the spacing on the former and the special-character
use on the latter.

``` r
impact_model_data <- impact_model_data %>% 
  mutate(across(diet,str_replace,"EAT Lancet","eat_lancet")) %>%
  mutate(across(diet,str_replace,"2/3_vegan","two_thirds_vegan")) 
```

Now, we perform a series of changes that will pivot the table such that
each country is represented by a single row with columns corresponding
to the different emissions profiles, as given by the annual per capita
kilograms of carbon-dioxide equivalents, associated with the following
dietary patterns: baseline, adjusted baseline, OECD baseline, meatless
day, low red meat, no red meat, no dairy, pescetarian, EAT Lancet,
vegetarian, 2/3 vegan, and vegan.

``` r
impact_model_data <- impact_model_data %>% 
  select(country,diet,attribute,value) %>%
  pivot_wider(names_from=c(diet,attribute),
              values_from=c(value)) %>%
  select(country,baseline_per_capita_kg_co2e,baseline_adjusted_per_capita_kg_co2e,baseline_oecd_per_capita_kg_co2e,meatless_day_per_capita_kg_co2e,low_red_meat_per_capita_kg_co2e,no_red_meat_per_capita_kg_co2e,no_dairy_per_capita_kg_co2e,pescetarian_per_capita_kg_co2e,lacto_ovo_vegetarian_per_capita_kg_co2e,two_thirds_vegan_per_capita_kg_co2e,vegan_per_capita_kg_co2e)
```

Finally, we (optionally) remove the 17 countries with incomplete data.

``` r
impact_modeL_data <- impact_model_data %>% 
  filter(!country=="Bermuda") %>%
  filter(!country=="Brunei Darussalam") %>%
  filter(!country=="The former Yugoslav Republic of Macedonia") %>%
  filter(!country=="Mongolia") %>%
  filter(!country=="Cuba") %>%
  filter(!country=="United Arab Emirates") %>%
  filter(!country=="Guinea") %>%
  filter(!country=="Djibouti") %>%
  filter(!country=="Gabon") %>%
  filter(!country=="Trinidad and Tobago") %>%
  filter(!country=="Saint Kitts and Nevis") %>%
  filter(!country=="Saint Vincent and the Grenadines") %>%
  filter(!country=="Saint Lucia") %>%
  filter(!country=="Sierra Leone") %>%
  filter(!country=="Bangladesh") %>%
  filter(!country=="Grenada") %>%
  filter(!country=="Nigeria") 
impact_modeL_data
```

    ## # A tibble: 137 × 12
    ##    country       basel…¹ basel…² basel…³ meatl…⁴ low_r…⁵ no_re…⁶ no_da…⁷ pesce…⁸
    ##    <chr>           <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Paraguay        5604.   4137.   5825.   3689.   4159.   1041.   3975.   1005.
    ##  2 Chile           3895.   3747.   4044.   3387.   3416.    892.   3419.    827.
    ##  3 Brazil          4366.   3421.   3454.   3219.   3170.   1557.   2690.   1053.
    ##  4 Argentina       3517.   2784.   2279.   2557.   2180.   1062.   2505.    887.
    ##  5 Australia       3142.   2799.   2298.   2635.   2079.   1156.   2389.   1121.
    ##  6 Bolivia (Plu…   2654.   3414.   4132.   3285.   3214.   1610.   3128.    901.
    ##  7 Central Afri…   2512.   3835.   5673.   3559.   3846.   1447.   3120.    638.
    ##  8 Uruguay         2667.   2207.   2222.   2059.   2034.   1081.   1728.   1038.
    ##  9 New Zealand     2634.   2476.   2106.   2385.   1944.   1306.   1933.   1173.
    ## 10 Israel          2344.   1765.   2201.   1674.   1765.    895.   1493.    825.
    ## # … with 127 more rows, 3 more variables:
    ## #   lacto_ovo_vegetarian_per_capita_kg_co2e <dbl>,
    ## #   two_thirds_vegan_per_capita_kg_co2e <dbl>, vegan_per_capita_kg_co2e <dbl>,
    ## #   and abbreviated variable names ¹​baseline_per_capita_kg_co2e,
    ## #   ²​baseline_adjusted_per_capita_kg_co2e, ³​baseline_oecd_per_capita_kg_co2e,
    ## #   ⁴​meatless_day_per_capita_kg_co2e, ⁵​low_red_meat_per_capita_kg_co2e,
    ## #   ⁶​no_red_meat_per_capita_kg_co2e, ⁷​no_dairy_per_capita_kg_co2e, …

## Adjusting the University-Enrollment Data

Like we did for the dietary-footprint data, we will need to perform a
series of transformations to `university-enrollment_data` before
executing our spatial join.

More specifically, we will need to mirror the structure introduced for
`climate_footprint_data`, whereby each country is represented by a
single row, with subsequent columns representing each of the indicators
of interest.

We begin this process by extracting the relevant columns, assigning a
format to the data frame, and removing the source information and update
history from the bottommost rows of the data set. For convenience, we
also rename the columns corresponding to the designated year range and
replace the NA values

``` r
university_enrollment_data %>% 
  select(-Country.Code,-Series.Code) %>% 
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=X2000..YR2000.,yr2001=X2001..YR2001.,yr2002=X2002..YR2002.,yr2003=X2003..YR2003.,yr2004=X2004..YR2004.,yr2005=X2005..YR2005.,yr2006=X2006..YR2006.,yr2007=X2007..YR2007.,yr2008=X2008..YR2008.,yr2009=X2009..YR2009.,yr2010=X2010..YR2010.,yr2011=X2011..YR2011.,yr2012=X2012..YR2012.,yr2013=X2013..YR2013.,yr2014=X2014..YR2014.,yr2015=X2015..YR2015.,yr2016=X2016..YR2016.,yr2017=X2017..YR2017.,yr2018=X2018..YR2018.,yr2019=X2019..YR2019.,yr2020=X2020..YR2020.) 
```

    ## # A tibble: 1,632 × 28
    ##    country series yr2000 yr2001 yr2002 yr2003 yr2004 yr2005 yr2006 yr2007 yr2008
    ##    <chr>   <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ##  1 Afghan… GNI p… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ##  2 Afghan… Popul… 20779… 21606… 22600… 23680… 24726… 25654… 26433… 27100… 27722…
    ##  3 Afghan… Schoo… 18337… 19309… 20500… 21799… 23052… 24158… 24573… 24794… 24949…
    ##  4 Afghan… Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ##  5 Afghan… Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ##  6 Afghan… Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ##  7 Albania GNI p… 1100   1280   1370   1650   2100   2620   3050   3460   4030  
    ##  8 Albania Popul… 30890… 30601… 30510… 30396… 30269… 30114… 29925… 29700… 29473…
    ##  9 Albania Schoo… 258261 256900 258469 261748 266530 272779 280612 283060 282325
    ## 10 Albania Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## # … with 1,622 more rows, and 17 more variables: yr2009 <chr>, yr2010 <chr>,
    ## #   yr2011 <chr>, yr2012 <chr>, yr2013 <chr>, yr2014 <chr>, yr2015 <chr>,
    ## #   yr2016 <chr>, yr2017 <chr>, yr2018 <chr>, yr2019 <chr>, yr2020 <chr>,
    ## #   X2021..YR2021. <chr>, X2022..YR2022. <chr>, X2023..YR2023. <chr>,
    ## #   X2024..YR2024. <chr>, X2025..YR2025. <chr>

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
