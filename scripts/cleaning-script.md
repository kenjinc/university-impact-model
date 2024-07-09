Data Cleaning and Aggregation
================
Last updated: June 28, 2024

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
  select(country,baseline_per_capita_kg_co2e,baseline_adjusted_per_capita_kg_co2e,baseline_oecd_per_capita_kg_co2e,meatless_day_per_capita_kg_co2e,low_red_meat_per_capita_kg_co2e,no_red_meat_per_capita_kg_co2e,no_dairy_per_capita_kg_co2e,pescetarian_per_capita_kg_co2e,lacto_ovo_vegetarian_per_capita_kg_co2e,eat_lancet_per_capita_kg_co2e,two_thirds_vegan_per_capita_kg_co2e,vegan_per_capita_kg_co2e)
```

Finally, we (optionally) remove the 17 countries with incomplete data.

``` r
impact_model_data <- impact_model_data %>% 
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
```

## Adjusting the University-Enrollment Data

Like we did for the dietary-footprint data, we will need to perform a
series of transformations to `university-enrollment_data` before we are
able to spatially join the primary three data sources.

More specifically, we will need to mirror the structure adopted in both
`shapefilea_data` and `climate_footprint_data`, whereby each country is
represented by a single row, with all subsequent columns representing
the relevant spatial and environmental indicators of interest.

For `university_enrollment_data`, we can begin this process by
extracting the relevant columns, assigning a format to the data frame,
and removing the source information and update-history stamps from the
bottommost rows of the data set. For convenience, we also take this
opportunity to rename the columns for each of the years where there is
available data.

``` r
university_enrollment_data <- university_enrollment_data %>% 
  select(-Country.Code,-Series.Code,-X2020..YR2020.,-X2021..YR2021.,-X2022..YR2022.,-X2023..YR2023.,-X2024..YR2024.,-X2025..YR2025.) %>% 
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=X2000..YR2000.,yr2001=X2001..YR2001.,yr2002=X2002..YR2002.,yr2003=X2003..YR2003.,yr2004=X2004..YR2004.,yr2005=X2005..YR2005.,yr2006=X2006..YR2006.,yr2007=X2007..YR2007.,yr2008=X2008..YR2008.,yr2009=X2009..YR2009.,yr2010=X2010..YR2010.,yr2011=X2011..YR2011.,yr2012=X2012..YR2012.,yr2013=X2013..YR2013.,yr2014=X2014..YR2014.,yr2015=X2015..YR2015.,yr2016=X2016..YR2016.,yr2017=X2017..YR2017.,yr2018=X2018..YR2018.,yr2019=X2019..YR2019.) 
```

We also use the following to convert the reference-year columns from
character strings to double vectors in preparation for our downstream
mutations, changing, in the process, the coerced `NA` inputs into zeros
to more easily set up the conditional logic that will be used in the
upcoming steps:

``` r
university_enrollment_data <- university_enrollment_data %>% 
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019"),as.double) %>%
    replace(is.na(.),0)
```

We will now pivot the tibble so that the population-level indicators
arranged under the `series` column are represented as their own
variables, with each country corresponding to a single row. We will also
take this opportunity to abbreviate the listed indicators for
convenience.

``` r
university_enrollment_data <- university_enrollment_data %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("School age population, tertiary education, both sexes (number)"),"school_aged_population")) %>%
  mutate(across(series,str_replace,fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced_6_enrollment")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced_7_enrollment")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced_8_enrollment")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"national_population")) %>%
  mutate(across("series",str_replace, fixed("GNI per capita, Atlas method (current US$)"),"per_capita_gni")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000"))
```

We are now left with a format that is closer to our desired
specifications, such that the data for each country corresponds to a
single row, with concatenated columns mapping to the values for the
different indicators we have selected by reference year.

The problem, though, is that, for each country, we only need one value
across each of the specified indicators.

The trouble with this, though, is that some of these indicators were
recorded more recently than others,meaning that the most recent year for
which there is data on one indicator may not be the same across all the
remaining indicators of interest.

As a result, we want to pull the values from the most recent reference
year for which there is data available across all indicators of
interest.

This will require us to conditionally mutate seven new columns (1) one
capturing the per capita gross national income, (2) another capturing
the national population, (3) one capturing the school-aged population
for tertiary education programs (18-23, according to
[this](https://www.un.org/esa/sustdev/natlinfo/indicators/indisd/english/chapt36e.htm#:~:text=The%20school%2Dage%20population%20is%20generally%20defined%20in%20three%20age,educational%20levels%20of%20many%20countries.)
UNESCO-provided definition), (4) another capturing the number of
enrollees in ISCED 6 programs, (5) one capturing the number of enrollees
in ISCED 7 programs, (6) another capturing the number of enrollees in
ISCED 8 programs, and (7) one that denoting the most recent reference
year from which all of these variables are being represented.

To accomplish this, we will first mutate new columns identifying the
most recent year for which each indicator has data represented across
the included countries.

``` r
university_enrollment_data <- university_enrollment_data %>%
  mutate(per_capita_gni_ref_year=ifelse(yr2019_per_capita_gni>0,2019,ifelse(yr2018_per_capita_gni>0,2018,ifelse(yr2017_per_capita_gni>0,2017,ifelse(yr2016_per_capita_gni>0,2016,ifelse(yr2015_per_capita_gni>0,2015,ifelse(yr2014_per_capita_gni>0,2014,ifelse(yr2013_per_capita_gni>0,2013,ifelse(yr2012_per_capita_gni>0,2012,ifelse(yr2011_per_capita_gni>0,2011,ifelse(yr2010_per_capita_gni>0,2010,ifelse(yr2009_per_capita_gni>0,2009,ifelse(yr2008_per_capita_gni>0,2008,ifelse(yr2007_per_capita_gni>0,2007,ifelse(yr2006_per_capita_gni>0,2006,ifelse(yr2005_per_capita_gni>0,2005,ifelse(yr2004_per_capita_gni>0,2004,ifelse(yr2003_per_capita_gni>0,2003,ifelse(yr2002_per_capita_gni>0,2002,ifelse(yr2001_per_capita_gni>0,2001,ifelse(yr2000_per_capita_gni>0,2000,0))))))))))))))))))))) %>%
  mutate(national_population_ref_year=ifelse(yr2019_national_population>0,2019,ifelse(yr2018_national_population>0,2018,ifelse(yr2017_national_population>0,2017,ifelse(yr2016_national_population>0,2016,ifelse(yr2015_national_population>0,2015,ifelse(yr2014_national_population>0,2014,ifelse(yr2013_national_population>0,2013,ifelse(yr2012_national_population>0,2012,ifelse(yr2011_national_population>0,2011,ifelse(yr2010_national_population>0,2010,ifelse(yr2009_national_population>0,2009,ifelse(yr2008_national_population>0,2008,ifelse(yr2007_national_population>0,2007,ifelse(yr2006_national_population>0,2006,ifelse(yr2005_national_population>0,2005,ifelse(yr2004_national_population>0,2004,ifelse(yr2003_national_population>0,2003,ifelse(yr2002_national_population>0,2002,ifelse(yr2001_national_population>0,2001,ifelse(yr2000_national_population>0,2000,0))))))))))))))))))))) %>%
  mutate(school_aged_population_ref_year=ifelse(yr2019_school_aged_population>0,2019,ifelse(yr2018_school_aged_population>0,2018,ifelse(yr2017_school_aged_population>0,2017,ifelse(yr2016_school_aged_population>0,2016,ifelse(yr2015_school_aged_population>0,2015,ifelse(yr2014_school_aged_population>0,2014,ifelse(yr2013_school_aged_population>0,2013,ifelse(yr2012_school_aged_population>0,2012,ifelse(yr2011_school_aged_population>0,2011,ifelse(yr2010_school_aged_population>0,2010,ifelse(yr2009_school_aged_population>0,2009,ifelse(yr2008_school_aged_population>0,2008,ifelse(yr2007_school_aged_population>0,2007,ifelse(yr2006_school_aged_population>0,2006,ifelse(yr2005_school_aged_population>0,2005,ifelse(yr2004_school_aged_population>0,2004,ifelse(yr2003_school_aged_population>0,2003,ifelse(yr2002_school_aged_population>0,2002,ifelse(yr2001_school_aged_population>0,2001,ifelse(yr2000_school_aged_population>0,2000,0))))))))))))))))))))) %>%
  mutate(isced_6_ref_year=ifelse(yr2019_isced_6_enrollment>0,2019,ifelse(yr2018_isced_6_enrollment>0,2018,ifelse(yr2017_isced_6_enrollment>0,2017,ifelse(yr2016_isced_6_enrollment>0,2016,ifelse(yr2015_isced_6_enrollment>0,2015,ifelse(yr2014_isced_6_enrollment>0,2014,ifelse(yr2013_isced_6_enrollment>0,2013,ifelse(yr2012_isced_6_enrollment>0,2012,ifelse(yr2011_isced_6_enrollment>0,2011,ifelse(yr2010_isced_6_enrollment>0,2010,ifelse(yr2009_isced_6_enrollment>0,2009,ifelse(yr2008_isced_6_enrollment>0,2008,ifelse(yr2007_isced_6_enrollment>0,2007,ifelse(yr2006_isced_6_enrollment>0,2006,ifelse(yr2005_isced_6_enrollment>0,2005,ifelse(yr2004_isced_6_enrollment>0,2004,ifelse(yr2003_isced_6_enrollment>0,2003,ifelse(yr2002_isced_6_enrollment>0,2002,ifelse(yr2001_isced_6_enrollment>0,2001,ifelse(yr2000_isced_6_enrollment>0,2000,0))))))))))))))))))))) %>%
  mutate(isced_7_ref_year=ifelse(yr2019_isced_7_enrollment>0,2019,ifelse(yr2018_isced_7_enrollment>0,2018,ifelse(yr2017_isced_7_enrollment>0,2017,ifelse(yr2016_isced_7_enrollment>0,2016,ifelse(yr2015_isced_7_enrollment>0,2015,ifelse(yr2014_isced_7_enrollment>0,2014,ifelse(yr2013_isced_7_enrollment>0,2013,ifelse(yr2012_isced_7_enrollment>0,2012,ifelse(yr2011_isced_7_enrollment>0,2011,ifelse(yr2010_isced_7_enrollment>0,2010,ifelse(yr2009_isced_7_enrollment>0,2009,ifelse(yr2008_isced_7_enrollment>0,2008,ifelse(yr2007_isced_7_enrollment>0,2007,ifelse(yr2006_isced_7_enrollment>0,2006,ifelse(yr2005_isced_7_enrollment>0,2005,ifelse(yr2004_isced_7_enrollment>0,2004,ifelse(yr2003_isced_7_enrollment>0,2003,ifelse(yr2002_isced_7_enrollment>0,2002,ifelse(yr2001_isced_7_enrollment>0,2001,ifelse(yr2000_isced_7_enrollment>0,2000,0))))))))))))))))))))) %>%
  mutate(isced_8_ref_year=ifelse(yr2019_isced_8_enrollment>0,2019,ifelse(yr2018_isced_8_enrollment>0,2018,ifelse(yr2017_isced_8_enrollment>0,2017,ifelse(yr2016_isced_8_enrollment>0,2016,ifelse(yr2015_isced_8_enrollment>0,2015,ifelse(yr2014_isced_8_enrollment>0,2014,ifelse(yr2013_isced_8_enrollment>0,2013,ifelse(yr2012_isced_8_enrollment>0,2012,ifelse(yr2011_isced_8_enrollment>0,2011,ifelse(yr2010_isced_8_enrollment>0,2010,ifelse(yr2009_isced_8_enrollment>0,2009,ifelse(yr2008_isced_8_enrollment>0,2008,ifelse(yr2007_isced_8_enrollment>0,2007,ifelse(yr2006_isced_8_enrollment>0,2006,ifelse(yr2005_isced_8_enrollment>0,2005,ifelse(yr2004_isced_8_enrollment>0,2004,ifelse(yr2003_isced_8_enrollment>0,2003,ifelse(yr2002_isced_8_enrollment>0,2002,ifelse(yr2001_isced_8_enrollment>0,2001,ifelse(yr2000_isced_8_enrollment>0,2000,0))))))))))))))))))))) 
```

From an initial glance, we can see that, for some of the countries
included in the dataset, there are differences in the reference years
being pulled across the various indicators of interest, with some
countries having no indicator-specific data between 2000 and 2019.

This means that, in addition to there being cases where we need to align
the reference years from which the values are being pulled, such that
the values for each country across all indicators line up to the same
reference year, we also know that there are some countries that we can
remove from the dataset entirely.

For example, we can surmise that, for the countries where the sum of
`isced_6_ref_year`, `isced_7_ref_year`, and `isced_8_ref_year` is
equivalent to `0`, there is no available university-enrollment data.

We identify these cases here:

``` r
university_enrollment_data %>%
  filter(isced_6_ref_year+isced_7_ref_year+isced_8_ref_year==0) %>%
  select(country)
```

    ## # A tibble: 56 × 1
    ##    country                 
    ##    <chr>                   
    ##  1 American Samoa          
    ##  2 Angola                  
    ##  3 Anguilla                
    ##  4 Antigua and Barbuda     
    ##  5 Bahamas, The            
    ##  6 Bolivia                 
    ##  7 British Virgin Islands  
    ##  8 Cayman Islands          
    ##  9 Central African Republic
    ## 10 Channel Islands         
    ## # … with 46 more rows

Given that these countries have no available enrollment data, we can
remove them from the tibble using the following groupings:

``` r
university_enrollment_data <- university_enrollment_data %>%
  rowwise() %>%
  filter(!isced_6_ref_year+isced_7_ref_year+isced_8_ref_year==0)
```

While we are at it, we can also remove the rows representing combined
data across multiple countries. In the EdStats database, these items are
referred to as “aggregates” and include the following:

- “Arab World”
- “Caribbean small states”
- “Central Europe and the Baltics”
- “Early-demographic dividend”
- “East Asia & Pacific”
- “East Asia & Pacific (excluding high income)”
- “East Asia & Pacific (IDA & IBRD countries)”
- “Euro area”
- “Europe & Central Asia”
- “Europe & Central Asia (excluding high income)”
- “Europe & Central Asia (IDA & IBRD countries)”
- “European Union”
- “Fragile and conflict affected situations”
- “Global Partnership for Education”
- “Heavily indebted poor countries (HIPC)”
- “High income”
- “IBRD only”
- “IDA & IBRD total”
- “IDA blend”
- “IDA only”
- “IDA total”
- “Late-demographic dividend”
- “Latin America & Caribbean”
- “Latin America & Caribbean (excluding high income)”
- “Latin America & the Caribbean (IDA & IBRD countries)”
- “Least developed countries: UN classification”
- “Low & middle income”
- “Low income”
- “Lower middle income”
- “Middle East & North Africa”
- “Middle East & North Africa (excluding high income)”
- “Middle East & North Africa (IDA & IBRD countries)”
- “Middle income”
- “North America”
- “OECD members”
- “Other small states”
- “Pacific island small states”
- “Post-demographic dividend”
- “Pre-demographic dividend”
- “Small states”
- “South Asia”
- “South Asia (IDA & IBR countries)”
- “Sub-Saharan Africa”
- “Sub-Saharan Africa (IDA & IBRD countries)”
- “Sub-Saharan Africa (excluding high income)”
- “Upper middle income”
- “World”

``` r
university_enrollment_data <- university_enrollment_data %>% 
  filter_at(vars(country),all_vars(!. %in% c("Arab World","Caribbean small states","Central Europe and the Baltics","Early-demographic dividend","East Asia & Pacific","East Asia & Pacific (excluding high income)","East Asia & Pacific (IDA & IBRD countries)","Euro area","Europe & Central Asia","Europe & Central Asia (excluding high income)","Europe & Central Asia (IDA & IBRD countries)","European Union","Fragile and conflict affected situations","Global Partnership for Education","Heavily indebted poor countries (HIPC)","High income","IBRD only","IDA & IBRD total","IDA blend","IDA only","IDA total","Late-demographic dividend","Latin America & Caribbean","Latin America & Caribbean (excluding high income)","Latin America & the Caribbean (IDA & IBRD countries)","Least developed countries: UN classification","Low & middle income","Low income","Lower middle income","Middle East & North Africa","Middle East & North Africa (excluding high income)","Middle East & North Africa (IDA & IBRD countries)","Middle income","North America","OECD members","Other small states","Pacific island small states","Post-demographic dividend","Pre-demographic dividend","Small states","South Asia","South Asia (IDA & IBRD)","Sub-Saharan Africa","Sub-Saharan Africa (excluding high income)","Sub-Saharan Africa (IDA & IBRD countries)","Upper middle income","World")))
```

With these rows removed, we can begin now generate an additional set of
columns that retrieve the values associated with all indicators of
interest for the identified reference years.

(NOTE THAT THIS DEVIATES FROM ORIGINAL PLAN - ACTUALLY JUST TAKES THE
MOST RECENT VALUE FOR EAACH INDICATOR AT THE. COUNTRY LEVEL)

``` r
university_enrollment_data <- university_enrollment_data %>% 
  mutate(national_population=ifelse(yr2019_national_population>0,yr2019_national_population,ifelse(yr2018_national_population>0,yr2018_national_population,ifelse(yr2017_national_population>0,yr2017_national_population,ifelse(yr2016_national_population>0,yr2016_national_population,ifelse(yr2015_national_population>0,yr2015_national_population,ifelse(yr2014_national_population>0,yr2014_national_population,ifelse(yr2013_national_population>0,yr2013_national_population,ifelse(yr2012_national_population>0,yr2012_national_population,ifelse(yr2011_national_population>0,yr2011_national_population,ifelse(yr2010_national_population>0,yr2010_national_population,ifelse(yr2009_national_population>0,yr2009_national_population,ifelse(yr2008_national_population>0,yr2008_national_population,ifelse(yr2007_national_population>0,yr2007_national_population,ifelse(yr2006_national_population>0,yr2006_national_population,ifelse(yr2005_national_population>0,yr2005_national_population,ifelse(yr2004_national_population>0,yr2004_national_population,ifelse(yr2003_national_population>0,yr2003_national_population,ifelse(yr2002_national_population>0,yr2002_national_population,ifelse(yr2001_national_population>0,yr2001_national_population,ifelse(yr2000_national_population>0,yr2000_national_population,0))))))))))))))))))))) %>%
   mutate(school_aged_population=ifelse(yr2019_school_aged_population>0,yr2019_school_aged_population,ifelse(yr2018_school_aged_population>0,yr2018_school_aged_population,ifelse(yr2017_school_aged_population>0,yr2017_school_aged_population,ifelse(yr2016_school_aged_population>0,yr2016_school_aged_population,ifelse(yr2015_school_aged_population>0,yr2015_school_aged_population,ifelse(yr2014_school_aged_population>0,yr2014_school_aged_population,ifelse(yr2013_school_aged_population>0,yr2013_school_aged_population,ifelse(yr2012_school_aged_population>0,yr2012_school_aged_population,ifelse(yr2011_school_aged_population>0,yr2011_school_aged_population,ifelse(yr2010_school_aged_population>0,yr2010_school_aged_population,ifelse(yr2009_school_aged_population>0,yr2009_school_aged_population,ifelse(yr2008_school_aged_population>0,yr2008_school_aged_population,ifelse(yr2007_school_aged_population>0,yr2007_school_aged_population,ifelse(yr2006_school_aged_population>0,yr2006_school_aged_population,ifelse(yr2005_school_aged_population>0,yr2005_school_aged_population,ifelse(yr2004_school_aged_population>0,yr2004_school_aged_population,ifelse(yr2003_school_aged_population>0,yr2003_school_aged_population,ifelse(yr2002_school_aged_population>0,yr2002_school_aged_population,ifelse(yr2001_school_aged_population>0,yr2001_school_aged_population,ifelse(yr2000_school_aged_population>0,yr2000_school_aged_population,0))))))))))))))))))))) %>%
  mutate(per_capita_gni=ifelse(yr2019_per_capita_gni>0,yr2019_per_capita_gni,ifelse(yr2018_per_capita_gni>0,yr2018_per_capita_gni,ifelse(yr2017_per_capita_gni>0,yr2017_per_capita_gni,ifelse(yr2016_per_capita_gni>0,yr2016_per_capita_gni,ifelse(yr2015_per_capita_gni>0,yr2015_per_capita_gni,ifelse(yr2014_per_capita_gni>0,yr2014_per_capita_gni,ifelse(yr2013_per_capita_gni>0,yr2013_per_capita_gni,ifelse(yr2012_per_capita_gni>0,yr2012_per_capita_gni,ifelse(yr2011_per_capita_gni>0,yr2011_per_capita_gni,ifelse(yr2010_per_capita_gni>0,yr2010_per_capita_gni,ifelse(yr2009_per_capita_gni>0,yr2009_per_capita_gni,ifelse(yr2008_per_capita_gni>0,yr2008_per_capita_gni,ifelse(yr2007_per_capita_gni>0,yr2007_per_capita_gni,ifelse(yr2006_per_capita_gni>0,yr2006_per_capita_gni,ifelse(yr2005_per_capita_gni>0,yr2005_per_capita_gni,ifelse(yr2004_per_capita_gni>0,yr2004_per_capita_gni,ifelse(yr2003_per_capita_gni>0,yr2003_per_capita_gni,ifelse(yr2002_per_capita_gni>0,yr2002_per_capita_gni,ifelse(yr2001_per_capita_gni>0,yr2001_per_capita_gni,ifelse(yr2000_per_capita_gni>0,yr2000_per_capita_gni,0))))))))))))))))))))) %>%
  mutate(isced_6_enrollment=ifelse(yr2019_isced_6_enrollment>0,yr2019_isced_6_enrollment,ifelse(yr2018_isced_6_enrollment>0,yr2018_isced_6_enrollment,ifelse(yr2017_isced_6_enrollment>0,yr2017_isced_6_enrollment,ifelse(yr2016_isced_6_enrollment>0,yr2016_isced_6_enrollment,ifelse(yr2015_isced_6_enrollment>0,yr2015_isced_6_enrollment,ifelse(yr2014_isced_6_enrollment>0,yr2014_isced_6_enrollment,ifelse(yr2013_isced_6_enrollment>0,yr2013_isced_6_enrollment,ifelse(yr2012_isced_6_enrollment>0,yr2012_isced_6_enrollment,ifelse(yr2011_isced_6_enrollment>0,yr2011_isced_6_enrollment,ifelse(yr2010_isced_6_enrollment>0,yr2010_isced_6_enrollment,ifelse(yr2009_isced_6_enrollment>0,yr2009_isced_6_enrollment,ifelse(yr2008_isced_6_enrollment>0,yr2008_isced_6_enrollment,ifelse(yr2007_isced_6_enrollment>0,yr2007_isced_6_enrollment,ifelse(yr2006_isced_6_enrollment>0,yr2006_isced_6_enrollment,ifelse(yr2005_isced_6_enrollment>0,yr2005_isced_6_enrollment,ifelse(yr2004_isced_6_enrollment>0,yr2004_isced_6_enrollment,ifelse(yr2003_isced_6_enrollment>0,yr2003_isced_6_enrollment,ifelse(yr2002_isced_6_enrollment>0,yr2002_isced_6_enrollment,ifelse(yr2001_isced_6_enrollment>0,yr2001_isced_6_enrollment,ifelse(yr2000_isced_6_enrollment>0,yr2000_isced_6_enrollment,0))))))))))))))))))))) %>%
  mutate(isced_7_enrollment=ifelse(yr2019_isced_7_enrollment>0,yr2019_isced_7_enrollment,ifelse(yr2018_isced_7_enrollment>0,yr2018_isced_7_enrollment,ifelse(yr2017_isced_7_enrollment>0,yr2017_isced_7_enrollment,ifelse(yr2016_isced_7_enrollment>0,yr2016_isced_7_enrollment,ifelse(yr2015_isced_7_enrollment>0,yr2015_isced_7_enrollment,ifelse(yr2014_isced_7_enrollment>0,yr2014_isced_7_enrollment,ifelse(yr2013_isced_7_enrollment>0,yr2013_isced_7_enrollment,ifelse(yr2012_isced_7_enrollment>0,yr2012_isced_7_enrollment,ifelse(yr2011_isced_7_enrollment>0,yr2011_isced_7_enrollment,ifelse(yr2010_isced_7_enrollment>0,yr2010_isced_7_enrollment,ifelse(yr2009_isced_7_enrollment>0,yr2009_isced_7_enrollment,ifelse(yr2008_isced_7_enrollment>0,yr2008_isced_7_enrollment,ifelse(yr2007_isced_7_enrollment>0,yr2007_isced_7_enrollment,ifelse(yr2006_isced_7_enrollment>0,yr2006_isced_7_enrollment,ifelse(yr2005_isced_7_enrollment>0,yr2005_isced_7_enrollment,ifelse(yr2004_isced_7_enrollment>0,yr2004_isced_7_enrollment,ifelse(yr2003_isced_7_enrollment>0,yr2003_isced_7_enrollment,ifelse(yr2002_isced_7_enrollment>0,yr2002_isced_7_enrollment,ifelse(yr2001_isced_7_enrollment>0,yr2001_isced_7_enrollment,ifelse(yr2000_isced_7_enrollment>0,yr2000_isced_7_enrollment,0))))))))))))))))))))) %>%
  mutate(isced_8_enrollment=ifelse(yr2019_isced_8_enrollment>0,yr2019_isced_8_enrollment,ifelse(yr2018_isced_8_enrollment>0,yr2018_isced_8_enrollment,ifelse(yr2017_isced_8_enrollment>0,yr2017_isced_8_enrollment,ifelse(yr2016_isced_8_enrollment>0,yr2016_isced_8_enrollment,ifelse(yr2015_isced_8_enrollment>0,yr2015_isced_8_enrollment,ifelse(yr2014_isced_8_enrollment>0,yr2014_isced_8_enrollment,ifelse(yr2013_isced_8_enrollment>0,yr2013_isced_8_enrollment,ifelse(yr2012_isced_8_enrollment>0,yr2012_isced_8_enrollment,ifelse(yr2011_isced_8_enrollment>0,yr2011_isced_8_enrollment,ifelse(yr2010_isced_8_enrollment>0,yr2010_isced_8_enrollment,ifelse(yr2009_isced_8_enrollment>0,yr2009_isced_8_enrollment,ifelse(yr2008_isced_8_enrollment>0,yr2008_isced_8_enrollment,ifelse(yr2007_isced_8_enrollment>0,yr2007_isced_8_enrollment,ifelse(yr2006_isced_8_enrollment>0,yr2006_isced_8_enrollment,ifelse(yr2005_isced_8_enrollment>0,yr2005_isced_8_enrollment,ifelse(yr2004_isced_8_enrollment>0,yr2004_isced_8_enrollment,ifelse(yr2003_isced_8_enrollment>0,yr2003_isced_8_enrollment,ifelse(yr2002_isced_8_enrollment>0,yr2002_isced_8_enrollment,ifelse(yr2001_isced_8_enrollment>0,yr2001_isced_8_enrollment,ifelse(yr2000_isced_8_enrollment>0,yr2000_isced_8_enrollment,0)))))))))))))))))))))
```

With these columns added to the dataset, we can now remove the
year-specific columns and abbreviate our dataset to the
indicator-specific values associated with the most recent year for which
there is data.

``` r
university_enrollment_data <- university_enrollment_data %>%
  select(country,national_population,national_population_ref_year,school_aged_population,school_aged_population_ref_year,per_capita_gni,per_capita_gni_ref_year,isced_6_enrollment,isced_6_ref_year,isced_7_enrollment,isced_7_ref_year,isced_8_enrollment,isced_8_ref_year)
```

Previously, we identified instances of missing data pertaining to
enrollment in ISCED 6, 7, and 8 programs, but we did not do the same for
the remaining EdStats indicators, which include the renamed
`national_population` , `school_aged_population`, and `per_capita_gni`
variables.

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(national_population==0) %>%
  distinct(country)
```

    ## # A tibble: 0 × 1
    ## # Rowwise: 
    ## # … with 1 variable: country <chr>

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(school_aged_population==0) %>%
  distinct(country)
```

    ## # A tibble: 4 × 1
    ## # Rowwise: 
    ##   country
    ##   <chr>  
    ## 1 Andorra
    ## 2 Japan  
    ## 3 Lebanon
    ## 4 Monaco

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(per_capita_gni==0) %>%
  distinct(country)
```

    ## # A tibble: 6 × 1
    ## # Rowwise: 
    ##   country                  
    ##   <chr>                    
    ## 1 Andorra                  
    ## 2 Curacao                  
    ## 3 Korea, Dem. People's Rep.
    ## 4 Monaco                   
    ## 5 San Marino               
    ## 6 Sint Maarten (Dutch part)

Based on this, we can see that there are a few missing-data problems we
may have to address later.

In the meantime, we can focus on constructing two new columns: (1) one
that reflects total enrollment across all ISCED 6, 7, and 8 programs
(i.e., `university_enrollment`), (2) another that reflects the
proportion of the national population that is school aged
(i.e.,`proportion_school_aged`), and (3) one that reflects the
proportion of the school-aged population that is enrolled in ISCED 6, 7,
or 8 programs (i.e., `proportion_school_aged_enrolled`)

``` r
university_enrollment_data <- university_enrollment_data %>%
  mutate(university_enrollment=isced_6_enrollment+isced_7_enrollment+isced_8_enrollment) %>%
  mutate(proportion_school_aged=school_aged_population/national_population) %>%
  mutate(proportion_school_aged_enrolled=university_enrollment/school_aged_population)
```

(Need to add code checking reference year alignment across indicators)

While there are still unresolved missing-data issues, we will defer on
how we will address them until after our spatial join.

## Spatially Joining Our Dietary-Footprint and University-Enrollment Data

Now that the three parent datasets are in a format that align with each
other and the needs of our analyses, we can begin the necessary
preparations for our spatial join.

To do this, we will first need to identify inconsistencies in how
countries are named and represented between the three three data
sources. We will begin by examining the countries that are listed in
`impact_model_data` without a one-to-one match in
`university_enrollment_data`, and vice versa.

``` r
anti_join(impact_model_data,university_enrollment_data,by="country") %>%
  select(country)
```

    ## # A tibble: 34 × 1
    ##    country                           
    ##    <chr>                             
    ##  1 Paraguay                          
    ##  2 Bolivia (Plurinational State of)  
    ##  3 Central African Republic          
    ##  4 China, Hong Kong SAR              
    ##  5 Venezuela (Bolivarian Republic of)
    ##  6 United States of America          
    ##  7 Kyrgyzstan                        
    ##  8 China, Macao SAR                  
    ##  9 Republic of Korea                 
    ## 10 French Polynesia                  
    ## # … with 24 more rows

``` r
anti_join(university_enrollment_data,impact_model_data,by="country") %>%
  select(country)
```

    ## # A tibble: 66 × 1
    ## # Rowwise: 
    ##    country          
    ##    <chr>            
    ##  1 Andorra          
    ##  2 Aruba            
    ##  3 Bahrain          
    ##  4 Bangladesh       
    ##  5 Bermuda          
    ##  6 Bhutan           
    ##  7 Brunei Darussalam
    ##  8 Burundi          
    ##  9 Chad             
    ## 10 China            
    ## # … with 56 more rows

Based on these two generated lists, we can see that there are three
types of cases that need to be addressed: (1) the `x` instances where
there is a mismatch in how a country is indexed across the two data
sources, (2) the `y` instances where a country appears in just one of
the two data sources because it was previously removed due to
insufficient data, and (3) the `z` instances where a country appears in
just one of the two data sources because it was not originally included
in one of the two data sources’ list of included countries.

In the case of `impact_model_data`, we removed 17 rows due to observed
differences in the countries being reported between the
dietary-footprint data containing the EAT-Lancet diet estimates and the
data containing the remaining dietary scenarios. More specifically, the
named countries of Bermuda, Brunei Darussalam, the former Yugoslav
Republic of Macedonia, Mongolia, Cuba, United Arab Emirates, Guinea,
Djibouti, Gabon, Trinidad and Tobago, Saint Kitts and Nevis, Saint
Vincent and the Grenadines, Saint Lucia, Sierra Leone, Bangladesh,
Grenada, and Nigeria all had incomplete data because they appeared in
only one of the two required data sources.

In `university_enrollment_data`, on the other hand, we removed a total
of 103 rows, either because they had no available university-enrollment
data (i.e., the sum of `isced_6`, `isced_7`, and `isced_8` enrollment
was equal to 0) or because they represented aggregate groupings of
countries rather than individual nation-states.

For the former scenario, this resulted in the removal of the following
55 rows: American Samoa, Angola, Anguilla, Antigua and Barbuda, The
Bahamas, Bolivia, British Virgin Islands, Cayman Islands, Central
African Republic, Channel Islands, Cook Islands, Djibouti, Dominica,
Equatorial Guinea, Faroe Islands, French Polynesia, Gabon, The Gambia,
Gibraltar, Greenland, Guam, Guinea-Bissau, Guyana, Haiti, Isle of Man,
Kiribati, Kosovo, Liberia, Mayotte, Federated States of Micronesia,
Montserraat, Nauru, Netherlands Antilles, New Caledonia, Nicaragua,
Niue, Northern Mariana Islands, Palau, Papua New Guinea, Paraguay, Sao
Tome and Principe, Sierra Leone, Solomon Islands, Somalia, South Sudan,
St, Martin, St. Vincent and the Grenadines, Suriname, Timor-Leste,
Tokelau, Tonga, Tuvualu, Vanuatu, Virgin Islands, and Zambia.

The latter scenario, on the other hand, resulted in the removal of the
following 48 rows: Arab World, Caribbean small states, Central Europe
and the Baltics, Early-demographic dividend, East Asia and Pacific, East
Asia and Pacific (excluding high income), East Asia and Pacific (IDA and
IBRD countries), Euro area, Europe and Central Asia, Europe and Central
Asia (excluding high income), Europe and Central Asia (IDA and IBRD
countries), European Union, Fragile and conflict affected situations,
Global Partnership for Education, Heavily indebted poor countries
(HIPC), High income, IBRD only, IDA and IBRD total, IDA blend, IDA only,
IDA total, Late-demogaphic dividend, Latin America and Caribbean, Latin
American and Caribbean (excluding high income), Latin America and
Caribbean (IDA and IBRD countries), Least developed countries: UN
classification, Low and middle income, Low income, Lower middle income,
Middle East and North Africa, Middle East and North Africa (excluding
high income), Middle East and North Africa (IDA and IBRD countries),
Middle income, North America, OECD members, Other small states, Pacific
island small states, Post-demographic dividend, Pre-demographic
dividend, Small states, South Asia, South Asia (IDA and IBRD countries),
Sub-Saharan Africa, Sub-Saharan Africa (IDA and IBRD countries),
Sub-Saharan Africa (excluding high income), Upper middle income, and
World.

Because the data will be incomplete for any country that is included in
one data source but not the other, we can continue with this cleaning
process by removing the rows that appear in the university-enrollment or
dietary-footprint data but not the other.

First, we filter out the 17 rows that were previously removed from
`university_enrollment_data` and appear in `impact_model_data`.

``` r
impact_model_data <- impact_model_data %>%
  filter_at(vars(country),all_vars(!. %in% c("Antigua and Barbuda","Bahamas","Bolivia (Plurinational State of)","Central African Republic","Djibouti","Dominica","French Polynesia","Gabon","Gambia","Guyana","Kiribati","New Caledonia","Nicaragua","Paraguay","Sao Tome and Principe","Sierra Leone","Solomon Islands","Saint Vincent and the Grenadines","Suriname","Vanuatu","Zambia")))
```

Next, we perform the inverse by filtering out the 13 rows that were
previously removed from `impact_model_data` and appear in
`university_enrollment_data`.

``` r
university_enrollment_data <- university_enrollment_data %>%
  filter_at(vars(country),all_vars(!. %in% c("Bermuda","Brunei Darussalam","North Macedonia","Mongolia","Cuba","United Arab Emirates","Guinea","Trinidad and Tobago","St. Kitts and Nevis","St. Lucia","Bangladesh","Grenada","Nigeria")))
```

With these two steps complete, we can now do a progress check to see how
the two data sources are aligning:

``` r
anti_join(university_enrollment_data,impact_model_data,by="country") %>%
  select(country)
```

    ## # A tibble: 53 × 1
    ## # Rowwise: 
    ##    country         
    ##    <chr>           
    ##  1 Andorra         
    ##  2 Aruba           
    ##  3 Bahrain         
    ##  4 Bhutan          
    ##  5 Burundi         
    ##  6 Chad            
    ##  7 China           
    ##  8 Comoros         
    ##  9 Congo, Dem. Rep.
    ## 10 Congo, Rep.     
    ## # … with 43 more rows

``` r
anti_join(impact_model_data,university_enrollment_data,by="country") %>%
  select(country) %>% arrange(country)
```

    ## # A tibble: 17 × 1
    ##    country                           
    ##    <chr>                             
    ##  1 CÃ´te d'Ivoire                    
    ##  2 China, Hong Kong SAR              
    ##  3 China, Macao SAR                  
    ##  4 China, mainland                   
    ##  5 China, Taiwan Province of         
    ##  6 Congo                             
    ##  7 Egypt                             
    ##  8 Iran (Islamic Republic of)        
    ##  9 Kyrgyzstan                        
    ## 10 Republic of Korea                 
    ## 11 Republic of Moldova               
    ## 12 Slovakia                          
    ## 13 Turkey                            
    ## 14 United Republic of Tanzania       
    ## 15 United States of America          
    ## 16 Venezuela (Bolivarian Republic of)
    ## 17 Yemen

Given the cases that are left, we can see that there are 16 instances
where the countries appearing in `impact_model_data` are indexed
differently than in `university_enrollment_data`. For the time-being, we
will address this misalignment by changing how the countries are named
in `university_enrollment_data` so that they match the conventions used
in `impact_model_data`.

With regard to the case of the was not explicitly clear which country
“Congo” referred to in the `impact_model_data`, but we inferred—based on
the population data provided in the data source from which the
EAT-Lancet-planetary-health-diet estimates were derived—that this
designation mapped on to the Republic of Congo named in
`university_enrollment_data` rather than the Democratic Republic of the
Congo.

``` r
university_enrollment_data <- university_enrollment_data %>%
  mutate(across(country,str_replace,"Cote d'Ivoire","CÃ´te d'Ivoire")) %>%
  mutate(across(country,str_replace,"China","China, mainland")) %>%
  mutate(across(country,str_replace,"Hong Kong SAR, China, mainland","China, Hong Kong SAR")) %>%
  mutate(across(country,str_replace,"Macao SAR, China, mainland","China, Macao SAR")) %>%
  mutate(across(country,str_replace,"Congo, Rep.","Congo")) %>%
  mutate(across(country,str_replace,"Egypt, Arab Rep.","Egypt")) %>%
  mutate(across(country,str_replace,"Iran, Islamic Rep.","Iran (Islamic Republic of)")) %>%
  mutate(across(country,str_replace,"Kyrgyz Republic","Kyrgyzstan")) %>%
  mutate(across(country,str_replace,"Korea, Rep.","Republic of Korea")) %>%
  mutate(across(country,str_replace,"Moldova","Republic of Moldova")) %>%
  mutate(across(country,str_replace,"Slovak Republic","Slovakia")) %>%
  mutate(across(country,str_replace,"Turkiye","Turkey")) %>%
  mutate(across(country,str_replace,"Tanzania","United Republic of Tanzania")) %>%
  mutate(across(country,str_replace,"United States","United States of America")) %>%
  mutate(across(country,str_replace,"Venezuela, RB","Venezuela (Bolivarian Republic of)")) %>%
  mutate(across(country,str_replace,"Yemen, Rep.","Yemen")) 
```

As we can see, this leaves only Taiwan, as the single nation-state that
is named in the dietary-footprint data sources, has all the requisite
dietary-footprint data, but was not listed in
`university_enrollment_data`.

``` r
anti_join(impact_model_data,university_enrollment_data,by="country") %>%
  select(country) %>% arrange(country)
```

    ## # A tibble: 1 × 1
    ##   country                  
    ##   <chr>                    
    ## 1 China, Taiwan Province of

We will circle back to this case once we have completed with the
subsequent step.

In addition to this single case, there are now 37 countries that appear
in `university_enrollment_data` without a one-to-one match in
`impact_model_data`. At this stage, these represent the countries that
have the required university-enrollment data but are unaccounted for in
the selected dietary-footprint data sources.

``` r
anti_join(university_enrollment_data,impact_model_data,by="country") %>%
  select(country) %>% arrange(country)
```

    ## # A tibble: 37 × 1
    ## # Rowwise: 
    ##    country           
    ##    <chr>             
    ##  1 Andorra           
    ##  2 Aruba             
    ##  3 Bahrain           
    ##  4 Bhutan            
    ##  5 Burundi           
    ##  6 Chad              
    ##  7 Comoros           
    ##  8 Congo, Dem. Rep.  
    ##  9 Curacao           
    ## 10 Dominican Republic
    ## # … with 27 more rows

Because `impact_model_data` is the more limiting of the two data sources
(i.e., between `impact_model_data` and `university_enrollment_data`), in
that the lack of available LCA estimates for a country are more
intractable than the lack of available university-enrollment data, we
opt to systematically remove the countries with complete enrollment data
that are listed in `impact_model_data` but not
`university_enrollment_data` ahead of our join.

``` r
university_enrollment_data <- university_enrollment_data %>%
  filter_at(vars(country),all_vars(!. %in% c("Andorra","Aruba","Bahrain","Bhutan","Burundi","Chad","Comoros","Congo, Dem. Rep.","Curacao","Dominican Republic","Eritrea","Eswatini","Iraq","Korea, Dem. People's Rep.","Lao PDR","Lesotho","Libya","Liechtenstein","Marshall Islands","Monaco","Mozambique","Myanmar","Puerto Rico","Qatar","Samoa","San Marino","Seychelles","Singapore","Sint Maarten (Dutch part)","Sudan","Syrian Arab Republic","Tajikistan","Turkmenistan","Turks and Caicos Islands","Uzbekistan","Vietnam","West Bank and Gaza")))
```

Conversely, given that there is complete dietary-footprint for Taiwan
but an absence of enrollment data, we, instead, opt to systematically
add the required enrollment data for this nation-state using estimates
derived from official government ministries.

More specifically, we gathered the requisite national-population (i.e.,
`national_population`) and per-capita-GNI (i.e., `per_capita_gni`)
estimates from the “Principal Figures”
[extension](https://ws.dgbas.gov.tw/001/Upload/464/relfile/10320/2397/table_eng(050a).xlsx)
available on the “Statistical Tables”
[page](https://eng.stat.gov.tw/cp.aspx?n=2334) from Taiwan’s Statistical
Bureau, using the reference year 2019, which was the most recent year
that these indicators were available through EdStats. This yielded
values of `23596027` and `26561`, respectively.

In addition, using
[data](https://pop-proj.ndc.gov.tw/main_en/Custom_Detail_Search.aspx?t=1&n=175&sms=0)
from Taiwan’s National Development Council, we also extracted the
single-age population estimates in 2019 for the ages of 18 (`256704`),
19 (`305581`), 20 (`282248`), 21 (`266558`), 22 (`321943`), and 23
(`320796`), and summarized these values to derive the total school-aged
population (`1753830`).

Finally, we gather the relevant university-enrollment data for the same
2019 reference year from the [statistical
indicators](https://english.moe.gov.tw/cp-86-18943-e698b-1.html)
provided by Taiwan’s Ministry of Education. More specifically, we use
Indicator Tables
[52](https://stats.moe.gov.tw/files/ebook/indicators/52.pdf) and
[24](https://stats.moe.gov.tw/files/ebook/indicators/24.pdf) to directly
extract estimates for enrollment in ISCED 7 programs (`168203`) and and
ISCED 8 programs (`28510`), and deduce enrollment in ISCED 6 programs
(`1016459`) by subtracting the sum of all ISCED 7 and 8 enrollees
provided in Table 52 (`196713`) by the total number of college and
university students in Taiwan provided in Table 24 (`1213172`).

From here, we can directly extract `university_enrollment` (`1213172`),
which represents the total number of students enrolled within ISCED 6-8
programs in that country, and calculate `proportion_school_aged`
(`0.07432734333`) and `proportion_school_aged_enrolled` (`0.6917272484`)
by dividing the school-aged population by the national population, and
dividing the enrollment total by the school-aged population,
respectively.

``` r
university_enrollment_data <- university_enrollment_data %>%
  add_row(tibble_row(country="China, Taiwan Province of",
                     national_population=23596027,
                     national_population_ref_year=2019,
                     school_aged_population=1753830,
                     school_aged_population_ref_year=2019,
                     per_capita_gni=26561,
                     per_capita_gni_ref_year=2019,
                     isced_6_enrollment=1016459,
                     isced_6_ref_year=2019,
                     isced_7_enrollment=168203,
                     isced_7_ref_year=2019,
                     isced_8_enrollment=28510,
                     isced_8_ref_year=2019,
                     university_enrollment=1213172,
                     proportion_school_aged=0.07432734333,
                     proportion_school_aged_enrolled=0.6917272484))
```

With this complete, the country lists across both
`university_enrollment_data` and `dietary_footprint_data` should now be
the same, both in terms of the land areas they represent and include,
and the way in which those land areas are indexed and named.

There are a few final things we still need to address, namely the
instances where there are either presumed or confirmed instances of
missing data for individual indicators within
`university_enrollment_data`, as highlighted below.

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(national_population==0) %>%
  distinct(country)
```

    ## # A tibble: 0 × 1
    ## # Rowwise: 
    ## # … with 1 variable: country <chr>

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(school_aged_population==0) %>%
  distinct(country)
```

    ## # A tibble: 2 × 1
    ## # Rowwise: 
    ##   country
    ##   <chr>  
    ## 1 Japan  
    ## 2 Lebanon

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(per_capita_gni==0) %>%
  distinct(country)
```

    ## # A tibble: 0 × 1
    ## # Rowwise: 
    ## # … with 1 variable: country <chr>

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(isced_6_enrollment==0) %>%
  distinct(country)
```

    ## # A tibble: 8 × 1
    ## # Rowwise: 
    ##   country                           
    ##   <chr>                             
    ## 1 Barbados                          
    ## 2 Benin                             
    ## 3 Fiji                              
    ## 4 Malawi                            
    ## 5 Uganda                            
    ## 6 Uruguay                           
    ## 7 Venezuela (Bolivarian Republic of)
    ## 8 Yemen

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(isced_7_enrollment==0) %>%
  distinct(country)
```

    ## # A tibble: 9 × 1
    ## # Rowwise: 
    ##   country                           
    ##   <chr>                             
    ## 1 Algeria                           
    ## 2 Barbados                          
    ## 3 Belize                            
    ## 4 Fiji                              
    ## 5 Malawi                            
    ## 6 Uganda                            
    ## 7 Uruguay                           
    ## 8 Venezuela (Bolivarian Republic of)
    ## 9 Yemen

``` r
university_enrollment_data %>%
  rowwise() %>%
  filter(isced_8_enrollment==0) %>%
  distinct(country)
```

    ## # A tibble: 3 × 1
    ## # Rowwise: 
    ##   country
    ##   <chr>  
    ## 1 Algeria
    ## 2 Belize 
    ## 3 Jamaica

However, for now, we can continue by joining these two data objects now
that we have harmonized them across their corresponding `country`
vectors.

``` r
university_impact_model <- left_join(university_enrollment_data,impact_model_data,by="country")
```

As we can see, apart from some of the indicator-level omissions
specified above, we have mostly complete data for the 120 countries.

Before making systematized decisions about whether to patch in these
specific data points for countries with missing data, as we did for
Taiwan, or omit them from our analyses entirely, we can generate a
series of additional variables to represent the technical potential of
shifting university-enrolled populations toward different dietary
patterns.

We begin this process by generating a dozen variables that scale the per
capita emissions estimates across each included dietary pattern for each
country according to its enrollment-population size.

``` r
university_impact_model <- university_impact_model %>%
  mutate(baseline_population_kg_co2e=baseline_per_capita_kg_co2e*university_enrollment) %>%
  mutate(baseline_adjusted_population_kg_co2e=baseline_adjusted_per_capita_kg_co2e*university_enrollment) %>%
  mutate(baseline_oecd_population_kg_co2e=baseline_oecd_per_capita_kg_co2e*university_enrollment) %>%
  mutate(meatless_day_population_kg_co2e=meatless_day_per_capita_kg_co2e*university_enrollment) %>%
  mutate(low_red_meat_population_kg_co2e=low_red_meat_per_capita_kg_co2e*university_enrollment) %>%
  mutate(no_red_meat_population_kg_co2e=no_red_meat_per_capita_kg_co2e*university_enrollment) %>%
  mutate(no_dairy_population_kg_co2e=no_dairy_per_capita_kg_co2e*university_enrollment) %>%
  mutate(pescetarian_population_kg_co2e=pescetarian_per_capita_kg_co2e*university_enrollment) %>%
  mutate(lacto_ovo_vegetarian_population_kg_co2e=lacto_ovo_vegetarian_per_capita_kg_co2e*university_enrollment) %>%
  mutate(eat_lancet_population_kg_co2e=eat_lancet_per_capita_kg_co2e*university_enrollment) %>%
  mutate(two_thirds_vegan_population_kg_co2e=two_thirds_vegan_per_capita_kg_co2e*university_enrollment) %>%
  mutate(vegan_population_kg_co2e=vegan_per_capita_kg_co2e*university_enrollment)
```

With these variables in place, we can additionally generate a set of
variables to represent the reductions that we can expect from shifting a
country’s university-enrolled population from its observed baseline
dietary pattern to each of the 11 modeled scenarios. We express these
reductions both in absolute (i.e., subtracting the modeled estimate from
the baseline estimate) and relative terms (i.e., taking the absolute
reduction and dividing that difference by the baseline estimate).

``` r
university_impact_model <- university_impact_model %>%
  mutate(baseline_population_reduction_kg_co2e=-(baseline_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(baseline_adjusted_population_reduction_kg_co2e=-(baseline_adjusted_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(baseline_oecd_population_reduction_kg_co2e=-(baseline_oecd_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(meatless_day_population_reduction_kg_co2e=-(meatless_day_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(low_red_meat_population_reduction_kg_co2e=-(low_red_meat_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(no_red_meat_population_reduction_kg_co2e=-(no_red_meat_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(no_dairy_population_reduction_kg_co2e=-(no_dairy_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(pescetarian_population_reduction_kg_co2e=-(pescetarian_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(lacto_ovo_vegetarian_population_reduction_kg_co2e=-(lacto_ovo_vegetarian_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(eat_lancet_population_reduction_kg_co2e=-(eat_lancet_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(two_thirds_vegan_population_reduction_kg_co2e=-(two_thirds_vegan_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(vegan_population_reduction_kg_co2e=-(vegan_population_kg_co2e-baseline_population_kg_co2e)) %>%
  mutate(baseline_population_percent_reduction_kg_co2e=baseline_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(baseline_adjusted_population_percent_reduction_kg_co2e=baseline_adjusted_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(baseline_oecd_population_percent_reduction_kg_co2e=baseline_oecd_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(meatless_day_population_percent_reduction_kg_co2e=meatless_day_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(low_red_meat_population_percent_reduction_kg_co2e=low_red_meat_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(no_red_meat_population_percent_reduction_kg_co2e=no_red_meat_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(no_dairy_population_percent_reduction_kg_co2e=no_dairy_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(pescetarian_population_percent_reduction_kg_co2e=pescetarian_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(lacto_ovo_vegetarian_population_percent_reduction_kg_co2e=lacto_ovo_vegetarian_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(eat_lancet_population_percent_reduction_kg_co2e=eat_lancet_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(two_thirds_vegan_population_percent_reduction_kg_co2e=two_thirds_vegan_population_reduction_kg_co2e/baseline_population_kg_co2e) %>%
  mutate(vegan_population_percent_reduction_kg_co2e=vegan_population_reduction_kg_co2e/baseline_population_kg_co2e)
```

With these new variables now newly incorporated into
`university_impact_model`, we can now proceed by making the final
preparations needed ahead of our spatial join. More specifically, we
will first need to decide how we want to address the rows we had
previously identified that have missing data for indicator-specific
variables.

These include the two countries with no available data for
`school_aged_population` (i.e., Japan and Lebanon), the eight countries
with no available data for `isced_6_enrollment` (i.e., Barbados, Benin,
Fiji, Malawi, Uganda, Uruguay, Venezuela, and Yemen), the nine countries
with no available data for `isced_7_enrollment` (Algeria, Barbados,
Belize, Fiji, Malawi, Uganda, Uruguay, Venezuela, and Yemen), and the
three countries with no available data for `isced_8_enrollment` (i.e.,
Algeria, Belize, and Jamaica).

These 22 instances of missing data correspond to 13 unique countries:
Algeria (i.e., `isced_7_enrollment` and `isced_8_enrollment`, Barbados
(i.e., `isced_6_enrollment` and `isced_7_enrollment`), Belize (i.e.,
`isced_7_enrollment` and `isced_8_enrollment`), Benin (i.e.,
`isced_6_enrollment`), Fiji (i.e., `isced_6_enrollment` and
`isced_7_enrollment`), Jamaica (i.e., `isced_8_enrollment`), Japan
(`school_aged_population`), Lebanon (`school_aged_population`), Malawi
(i.e., `isced_6_enrollment` and `isced_7_enrollment`), Uganda (i.e.,
`isced_6_enrollment` and `isced_7_enrollment`), Uruguay (i.e.,
`isced_6_enrollment` and `isced_7_enrollment`), Venezuela (i.e.,
`isced_6_enrollment` and `isced_7_enrollment`), and Yemen
(`isced_6_enrollment` and `isced_7_enrollment`).

``` r
university_impact_model %>%
  filter(country=="Algeria"|country=="Barbados"|country=="Belize"|country=="Benin"|country=="Fiji"|country=="Jamaica"|country=="Japan"|country=="Lebanon"|country=="Malawi"|country=="Uganda"|country=="Uruguay"|country=="Venezuela (Bolivarian Republic of)"|country=="Yemen")
```

    ## # A tibble: 13 × 64
    ## # Rowwise: 
    ##    country       natio…¹ natio…² schoo…³ schoo…⁴ per_c…⁵ per_c…⁶ isced…⁷ isced…⁸
    ##    <chr>           <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Algeria        4.31e7    2019 3116237    2018    3920    2017  996087    2018
    ##  2 Barbados       2.87e5    2019   18979    2015   15410    2018       0       0
    ##  3 Belize         3.9 e5    2019   39334    2019    4470    2018    5383    2019
    ##  4 Benin          1.18e7    2019 1057994    2018     870    2018       0       0
    ##  5 Fiji           8.9 e5    2019   74034    2016    5860    2018       0       0
    ##  6 Jamaica        2.95e6    2019  263211    2019    4970    2018   31487    2019
    ##  7 Japan          1.26e8    2019       0       0   41310    2018 2674263    2018
    ##  8 Lebanon        6.86e6    2019       0       0    7920    2018  191885    2019
    ##  9 Malawi         1.86e7    2019 1594473    2014     360    2018       0       0
    ## 10 Uganda         4.43e7    2019 3419644    2014     620    2018       0       0
    ## 11 Uruguay        3.46e6    2019  257364    2017   15650    2018       0       0
    ## 12 Venezuela (B…  2.85e7    2019 2660784    2015   13080    2014       0       0
    ## 13 Yemen          2.92e7    2019 2877905    2014    1460    2014       0       0
    ## # … with 55 more variables: isced_7_enrollment <dbl>, isced_7_ref_year <dbl>,
    ## #   isced_8_enrollment <dbl>, isced_8_ref_year <dbl>,
    ## #   university_enrollment <dbl>, proportion_school_aged <dbl>,
    ## #   proportion_school_aged_enrolled <dbl>, baseline_per_capita_kg_co2e <dbl>,
    ## #   baseline_adjusted_per_capita_kg_co2e <dbl>,
    ## #   baseline_oecd_per_capita_kg_co2e <dbl>,
    ## #   meatless_day_per_capita_kg_co2e <dbl>, …

Using the same conventions as we did when populating demographic data
for Taiwan, we will begin by trying to find single-year age data from
official government bureaus or ministries to fill in data for Japan and
Lebanon’s school-age-population, which UNESCO defines as ages 18-23 for
tertiary education.

In the case of Japan, we were able to extract this information from the
[2020 Population
Census](https://www.stat.go.jp/english/data/kokusei/index.html) reported
by the [Statistics Bureau of
Japan](https://www.stat.go.jp/english/index.html).

More specifically, after selecting the set of indicators available
within the [Basic Complete Tabulation on Population and
Households](https://www.stat.go.jp/english/data/kokusei/2020/summary.html),
we referenced [Table
2-1](https://www.e-stat.go.jp/en/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&year=20200&month=24101210&tclass1=000001136466),
which can also be downloaded as a spreadsheet
[here](https://www.e-stat.go.jp/en/stat-search/file-download?statInfId=000032142404&fileKind=0),
to retrieve the single-year age populations for the whole country at age
18 (`1151389`), 19 (`1159285`), 20 (`1177049`), 21 (`1174456`), 22
(`1193935`), and 23 (`1193983`). These values yield a total school-aged
population of `7050097` for reference year 2020, as single-year age data
was unavailable for the year 2019.

However, given that the data representing this variable for the
remaining countries reflect population estimates rather than complete
population counts, we will instead use this as a reference for the
[IDB](https://www.census.gov/data-tools/demo/idb/#/dashboard?COUNTRY_YEAR=2024&COUNTRY_YR_ANIM=2024)-provided
single-year-age-group estimates.

When looking at the age tables for Japan for the selected reference year
of 2019, we see that the population aged 18 (`1185752`), 19 (`1184102`),
20 (`1221306`), 21 (`1226508`), 22 (`1231375`), and 23 (`1245298`) sum
to `7294341`.

``` r
university_impact_model <- university_impact_model %>%
  mutate(school_aged_population=replace(school_aged_population,country=="Japan",7294341)) %>%
  mutate(school_aged_population_ref_year=replace(school_aged_population_ref_year,country=="Japan",2019))
```

Using the same resource, we can see that for the selected reference year
of 2019, the Lebanese population aged 18 (`89824`), 19 (`94964`), 20
(`89747`), 21 (`88918`), 22 (`89575`), and 23 (`90416`) sum to a total
of `543444`.

``` r
university_impact_model <- university_impact_model %>%
  mutate(school_aged_population=replace(school_aged_population,country=="Lebanon",543444)) %>%
  mutate(school_aged_population_ref_year=replace(school_aged_population_ref_year,country=="Lebanon",2019))
```

For reasons of consistency, we can also optionally elect to update the
single-year-age-group estimates comprising the `school_aged_population`
indicator for Taiwan using the values provided by the IDB. As a
reminder, this data point was previously derived from the [National
Development
Council](https://pop-proj.ndc.gov.tw/main_en/Custom_Detail_Search.aspx?t=1&n=175&sms=0)
of Taiwan, also for reference year 2019, with the `1753830` sum being
composed of single-age populations of `256704`, `305581`, `282248`,
`266558`, `321943`, and `320796` for aged 18, 19, 20, 21, 22, and 23,
respectively.

The IDB-provided values, on the other hand, only differ slightly as
follows: 18 (`282626`), 19 (`296353`), 20 (`276860`), 21 (`296251`), 22
(`323160`), and 23 (`324116`).

Again, for consistency and parsimony reasons, we will choose to plug in
this new sum of `1799366` for Taiwan.

``` r
university_impact_model <- university_impact_model %>%
  mutate(school_aged_population=replace(school_aged_population,country=="Taiwan",1799366)) %>%
  mutate(school_aged_population_ref_year=replace(school_aged_population_ref_year,country=="Taiwan",2019))
```

With these missing-data issues addressed, we can now re-run these
commands so that the calculations apply, too, to the recently input
value:

``` r
university_impact_model <- university_impact_model %>%
  mutate(university_enrollment=isced_6_enrollment+isced_7_enrollment+isced_8_enrollment) %>%
  mutate(proportion_school_aged=school_aged_population/national_population) %>%
  mutate(proportion_school_aged_enrolled=university_enrollment/school_aged_population)
```

Now, of the originally identified 22 missing-data instances impacting
the data integrity of 13 countries, we have resolved two instances to
yield complete data for another two countries.

Of the 20 remaining instances of missing data, which now impact the data
integrity of 11 countries, all pertain to country-level enrollment
estimates in ISCED 6, 7, and 8 programs.

- Algeria (i.e., `isced_7_enrollment` and `isced_8_enrollment`)
- Barbados (i.e., `isced_6_enrollment` and `isced_7_enrollment`)
- Belize (i.e., `isced_7_enrollment` and `isced_8_enrollment`)
- Benin (i.e., `isced_6_enrollment`)
- Fiji (i.e., `isced_6_enrollment` and `isced_7_enrollment`)
- Jamaica (i.e., `isced_8_enrollment`)
- Malawi (i.e., `isced_6_enrollment` and `isced_7_enrollment`)
- Uganda (i.e., `isced_6_enrollment` and `isced_7_enrollment`)
- Uruguay (i.e., `isced_6_enrollment` and `isced_7_enrollment`)
- Venezuela (i.e., `isced_6_enrollment` and `isced_7_enrollment`)
- Yemen (`isced_6_enrollment` and `isced_7_enrollment`).

As a reminder, we had originally programmed `university_enrollment_data`
such that all cases where `isced_6_enrollment` + `isced_7_enrollment` +
`isced_8_enrollment` = 0 were filtered out of the dataset.

These remaining 11 countries, by contrast, represent any occasion where
enrollment in any particular set of programs was equivalent to zero.

``` r
university_impact_model %>%
  filter(country=="Algeria"|country=="Barbados"|country=="Belize"|country=="Benin"|country=="Fiji"|country=="Jamaica"|country=="Malawi"|country=="Uganda"|country=="Uruguay"|country=="Venezuela (Bolivarian Republic of)"|country=="Yemen") %>%
  select(country,university_enrollment,isced_6_ref_year,isced_6_enrollment,isced_7_ref_year,isced_7_enrollment,isced_8_ref_year,isced_8_enrollment)
```

    ## # A tibble: 11 × 8
    ## # Rowwise: 
    ##    country               unive…¹ isced…² isced…³ isced…⁴ isced…⁵ isced…⁶ isced…⁷
    ##    <chr>                   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Algeria                9.96e5    2018  996087       0       0       0     0  
    ##  2 Barbados               1.33e2       0       0       0       0    2011   133  
    ##  3 Belize                 5.38e3    2019    5383       0       0       0     0  
    ##  4 Benin                  1.47e4       0       0    2018   12657    2018  2060  
    ##  5 Fiji                   1.62e2       0       0       0       0    2005   162  
    ##  6 Jamaica                3.44e4    2019   31487    2018    2894       0     0  
    ##  7 Malawi                 1.54e2       0       0       0       0    2010   154  
    ##  8 Uganda                 2.19e3       0       0       0       0    2004  2194  
    ##  9 Uruguay                9.10e1       0       0       0       0    2006    91.0
    ## 10 Venezuela (Bolivaria…  5.72e3       0       0       0       0    2008  5718  
    ## 11 Yemen                  6.4 e1       0       0       0       0    2007    64  
    ## # … with abbreviated variable names ¹​university_enrollment, ²​isced_6_ref_year,
    ## #   ³​isced_6_enrollment, ⁴​isced_7_ref_year, ⁵​isced_7_enrollment,
    ## #   ⁶​isced_8_ref_year, ⁷​isced_8_enrollment

Based on this, we need to construct a systematized process to separate
the possible assumptions: (1) that these data accurately represent null
levels of ISCED-program enrollment at the country level, (2) that these
data are incomplete and provide enrollment estimates for some programs
and not others, or (3) that the enrollment totals across programs are
accurate but are aggregated incorrectly within specific program types.

To add another layer of complexity, it is also possible that the most
appropriate assumption may vary across countries. Given this, we will
take the most conservative approach and assume that these EdStats
indicators precisely reflect enrollment estimates across these divisions
of ISCED programs but will caution in the manuscript that, specifically
for these 11 countries, that these figures may be underreporting total
enrollment across national colleges and universities.

Now, we move on to the final steps ahead of our spatila join by matching
the 120 land areas represented in the newly merged and corrected
`university_impact_model` and the 258 land areas represented in
`shapefile_data`.

``` r
anti_join(university_impact_model,shapefile_data,by="country") %>% distinct(country) %>% arrange(country)
```

    ## # A tibble: 17 × 1
    ## # Rowwise: 
    ##    country                           
    ##    <chr>                             
    ##  1 CÃ´te d'Ivoire                    
    ##  2 Cabo Verde                        
    ##  3 China, Hong Kong SAR              
    ##  4 China, Macao SAR                  
    ##  5 China, mainland                   
    ##  6 China, Taiwan Province of         
    ##  7 Congo                             
    ##  8 Czechia                           
    ##  9 Iran (Islamic Republic of)        
    ## 10 Kyrgyzstan                        
    ## 11 Republic of Korea                 
    ## 12 Republic of Moldova               
    ## 13 Russian Federation                
    ## 14 Slovakia                          
    ## 15 United Republic of Tanzania       
    ## 16 United States of America          
    ## 17 Venezuela (Bolivarian Republic of)

By identifying the 17 countries that appear in `university_impact_model`
without a one-to-one match in `shapefile_data`, we can see that all of
these mismatches are a product of inconsistent naming. Said differently,
all land areas found in `university_impact_model` are represented
spatially within `shapefile_data`, just using different conventions.

We correct this in two steps using the following:

``` r
university_impact_model <- university_impact_model %>%
  mutate(across(country,str_replace,"China, mainland","China")) %>%
  mutate(across(country,str_replace,"China, Hong Kong SAR","Hong Kong")) %>%
  mutate(across(country,str_replace,"China, Macao SAR","Macao")) %>%
  mutate(across(country,str_replace,"China, Taiwan Province of","Taiwan"))
```

``` r
shapefile_data <- shapefile_data %>%
  mutate(across(country,str_replace,"Cote d'Ivoire","CÃ´te d'Ivoire")) %>%
  mutate(across(country,str_replace,"Cape Verde","Cabo Verde")) %>%
  mutate(across(country,str_replace,"Hong Kong, China","Hong Kong")) %>%
  mutate(across(country,str_replace,"Macao, China","Macao")) %>%
  mutate(across(country,str_replace,"Congo, Rep.","Congo")) %>%
  mutate(across(country,str_replace,"Czech Republic","Czechia")) %>%
  mutate(across(country,str_replace,"Iran","Iran (Islamic Republic of)")) %>%
  mutate(across(country,str_replace,"Krygyz Republic","Kyrgyzstan")) %>%
  mutate(across(country,str_replace,"South Korea","Republic of Korea")) %>%
  mutate(across(country,str_replace,"Moldova","Republic of Moldova")) %>%
  mutate(across(country,str_replace,"Russia","Russian Federation")) %>%
  mutate(across(country,str_replace,"Slovak Republic","Slovakia")) %>%
  mutate(across(country,str_replace,"Tanzania","United Republic of Tanzania")) %>%
  mutate(across(country,str_replace,"United States","United States of America")) %>%
  mutate(across(country,str_replace,"Venezuela","Venezuela (Bolivarian Republic of)"))
```

As a final step before our spatial join, we will input the necessary
information in order to make the requisite classifications for our
comparative analysis. More specifically, we will sort nation-states into
groups according to the different development and lending groups that
fall into.

Lending-group boundaries were determine via
[this](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups)
2024 classification scheme from the World Bank (i.e., [the Atlas
method](https://datahelpdesk.worldbank.org/knowledgebase/articles/378832-the-world-bank-atlas-method-detailed-methodology)),
which defines low-income economies as those with per capita GNI
estimates of less 1,135 USD or less, lower-middle-income economies as
those between 1,136 and 4,465, upper-middle-income economies as those
between 4,466 and 13,845, and high-income economies as those equivalent
to or exceeding 13,846.

``` r
university_impact_model <- university_impact_model %>%
  mutate(lending_group=case_when(per_capita_gni<=1135~"Low",
                                 per_capita_gni>=1136&per_capita_gni<=4465~"Lower-Middle",
                                 per_capita_gni>=4466&per_capita_gni<=13845~"Upper-Middle",
                                 per_capita_gni>=13846~"High")) 
```

Unlike with the identified lending-group categories, development-group
membership can be determined using multiple indicators. The
Human-Development Index (HDI), for example, is a quality-of-life metric
generated by the United Nations that consider various dimensions of
human achievement. Alterantively, researchers have also used the Brandt
Line, which strictly uses per capita GDP as a proxy measure for economic
growth.

Because the HDI provides a summary measure of human development across
the dimensions of health (i.e., life expectancy at birth), education
(i.e., expected years of schooling and mean years of schooling), and
living standards (i.e., GNI per capita), we will select this method, as
it takes into consideration a broader view of development.

More specifically, we will first input the corresponding HDI values
provided by the United Nations
[2022](https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Statistical_Annex_HDI_Table.xlsx)
Human Development Report, downloaded directly from
[here](https://hdr.undp.org/data-center/human-development-index#/indicies/HDI).

However, since this data source leaves out estimates for the Special
Administrative Region of Macao and the nation-state of Taiwan—neither of
which are member states to the United Nations—we supplement this data
with the information provided directly from both the Statistical Bureau
of Taiwan and the Statistics and Census Service of the Government of
Macao Special Administrative Region.

Using the same Development-Report methodology for the same reference
year, we can see the provided estimate for
[Taiwan](https://win.dgbas.gov.tw/eyimc/ebook/SB/statistcs-brief_opf_files/pdfs/statistcs-brief__.pdf)
and
[Macao](https://www.dsec.gov.mo/getAttachment/ab7d3848-5edc-4947-9d2a-8e8a97b8a7c8/C_MN_PUB_2024_Y.aspx)
are both equivalent to 0.925.

``` r
university_impact_model <- university_impact_model %>%
  mutate(development_index_score=case_when(country=="Afghanistan"~0.462,country=="Albania"~0.789,country=="Algeria"~0.745,country=="Argentina"~0.849,country=="Armenia"~0.786,country=="Australia"~0.946,country=="Austria"~0.926,country=="Azerbaijan"~0.760,country=="Barbados"~0.809,country=="Belarus"~0.801,country=="Belgium"~0.942,country=="Belize"~0.700,country=="Benin"~0.504,country=="Bosnia and Herzegovina"~0.779,country=="Botswana"~0.708,country=="Brazil"~0.760,country=="Bulgaria"~0.799,country=="Burkina Faso"~0.438,country=="Cabo Verde"~0.661,country=="Cambodia"~0.600,country=="Cameroon"~0.587,country=="Canada"~0.935,country=="Chile"~0.860,country=="China"~0.788,country=="Colombia"~0.758,country=="Congo"~0.593,country=="Costa Rica"~0.806,country=="CÃ´te d'Ivoire"~0.534,country=="Croatia"~0.878,country=="Cyprus"~0.907,country=="Czechia"~0.895,country=="Denmark"~0.952,country=="Ecuador"~0.765,country=="Egypt"~0.728,country=="El Salvador"~0.674,country=="Estonia"~0.899,country=="Ethiopia"~0.492,country=="Fiji"~0.729,country=="Finland"~0.942,country=="France"~0.910,country=="Georgia"~0.814,country=="Germany"~0.950,country=="Ghana"~0.602,country=="Greece"~0.893,country=="Guatemala"~0.629,country=="Honduras"~0.624,country=="Hong Kong"~0.956,country=="Hungary"~0.851,country=="Iceland"~0.959,country=="India"~0.644,country=="Indonesia"~0.713,country=="Iran (Islamic Republic of)"~0.780,country=="Ireland"~0.950,country=="Israel"~0.915,country=="Italy"~0.906,country=="Jamaica"~0.706,country=="Japan"~0.920,country=="Jordan"~0.736,country=="Kazakhstan"~0.802,country=="Kenya"~0.601,country=="Republic of Korea"~0.929,country=="Kuwait"~0.847,country=="Kyrgyzstan"~0.701,country=="Latvia"~0.879,country=="Lebanon"~0.723,country=="Lithuania"~0.879,country=="Luxembourg"~0.927,country=="Macao"~0.925,country=="Madagascar"~0.487,country=="Malawi"~0.508,country=="Malaysia"~0.807,country=="Maldives"~0.762,country=="Mali"~0.410,country=="Malta"~0.915,country=="Mauritania"~0.540,country=="Mauritius"~0.796,country=="Mexico"~0.781,country=="Republic of Moldova"~0.763,country=="Montenegro"~0.844,country=="Morocco"~0.698,country=="Namibia"~0.610,country=="Nepal"~0.601,country=="Netherlands"~0.946,country=="New Zealand"~0.939,country=="Niger"~0.394,country=="Norway"~0.966,country=="Oman"~0.819,country=="Pakistan"~0.540,country=="Panama"~0.820,country=="Peru"~0.762,country=="Philippines"~0.710,country=="Poland"~0.881,country=="Portugal"~0.874,country=="Romania"~0.827,country=="Russian Federation"~0.821,country=="Rwanda"~0.548,country=="Saudi Arabia"~0.875,country=="Senegal"~0.517,country=="Serbia"~0.805,country=="Slovakia"~0.855,country=="Slovenia"~0.926,country=="South Africa"~0.717,country=="Spain"~0.911,country=="Sri Lanka"~0.780,country=="Sweden"~0.952,country=="Switzerland"~0.967,country=="United Republic of Tanzania"~0.532,country=="Thailand"~0.803,country=="Togo"~0.547,country=="Tunisia"~0.732,country=="Turkey"~0.855,country=="Uganda"~0.550,country=="Ukraine"~0.734,country=="United Kingdom"~0.940,country=="United States of America"~0.927,country=="Uruguay"~0.830,country=="Venezuela (Bolivarian Republic of)"~0.699,country=="Yemen"~0.424,country=="Zimbabwe"~0.550,country=="Taiwan"~0.925))
```

Using the categorical boundaries described in the 2022 Development
Report—whereby nation-states with an HDI of less than 0.550 are
considered low, more than or equal to 0.550 but equal to or less than
0.699 are considered medium, more than or equal to 0.700 but equal to or
less than 0.799 are considered high, and more than or equal to 0.800 are
considered high—we construct a new variable that classifies
nation-states according to these parameters here:

``` r
university_impact_model <- university_impact_model %>%
  mutate(development_group=case_when(development_index_score<0.550~"Low",
                                     development_index_score>=0.550&development_index_score<=0.699~"Medium",
                                     development_index_score>=0.700&development_index_score<=0.799~"High",
                                     development_index_score>=0.800~"Very High")) 
```

In addition to this, using the guidance provided by the [the World
Population
Review](https://worldpopulationreview.com/country-rankings/global-north-countries),
we can further classify these groupings into nation-states of the Global
North and nation-states of the Global South by dichotomizing the
entities based on whether they are associated with HDIs of 0.800 and
above or not.

``` r
university_impact_model <- university_impact_model %>% mutate(development_group_dichotomy=case_when(development_group=="Low"~"Global South",
                                                                         development_group=="Medium"~"Global South",
                                                                         development_group=="High"~"Global South",
                                                                         development_group=="Very High"~"Global North"))
```

Finally, we can do the same for larger lending-group classification
scheme by grouping together low- and lower-middle-income nation-states
as lower-income and upper-middle- and high-income nation-states as
higher-income.

``` r
university_impact_model <- university_impact_model %>% mutate(lending_group_dichotomy=case_when(lending_group=="Low"~"Lower",
                                                                         lending_group=="Lower-Middle"~"Lower",
                                                                         lending_group=="Upper-Middle"~"Higher",
                                                                         lending_group=="High"~"Higher"))
```

To aid with future visualizations, and also for consistency purposes, we
can now also take the opportunity to both adjust any naming
discrepancies between the listed nation-states and how the published ISO
3166 [standards](https://www.iso.org/obp/ui/#search/code/), and add the
abbreviated and corresponding Alpha-3 codes that reference each of our
included nation-states, as shown below.

- Alpha-3 Codes

``` r
university_impact_model <- university_impact_model %>%
  mutate(country_alpha_three=case_when(country=="Afghanistan"~"AFG",country=="Albania"~"ALB",country=="Algeria"~"DZA",country=="Argentina"~"ARG",country=="Armenia"~"ARM",country=="Australia"~"AUS",country=="Austria"~"AUT",country=="Azerbaijan"~"AZE",country=="Barbados"~"BRB",country=="Belarus"~"BLR",country=="Belgium"~"BEL",country=="Belize"~"BLZ",country=="Benin"~"BEN",country=="Bosnia and Herzegovina"~"BIH",country=="Botswana"~"BWA",country=="Brazil"~"BRA",country=="Bulgaria"~"BGR",country=="Burkina Faso"~"BFA",country=="Cabo Verde"~"CPV",country=="Cambodia"~"KHM",country=="Cameroon"~"CMR",country=="Canada"~"CAN",country=="Chile"~"CHL",country=="China"~"CHN",country=="Colombia"~"COL",country=="Congo"~"COG",country=="Costa Rica"~"CRI",country=="CÃ´te d'Ivoire"~"CIV",country=="Croatia"~"HRV",country=="Cyprus"~"CYP",country=="Czechia"~"CZE",country=="Denmark"~"DNK",country=="Ecuador"~"ECU",country=="Egypt"~"EGY",country=="El Salvador"~"SLV",country=="Estonia"~"EST",country=="Ethiopia"~"ETH",country=="Fiji"~"FJI",country=="Finland"~"FIN",country=="France"~"FRA",country=="Georgia"~"GEO",country=="Germany"~"DEU",country=="Ghana"~"GHA",country=="Greece"~"GRC",country=="Guatemala"~"GTM",country=="Honduras"~"HND",country=="Hong Kong"~"HKG",country=="Hungary"~"HUN",country=="Iceland"~"ISL",country=="India"~"IND",country=="Indonesia"~"IDN",country=="Iran (Islamic Republic of)"~"IRN",country=="Ireland"~"IRL",country=="Israel"~"ISR",country=="Italy"~"ITA",country=="Jamaica"~"JAM",country=="Japan"~"JPN",country=="Jordan"~"JOR",country=="Kazakhstan"~"KAZ",country=="Kenya"~"KEN",country=="Republic of Korea"~"KOR",country=="Kuwait"~"KWT",country=="Kyrgyzstan"~"KGZ",country=="Latvia"~"LVA",country=="Lebanon"~"LBN",country=="Lithuania"~"LTU",country=="Luxembourg"~"LUX",country=="Macao"~"MAC",country=="Madagascar"~"MDG",country=="Malawi"~"MWI",country=="Malaysia"~"MYS",country=="Maldives"~"MDV",country=="Mali"~"MLI",country=="Malta"~"MLT",country=="Mauritania"~"MRT",country=="Mauritius"~"MUS",country=="Mexico"~"MEX",country=="Republic of Moldova"~"MDA",country=="Montenegro"~"MNE",country=="Morocco"~"MAR",country=="Namibia"~"NAM",country=="Nepal"~"NPL",country=="Netherlands"~"NLD",country=="New Zealand"~"NZL",country=="Niger"~"NER",country=="Norway"~"NOR",country=="Oman"~"OMN",country=="Pakistan"~"PAK",country=="Panama"~"PAN",country=="Peru"~"PER",country=="Philippines"~"PHL",country=="Poland"~"POL",country=="Portugal"~"PRT",country=="Romania"~"ROU",country=="Russian Federation"~"RUS",country=="Rwanda"~"RWA",country=="Saudi Arabia"~"SAU",country=="Senegal"~"SEN",country=="Serbia"~"SRB",country=="Slovakia"~"SVK",country=="Slovenia"~"SVN",country=="South Africa"~"ZAF",country=="Spain"~"ESP",country=="Sri Lanka"~"LKA",country=="Sweden"~"SWE",country=="Switzerland"~"CHE",country=="United Republic of Tanzania"~"TZA",country=="Thailand"~"THA",country=="Togo"~"TGO",country=="Tunisia"~"TUN",country=="Turkey"~"TUR",country=="Uganda"~"UGA",country=="Ukraine"~"UKR",country=="United Kingdom"~"GBR",country=="United States of America"~"USA",country=="Uruguay"~"URY",country=="Venezuela (Bolivarian Republic of)"~"VEN",country=="Yemen"~"YEM",country=="Zimbabwe"~"ZWE",country=="Taiwan"~"TWN"))
```

- Name Alignment

Because the names have yet to be adjusted within our shape-file data, we
will wait to do this step until after the join has been done.

With these last few steps complete, we can now finally proceed with our
spatial join:

``` r
global_university_impact_model <- left_join(shapefile_data,university_impact_model,by="country")
```

Now that the spatial join has been completed, we can align the matched
nation-state names to the shortened ISO 3166 names.

``` r
university_impact_model <- university_impact_model %>%
  mutate(across(country,str_replace,"Congo","Congo (the)")) %>%
  mutate(across(country,str_replace,"CÃ´te d'Ivoire","Côte d'Ivoire")) %>%
  mutate(across(country,str_replace,"Republic of Korea","Korea (the Republic of)")) %>%
  mutate(across(country,str_replace,"Republic of Moldova","Moldova (the Republic of)")) %>%
  mutate(across(country,str_replace,"Netherlands","Netherlands (Kingdom of the)")) %>%
  mutate(across(country,str_replace,"Niger","Niger (the)")) %>%
  mutate(across(country,str_replace,"Philippines","Philippines (the)")) %>%
  mutate(across(country,str_replace,"Russian Federation","Russidan Federation (the)")) %>%
  mutate(across(country,str_replace,"United Republic of Tanzania","Tanzania (the United Republic of)")) %>%
  mutate(across(country,str_replace,"Turkey","Türkiye")) %>%
  mutate(across(country,str_replace,"United Kingdom","United Kingdom of Great Britain and Northern Ireland (the)")) %>%
  mutate(across(country,str_replace,"United States of America","United States of America (the)")) 
global_university_impact_model <- global_university_impact_model %>%
  mutate(across(country,str_replace,"Congo","Congo (the)")) %>%
  mutate(across(country,str_replace,"CÃ´te d'Ivoire","Côte d'Ivoire")) %>%
  mutate(across(country,str_replace,"Republic of Korea","Korea (the Republic of)")) %>%
  mutate(across(country,str_replace,"Republic of Moldova","Moldova (the Republic of)")) %>%
  mutate(across(country,str_replace,"Netherlands","Netherlands (Kingdom of the)")) %>%
  mutate(across(country,str_replace,"Niger","Niger (the)")) %>%
  mutate(across(country,str_replace,"Philippines","Philippines (the)")) %>%
  mutate(across(country,str_replace,"Russian Federation","Russidan Federation (the)")) %>%
  mutate(across(country,str_replace,"United Republic of Tanzania","Tanzania (the United Republic of)")) %>%
  mutate(across(country,str_replace,"Turkey","Türkiye")) %>%
  mutate(across(country,str_replace,"United Kingdom","United Kingdom of Great Britain and Northern Ireland (the)")) %>%
  mutate(across(country,str_replace,"United States of America","United States of America (the)")) 
```

``` r
university_impact_model %>%
  filter(if_any(everything(),is.na))
```

    ## # A tibble: 0 × 70
    ## # Rowwise: 
    ## # … with 70 variables: country <chr>, national_population <dbl>,
    ## #   national_population_ref_year <dbl>, school_aged_population <dbl>,
    ## #   school_aged_population_ref_year <dbl>, per_capita_gni <dbl>,
    ## #   per_capita_gni_ref_year <dbl>, isced_6_enrollment <dbl>,
    ## #   isced_6_ref_year <dbl>, isced_7_enrollment <dbl>, isced_7_ref_year <dbl>,
    ## #   isced_8_enrollment <dbl>, isced_8_ref_year <dbl>,
    ## #   university_enrollment <dbl>, proportion_school_aged <dbl>, …

## Writing the Final Data Files

For our last step, we will export these final data files as CSVs in our
repository using the following:

``` r
write.csv(university_impact_model,"~/github/university-impact-model/data/model-output/university-impact-model.csv")
write.csv(global_university_impact_model,"~/github/university-impact-model/data/model-output/university-impact-model-shapefile.csv")
```

In addition to these two data files—which differ only in whether or not
they contain the shape-file information, with
`university-impact-model.csv` omitting it for ease of downstream
analysis and `university-impact-model-shapefile.csv` containing it for
data-visualization purposes—we also will generate a supplementary data
file detailing the list of countries included.

``` r
country_list <- university_impact_model %>% distinct(country) %>% arrange(country)
write.csv(country_list,"~/github/university-impact-model/data/model-output/country-list.csv")
```
