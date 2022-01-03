Merge Data
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(mice)
```

    ## 
    ## Attaching package: 'mice'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

``` r
life_expectancy = 
  read_csv("data/life_expectancy.csv") %>% 
  janitor::clean_names() %>% 
  mutate(country = recode(country,
      "AntiguaandBarbuda" = "Antigua and Barbuda",
      "Bolivia(PlurinationalStateof)" = "Bolivia (Plurinational State of)", 
      "BosniaandHerzegovina" = "Bosnia and Herzegovina",
      "BruneiDarussalam" = "Brunei Darussalam",
      "BurkinaFaso" = "Burkina Faso",
      "CostaRica" = "Costa Rica",
      "Coted'Ivoire" = "Cote d'Ivoire",
      "CaboVerde" = "Cabo Verde",
      "CentralAfricanRepublic" = "Central African Republic",
      "CookIslands" = "Cook Islands",
      "Czechia" = "Czech Republic",
      "DemocraticPeople'sRepublicofKorea" = "Korea (Democratic People's Republic of)",
      "DemocraticRepublicoftheCongo" = "Congo (Democratic Republic of the)",
      "DominicanRepublic" = "Dominican Republic",
      "ElSalvador" = "El Salvador",
      "EquatorialGuinea" = "Equatorial Guinea",
      "Iran(IslamicRepublicof)" = "Iran (Islamic Republic of)",
      "LaoPeople'sDemocraticRepublic" = "Lao People's Democratic Republic",
      "MarshallIslands" = "Marshall Islands",
      "Micronesia(FederatedStatesof)" = "Micronesia (Federated States of)",
      "NewZealand" = "New Zealand",
      "PapuaNewGuinea" = "Papua New Guinea",
      "RepublicofKorea" = "Korea (Republic of)",
      "RepublicofMoldova" = "Moldova (Republic of)",
      "RussianFederation" = "Russian Federation",
      "SaintKittsandNevis" = "Saint Kitts and Nevis",
      "SaintLucia" = "Saint Lucia",
      "SaintVincentandtheGrenadines" = "Saint Vincent and the Grenadines",
      "SanMarino" = "San Marino",
      "SaoTomeandPrincipe" = "Sao Tome and Principe",
      "SaudiArabia" = "Saudi Arabia",
      "SierraLeone" = "Sierra Leone",
      "SolomonIslands" = "Solomon Islands",
      "SouthAfrica" = "South Africa",
      "SouthSudan" = "South Sudan",
      "SriLanka" = "Sri Lanka",
      "SyrianArabRepublic" = "Syrian Arab Republic",
      "TheformerYugoslavrepublicofMacedonia" = "Macedonia (the former Yugoslav Republic of)",
      "TrinidadandTobago" = "Trinidad and Tobago",
      "UnitedArabEmirates" = "United Arab Emirates",
      "UnitedKingdomofGreatBritainandNorthernIreland" = "United Kingdom of Great Britain and Northern Ireland",
      "UnitedRepublicofTanzania" = "Tanzania, United Republic of",
      "UnitedStatesofAmerica" = "United States of America",
      "Venezuela(BolivarianRepublicof)" = "Venezuela (Bolivarian Republic of)",
      "VietNam" = "Viet Nam")) 
```

    ## Rows: 2938 Columns: 22

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (2): Country, Status
    ## dbl (20): Year, Lifeexpectancy, AdultMortality, infantdeaths, Alcohol, perce...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
continent_df =
  read_csv("data/countryContinent.csv") %>%
  janitor::clean_names() %>% 
  select(country,continent) 
```

    ## Rows: 249 Columns: 9

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (6): country, code_2, code_3, iso_3166_2, continent, sub_region
    ## dbl (3): country_code, region_code, sub_region_code

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# df with continent

life_expectancy_df = 
  life_expectancy %>% 
  left_join(continent_df, by = "country") %>% 
  relocate(country,continent) %>% 
  mutate(year = as.character(year)) 
```

``` r
check_na = function(x) {
  
  na = 
    life_expectancy_df %>%  
    filter(is.na(x)) %>%  
    count()
  
  return(na)
}

## Check for NA values 
colSums(is.na(life_expectancy_df))
```

    ##                      country                    continent 
    ##                            0                            0 
    ##                         year                       status 
    ##                            0                            0 
    ##               lifeexpectancy              adult_mortality 
    ##                           10                           10 
    ##                 infantdeaths                      alcohol 
    ##                            0                          194 
    ##        percentageexpenditure                  hepatitis_b 
    ##                            0                          553 
    ##                      measles                          bmi 
    ##                            0                           34 
    ##             under_fivedeaths                        polio 
    ##                            0                           19 
    ##             totalexpenditure                   diphtheria 
    ##                          226                           19 
    ##                     hiv_aids                          gdp 
    ##                            0                          448 
    ##                   population            thinness1_19years 
    ##                          652                           34 
    ##             thinness5_9years incomecompositionofresources 
    ##                           34                          167 
    ##                    schooling 
    ##                          163

``` r
tempData <- mice(life_expectancy_df,m=5,maxit=50,meth='cart',seed=100)
```

    ## 
    ##  iter imp variable
    ##   1   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   1   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   1   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   1   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   1   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   2   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   2   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   2   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   2   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   2   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   3   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   3   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   3   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   3   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   3   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   4   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   4   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   4   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   4   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   4   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   5   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   5   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   5   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   5   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   5   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   6   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   6   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   6   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   6   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   6   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   7   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   7   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   7   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   7   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   7   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   8   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   8   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   8   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   8   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   8   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   9   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   9   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   9   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   9   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   9   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   10   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   10   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   10   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   10   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   10   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   11   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   11   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   11   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   11   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   11   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   12   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   12   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   12   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   12   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   12   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   13   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   13   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   13   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   13   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   13   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   14   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   14   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   14   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   14   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   14   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   15   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   15   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   15   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   15   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   15   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   16   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   16   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   16   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   16   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   16   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   17   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   17   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   17   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   17   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   17   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   18   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   18   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   18   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   18   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   18   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   19   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   19   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   19   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   19   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   19   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   20   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   20   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   20   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   20   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   20   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   21   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   21   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   21   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   21   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   21   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   22   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   22   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   22   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   22   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   22   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   23   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   23   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   23   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   23   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   23   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   24   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   24   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   24   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   24   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   24   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   25   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   25   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   25   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   25   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   25   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   26   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   26   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   26   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   26   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   26   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   27   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   27   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   27   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   27   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   27   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   28   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   28   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   28   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   28   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   28   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   29   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   29   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   29   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   29   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   29   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   30   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   30   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   30   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   30   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   30   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   31   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   31   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   31   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   31   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   31   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   32   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   32   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   32   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   32   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   32   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   33   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   33   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   33   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   33   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   33   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   34   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   34   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   34   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   34   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   34   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   35   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   35   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   35   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   35   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   35   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   36   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   36   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   36   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   36   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   36   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   37   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   37   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   37   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   37   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   37   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   38   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   38   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   38   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   38   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   38   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   39   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   39   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   39   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   39   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   39   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   40   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   40   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   40   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   40   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   40   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   41   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   41   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   41   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   41   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   41   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   42   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   42   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   42   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   42   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   42   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   43   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   43   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   43   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   43   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   43   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   44   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   44   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   44   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   44   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   44   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   45   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   45   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   45   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   45   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   45   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   46   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   46   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   46   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   46   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   46   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   47   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   47   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   47   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   47   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   47   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   48   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   48   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   48   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   48   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   48   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   49   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   49   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   49   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   49   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   49   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   50   1  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   50   2  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   50   3  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   50   4  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling
    ##   50   5  lifeexpectancy  adult_mortality  alcohol  hepatitis_b  bmi  polio  totalexpenditure  diphtheria  gdp  population  thinness1_19years  thinness5_9years  incomecompositionofresources  schooling

    ## Warning: Number of logged events: 4

``` r
Complete_df <- complete(tempData,1)

colSums(is.na(Complete_df))
```

    ##                      country                    continent 
    ##                            0                            0 
    ##                         year                       status 
    ##                            0                            0 
    ##               lifeexpectancy              adult_mortality 
    ##                            0                            0 
    ##                 infantdeaths                      alcohol 
    ##                            0                            0 
    ##        percentageexpenditure                  hepatitis_b 
    ##                            0                            0 
    ##                      measles                          bmi 
    ##                            0                            0 
    ##             under_fivedeaths                        polio 
    ##                            0                            0 
    ##             totalexpenditure                   diphtheria 
    ##                            0                            0 
    ##                     hiv_aids                          gdp 
    ##                            0                            0 
    ##                   population            thinness1_19years 
    ##                            0                            0 
    ##             thinness5_9years incomecompositionofresources 
    ##                            0                            0 
    ##                    schooling 
    ##                            0

``` r
write.csv(Complete_df, file = ".\\data/Complete Data", row.names = F)
```
