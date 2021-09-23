Sites Spatail Data
================

#### Packages

``` r
library(readxl) # to read xls files
library(tidyverse) # Data load/manipulation and graphics build
library(janitor)# Data headers make clean
library(lubridate) # Day/Time Manipulation
library(snakecase) # Factor levels and names manipulation
```

### Site Coordinates table

``` r
sites <- read_excel("2.Datos/gis_data/sites_references.xls",
                     .name_repair = make_clean_names) %>% 
  mutate(
    programa = to_snake_case(programa),
    nombre_punto = to_snake_case (estacion),
    .keep= "unused")
```

Delete station from the bottom idetified by and `F` and the code end
label

``` r
 sites <- sites %>% filter(!str_detect(codigo_estacion,"F"))
```

Also load OAN Database, to match stations and calculte freqeuncy and
dates

``` r
oan_data <- read_csv("2.Datos/working_data/OAN_complet_data.csv")
```

Filter by data points from oan data, generate the varyble type and
arrange by `latitude`

``` r
 sites_reply <- sites %>% 
  filter(codigo_estacion %in% oan_data$estacion) %>% 
     mutate(type = ifelse(str_detect(codigo_estacion, c("RN","RU")), "train", 
                       ifelse( str_detect(codigo_estacion, "RC"),"test","not_used"))) %>%
  arrange(latitud, .desc = TRUE) %>% 
   mutate(site_number = row_number())
```

### Sampling stations data

Calculates n data per site and starting end dates

``` r
sites_oan_data <- oan_data %>% 
   group_by(estacion) %>% 
   summarize ( N_Station = length(estacion),
               Start_Date = min(date),
               End_Date = max(date)) %>% 
   rename (Sampling_Point = "estacion")
```

#### Table A1: Sampling station

``` r
sites_reply %>% 
   rename (Sampling_Point = "codigo_estacion",
           Latitude = "latitud",
           Longitude = "longitud",
           `Site Code` = "site_number") %>% 
    mutate (Basin = ifelse(str_detect(Sampling_Point,c("RC","RU","SSS")), "Uruguay","Negro")) %>% 
     full_join(sites_oan_data, by = "Sampling_Point") %>% 
      dplyr::select(Basin,Sampling_Point,Latitude,Longitude,type,N_Station,Start_Date,End_Date, `Site Code`) %>% 
  kable()
```

| Basin   | Sampling\_Point | Latitude   | Longitude  | type      | N\_Station | Start\_Date | End\_Date  | Site Code |
|:--------|:----------------|:-----------|:-----------|:----------|-----------:|:------------|:-----------|----------:|
| Uruguay | RC50            | -30.153338 | -56.784112 | test      |         31 | 2012-07-24  | 2018-07-31 |         1 |
| Negro   | RC60            | -30.27903  | -57.41578  | test      |         39 | 2008-01-08  | 2017-08-01 |         2 |
| Negro   | RC3C70          | -30.335783 | -57.046419 | test      |         24 | 2014-03-11  | 2018-07-31 |         3 |
| Uruguay | RCYU80          | -30.347013 | -57.328938 | test      |         23 | 2014-03-11  | 2018-07-31 |         4 |
| Negro   | RC40            | -30.357963 | -56.547252 | test      |         49 | 2008-01-09  | 2018-07-31 |         5 |
| Negro   | RC35            | -30.395397 | -56.455783 | test      |         49 | 2008-01-09  | 2018-08-02 |         6 |
| Uruguay | RC20            | -30.503327 | -56.367441 | test      |         41 | 2009-05-05  | 2018-08-01 |         7 |
| Negro   | RC10            | -30.694941 | -56.150588 | test      |         29 | 2012-07-25  | 2018-08-01 |         8 |
| Negro   | CU1             | -30.917365 | -55.541327 | not\_used |          7 | 2017-02-08  | 2018-08-21 |         9 |
| Negro   | TG1             | -31.177506 | -55.762436 | not\_used |          1 | 2018-08-21  | 2018-08-21 |        10 |
| Negro   | CU2             | -31.340686 | -55.475883 | not\_used |          1 | 2018-08-21  | 2018-08-21 |        11 |
| Negro   | TG2             | -31.52985  | -55.686678 | not\_used |          1 | 2018-08-21  | 2018-08-21 |        12 |
| Negro   | TCH010          | -31.709354 | -55.963679 | not\_used |          7 | 2017-02-08  | 2018-08-20 |        13 |
| Negro   | CU3             | -31.737324 | -55.543746 | not\_used |          6 | 2017-02-08  | 2018-08-22 |        14 |
| Negro   | RN0             | -31.81922  | -54.459889 | train     |          6 | 2016-11-22  | 2018-07-02 |        15 |
| Negro   | TG3             | -31.879482 | -55.472443 | not\_used |          7 | 2017-02-07  | 2018-08-20 |        16 |
| Negro   | TCH020          | -31.965648 | -55.675629 | not\_used |          7 | 2017-02-09  | 2018-08-22 |        17 |
| Negro   | YA1             | -32.033503 | -55.366831 | not\_used |          7 | 2017-02-07  | 2018-08-20 |        18 |
| Negro   | RN1             | -32.109839 | -54.667486 | not\_used |         31 | 2009-05-26  | 2018-09-25 |        19 |
| Negro   | CA1             | -32.158081 | -55.023937 | not\_used |          7 | 2017-02-07  | 2018-08-20 |        20 |
| Negro   | TG4             | -32.321903 | -55.416429 | not\_used |          7 | 2017-02-07  | 2018-08-20 |        21 |
| Negro   | RN2             | -32.503888 | -55.505278 | train     |         30 | 2009-09-15  | 2018-09-25 |        22 |
| Negro   | RN3             | -32.621107 | -55.841937 | not\_used |         31 | 2009-05-26  | 2018-09-25 |        23 |
| Negro   | RN7             | -32.821112 | -56.513057 | train     |         31 | 2009-05-27  | 2018-09-26 |        24 |
| Negro   | RN5             | -32.823319 | -56.419437 | train     |         30 | 2009-05-27  | 2018-09-27 |        25 |
| Negro   | RN6             | -32.835265 | -56.419282 | not\_used |         31 | 2009-05-27  | 2018-09-27 |        26 |
| Negro   | RN10            | -32.871883 | -56.809333 | train     |         31 | 2009-05-28  | 2018-09-26 |        27 |
| Negro   | RN9             | -32.876116 | -56.798615 | not\_used |         31 | 2009-05-28  | 2018-09-26 |        28 |
| Uruguay | RU0             | -32.902139 | -58.116389 | train     |         21 | 2014-07-22  | 2018-09-04 |        29 |
| Negro   | RN14            | -33.049706 | -57.453616 | train     |         31 | 2009-06-02  | 2018-10-02 |        30 |
| Negro   | RN13            | -33.066936 | -57.45417  | not\_used |         31 | 2009-06-02  | 2018-10-02 |        31 |
| Uruguay | RU1             | -33.074944 | -58.151806 | not\_used |         22 | 2014-07-22  | 2018-09-04 |        32 |
| Negro   | RU2             | -33.086556 | -58.136611 | not\_used |         22 | 2014-07-22  | 2018-09-04 |        33 |
| Negro   | RU4             | -33.0945   | -58.211889 | not\_used |         21 | 2014-07-22  | 2018-09-04 |        34 |
| Negro   | RN11            | -33.097219 | -57.126662 | not\_used |         31 | 2009-06-02  | 2018-10-02 |        35 |
| Negro   | RU7             | -33.104111 | -58.25775  | train     |         22 | 2014-07-22  | 2018-09-04 |        36 |
| Negro   | RU12            | -33.104667 | -58.301994 | train     |         22 | 2014-07-23  | 2018-09-05 |        37 |
| Uruguay | RU5             | -33.105167 | -58.216583 | train     |         22 | 2014-07-22  | 2018-09-04 |        38 |
| Negro   | RU11            | -33.106111 | -58.2955   | not\_used |         22 | 2014-07-23  | 2018-09-05 |        39 |
| Negro   | RU3             | -33.107472 | -58.186444 | train     |         22 | 2014-07-22  | 2018-09-04 |        40 |
| Uruguay | RU8             | -33.109417 | -58.265111 | not\_used |         22 | 2014-07-22  | 2018-09-04 |        41 |
| Negro   | RU6             | -33.1115   | -58.219361 | not\_used |         22 | 2014-07-22  | 2018-09-04 |        42 |
| Negro   | RU10            | -33.1155   | -58.282139 | train     |         22 | 2014-07-23  | 2018-09-05 |        43 |
| Uruguay | RU13            | -33.118056 | -58.3365   | not\_used |         22 | 2014-07-23  | 2018-09-05 |        44 |
| Negro   | RU9             | -33.119361 | -58.269472 | train     |         22 | 2014-07-23  | 2018-09-05 |        45 |
| Negro   | RN12            | -33.143322 | -57.101672 | train     |         30 | 2009-09-22  | 2018-10-02 |        46 |
| Uruguay | RU15            | -33.165111 | -58.391917 | not\_used |         22 | 2014-07-23  | 2018-09-05 |        47 |
| Negro   | RU16            | -33.168    | -58.358167 | train     |         22 | 2014-07-23  | 2018-09-05 |        48 |
| Negro   | RU14            | -33.177139 | -58.359972 | train     |         22 | 2014-07-23  | 2018-09-05 |        49 |
| Negro   | RN15            | -33.234717 | -58.009994 | not\_used |         29 | 2009-06-03  | 2018-10-03 |        50 |
| Negro   | RN16            | -33.240824 | -58.056944 | train     |         30 | 2009-06-03  | 2018-10-03 |        51 |
| Negro   | RN17            | -33.388324 | -58.317224 | not\_used |         31 | 2009-06-03  | 2018-10-03 |        52 |
| Negro   | SS6             | -33.516199 | -58.200569 | not\_used |         25 | 2014-05-14  | 2018-10-23 |        53 |
| Negro   | SS6.5           | -33.519189 | -58.234396 | not\_used |         14 | 2016-07-26  | 2018-10-23 |        54 |
| Negro   | SS7             | -33.533889 | -58.264444 | not\_used |         11 | 2014-05-14  | 2016-05-10 |        55 |
| Negro   | SS5             | -33.555556 | -58.163611 | not\_used |         25 | 2014-05-14  | 2018-10-23 |        56 |
| Negro   | SS4             | -33.780012 | -57.928566 | not\_used |         24 | 2014-05-13  | 2018-10-23 |        57 |
| Negro   | SS3             | -33.816042 | -57.788771 | not\_used |         25 | 2014-05-13  | 2018-10-23 |        58 |
| Negro   | SS1             | -33.857532 | -57.501609 | not\_used |         25 | 2014-05-13  | 2018-10-23 |        59 |
| Negro   | SS2             | -33.860271 | -57.727698 | not\_used |         25 | 2014-05-13  | 2018-10-23 |        60 |

``` r
sessionInfo()
```

    ## R version 4.1.1 (2021-08-10)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 20.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=es_UY.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=es_UY.UTF-8        LC_COLLATE=es_UY.UTF-8    
    ##  [5] LC_MONETARY=es_UY.UTF-8    LC_MESSAGES=es_UY.UTF-8   
    ##  [7] LC_PAPER=es_UY.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=es_UY.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] snakecase_0.11.0 lubridate_1.7.10 janitor_2.1.0    forcats_0.5.1   
    ##  [5] stringr_1.4.0    dplyr_1.0.7      purrr_0.3.4      readr_2.0.1     
    ##  [9] tidyr_1.1.3      tibble_3.1.4     ggplot2_3.3.5    tidyverse_1.3.1 
    ## [13] readxl_1.3.1     knitr_1.33      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.1  xfun_0.25         haven_2.4.3       colorspace_2.0-2 
    ##  [5] vctrs_0.3.8       generics_0.1.0    htmltools_0.5.1.1 yaml_2.2.1       
    ##  [9] utf8_1.2.2        rlang_0.4.11      pillar_1.6.2      glue_1.4.2       
    ## [13] withr_2.4.2       DBI_1.1.1         bit64_4.0.5       dbplyr_2.1.1     
    ## [17] modelr_0.1.8      lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0     
    ## [21] cellranger_1.1.0  rvest_1.0.1       evaluate_0.14     tzdb_0.1.2       
    ## [25] parallel_4.1.1    fansi_0.5.0       highr_0.9         broom_0.7.9      
    ## [29] Rcpp_1.0.7        scales_1.1.1      backports_1.2.1   vroom_1.5.4      
    ## [33] jsonlite_1.7.2    bit_4.0.4         fs_1.5.0          hms_1.1.0        
    ## [37] digest_0.6.27     stringi_1.7.3     grid_4.1.1        cli_3.0.1        
    ## [41] tools_4.1.1       magrittr_2.0.1    crayon_1.4.1      pkgconfig_2.0.3  
    ## [45] ellipsis_0.3.2    xml2_1.3.2        reprex_2.0.1      assertthat_0.2.1 
    ## [49] rmarkdown_2.10    httr_1.4.2        rstudioapi_0.13   R6_2.5.1         
    ## [53] compiler_4.1.1
