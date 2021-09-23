Database integration and curation
================

### Functions load

``` r
library(tidyverse) # Data load/manipulation and graphics build
library(janitor)# Data headers make clean
library(lubridate) # Day/Time Manipulation
library(snakecase) # Factor levels and names manipulation
```

### Zipped data extraction

Data from OAN ere downloaded in separated .csv for each river. Large
rivers like Uruguay and Negro, are split into two files due to max size
allowed to OAN download. Files are compressed into zip folder.

This code intends to unzip them, extract and unify unique Database Zip
load and decompression

``` r
#unzip("2.Datos/redatosoan.zip", exdir = "2.Datos/oan_data") # It's only need to be done the first time, because de function can't overwrite an existing folder
```

### csv integration

Function to read several csv file. The columns are set to character by
default because there special characters (like “&lt;LD”) Three new
columns are generated \* river: stores `river` name from file name \*
date\_time = parse old `fecha` to date/time object \* date = store only
date from `date_time`

``` r
read_multiple_csv <- function (file) {
  file1 <- read_csv(file, col_types = cols(.default = "c")) %>% 
    clean_names() %>% 
    mutate (river = str_sub(file, start = 21, end = -15),
            date_time = dmy_hm(fecha),
              date = date(date_time),
            .keep = "unused") # Delete old `fecha`
}
```

List all csv files y the directory and then load and integrate into
single data frame with `read_multiple_csv` function

``` r
oan_complet <- list.files(
  path = "2.Datos/oan_data",
  pattern = "*.csv",
  full.names = TRUE
)  %>%
  map_df(~ read_multiple_csv(.))
```

``` r
dim(oan_complet) # 2059 rows by 19 columns
```

    ## [1] 2059   18

Define de row names that belongs to ID of data like: `date`, `site`,
`river`…

``` r
id_vars <- c("date","date_time", "estacion","river")
```

Variables used only in BC2021

``` r
bc_vars <- c(
  "clorofila_a_mg_l",
  "alcalinidad_total_mg_ca_co3_l",
  "conductividad_m_s_cm",
  "fosforo_total_mg_p_l",
  "solidos_suspendidos_totales_mg_l",
  "potencial_de_hidrogeno_p_h_sin_unid",
  "temperatura_o_c")
```

## Lets see what is in the data

### Date range

``` r
oan_complet %>% 
  group_by(river) %>% 
  summarise (start = min(date),
             end = max(date)) %>% 
  kable()
```

| river       | start      | end        |
|:------------|:-----------|:-----------|
| Cuareim     | 2008-01-08 | 2020-10-29 |
| Negro       | 2009-05-26 | 2020-10-07 |
| SanSalvador | 2014-05-13 | 2020-10-27 |
| Tacuarembo  | 2017-02-07 | 2020-11-19 |
| Uruguay     | 2014-07-22 | 2020-11-11 |

Date filtering according to BC2021. For Cuareim (used in BC2021 but
without criteria defined), Tacuarembo and San Salvador (used here) end
date are choose the same as Uruguay a Negro river

``` r
oan_BC <- oan_complet %>% 
  filter (date <= "2018-11-30") 
```

### Check if samples are from bottom.

This is identified by an `F` at the end of the site code (`estacion`)

``` r
oan_BC %>% filter(str_detect(estacion, "F")) %>%
  dplyr::select(estacion) %>% unique() %>% kable()
```

| estacion |
|:---------|
| RN5F     |
| RN9F     |
| RN13F    |
| RU1F     |
| RU3F     |
| RU5F     |
| RU7F     |
| RU12F    |
| RU15F    |

### Remove the 109 rows that contains data from bottom samples

``` r
oan_BC <- oan_BC %>%
  filter(!(str_detect(estacion, "F")))
```

Generate new variable that identifies if data where used for model
Train, Test or Not Used

``` r
oan_BC <- oan_BC %>% 
  mutate(data_model = ifelse (river %in% c("Negro","Uruguay"),"Train",
                              ifelse(river == "Cuareim", "Test","NotUsed")))
```

Add the `data_model` label for the `id_labels` vector

``` r
id_vars[5] <- "data_model" 
```

### Special characters

Function to detect a patter of any special character like: \* &gt; Below
\* &lt; Under \* *L**D* Limit Detection \* *L**C* Limit Quantification

And counts how many of this special character are in a vector

``` r
spec_det <- function (x) {
  x2 <- str_detect(x, pattern =  "[<>LCLD]") # If any of this characters are detcted returns: TRUE  
    sum(x2, na.rm = T)
    }
```

How many special characters are only in Negro and Uruguay river?

``` r
oan_BC %>% filter (river %in% c("Negro", "Uruguay")) %>% 
  dplyr::select(all_of(bc_vars)) %>% 
  summarise(across(everything(), ~ spec_det(.))) %>% # Apply spec_detect function across all data columns
  pivot_longer( everything() ,names_to = "variable",
                values_to = "Special_Characaters") %>% # Format in long table with one variable to each row and its count value 
  filter (Special_Characaters > 0) %>% # 
  mutate (prop = round(Special_Characaters/nrow(oan_BC),2)) %>% 
  arrange (Special_Characaters) %>% 
  kable()
```

| variable                             | Special\_Characaters | prop |
|:-------------------------------------|---------------------:|-----:|
| clorofila\_a\_mg\_l                  |                  166 | 0.12 |
| solidos\_suspendidos\_totales\_mg\_l |                  287 | 0.21 |

From here to ahead all variables will be converted to numeric. All
special characters (“&lt;,&gt;, LC, LD”) will be subtituted by NA

``` r
bc_numeric <-
  oan_BC %>% 
  dplyr::select(!all_of(id_vars)) %>% # Drop id vars wich will remain as a characters columns
  mutate_if(is.character,as.numeric)
```

Add `id_vars` that not were transformed into numeric

``` r
bc_tidy <- oan_BC %>% dplyr::select(all_of(id_vars)) %>% 
  bind_cols(bc_numeric)
```

### Take a look at data frame

``` r
glimpse(bc_tidy)
```

    ## Rows: 1,354
    ## Columns: 19
    ## $ date                                  <date> 2008-01-08, 2008-01-09, 2008-01…
    ## $ date_time                             <dttm> 2008-01-08, 2008-01-09, 2008-01…
    ## $ estacion                              <chr> "RC60", "RC35", "RC40", "RC35", …
    ## $ river                                 <chr> "Cuareim", "Cuareim", "Cuareim",…
    ## $ data_model                            <chr> "Test", "Test", "Test", "Test", …
    ## $ alcalinidad_total_mg_ca_co3_l         <dbl> 84, 80, 79, 85, 110, 110, 68, 29…
    ## $ clorofila_a_mg_l                      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ conductividad_m_s_cm                  <dbl> 193.5, 154.7, 155.8, 185.0, 241.…
    ## $ fosfato_ortofosfato_mg_po4_p_l        <dbl> NA, NA, NA, NA, 41, NA, NA, NA, …
    ## $ fosforo_total_mg_p_l                  <dbl> NA, 42, 5, NA, 12, 51, 38, 34, 4…
    ## $ ion_nitrito_mg_no2_n_l                <dbl> 0.096, NA, NA, 0.007, NA, NA, 0.…
    ## $ nitrato_mg_no3_n_l                    <dbl> 0.900, NA, 0.067, 0.310, 0.290, …
    ## $ nitrogeno_amoniacal_amonio_mg_nh4_n_l <dbl> 0.110, NA, 0.190, 0.049, 0.028, …
    ## $ nitrogeno_total_mg_n_l                <dbl> 5.70, NA, NA, 0.65, 0.80, 0.73, …
    ## $ oxigeno_disuelto_mg_l                 <dbl> 8.05, 7.30, 5.94, 7.24, 5.39, 7.…
    ## $ potencial_de_hidrogeno_p_h_sin_unid   <dbl> 7.10, 6.71, 6.27, 8.22, 7.85, 7.…
    ## $ solidos_suspendidos_totales_mg_l      <dbl> 16, 23, 120, NA, NA, 16, NA, NA,…
    ## $ temperatura_o_c                       <dbl> 30.9, 32.0, 32.2, 28.0, 28.7, 28…
    ## $ turbidez_ntu                          <dbl> 3.1, 3.0, 37.0, 3.8, 5.1, 6.4, 1…

### What about NA values now?

Is there an entire row with all numeric variable with NA values?

``` r
bc_tidy %>% dplyr::select(where(is.numeric)) %>% 
    filter(if_all(is.numeric, is.na)) %>% 
  count() %>% rename (`NA Count` = "n") %>% 
  kable()
```

| NA Count |
|---------:|
|        0 |

### How many data per river is available?

``` r
bc_tidy %>% 
  group_by(river) %>% 
    count() %>% 
  kable()
```

| river       |   n |
|:------------|----:|
| Cuareim     | 285 |
| Negro       | 465 |
| SanSalvador | 174 |
| Tacuarembo  |  58 |
| Uruguay     | 372 |

Write database with all rivers and extra variables than used in BC2021

``` r
#write_csv(bc_tidy, file ="2.Datos/working_data/OAN_complet_data.csv")
```

## Database with only rivers and variables used in BC 2021

``` r
bc2021 <- bc_tidy %>% 
  dplyr::select(all_of(c(bc_vars,id_vars))) %>% 
  filter (data_model != "NotUsed")
```

``` r
#write_csv(bc2021, file = "2.Datos/working_data/bc2021_data.csv")
```

## How many data per river considering only BC2021 Variables

``` r
bc2021 %>% 
  group_by(river) %>% 
  count() %>% 
  kable()
```

| river   |   n |
|:--------|----:|
| Cuareim | 285 |
| Negro   | 465 |
| Uruguay | 372 |

#### Session Info

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
    ## [13] knitr_1.33      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.1  xfun_0.25         haven_2.4.3       colorspace_2.0-2 
    ##  [5] vctrs_0.3.8       generics_0.1.0    htmltools_0.5.1.1 yaml_2.2.1       
    ##  [9] utf8_1.2.2        rlang_0.4.11      pillar_1.6.2      glue_1.4.2       
    ## [13] withr_2.4.2       DBI_1.1.1         bit64_4.0.5       dbplyr_2.1.1     
    ## [17] modelr_0.1.8      readxl_1.3.1      lifecycle_1.0.0   munsell_0.5.0    
    ## [21] gtable_0.3.0      cellranger_1.1.0  rvest_1.0.1       evaluate_0.14    
    ## [25] tzdb_0.1.2        parallel_4.1.1    fansi_0.5.0       highr_0.9        
    ## [29] broom_0.7.9       Rcpp_1.0.7        scales_1.1.1      backports_1.2.1  
    ## [33] vroom_1.5.4       jsonlite_1.7.2    bit_4.0.4         fs_1.5.0         
    ## [37] hms_1.1.0         digest_0.6.27     stringi_1.7.3     grid_4.1.1       
    ## [41] cli_3.0.1         tools_4.1.1       magrittr_2.0.1    crayon_1.4.1     
    ## [45] pkgconfig_2.0.3   ellipsis_0.3.2    xml2_1.3.2        reprex_2.0.1     
    ## [49] assertthat_0.2.1  rmarkdown_2.10    httr_1.4.2        rstudioapi_0.13  
    ## [53] R6_2.5.1          compiler_4.1.1
