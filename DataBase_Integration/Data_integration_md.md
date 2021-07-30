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
load and decompression (It only need to be done the first time)

``` r
#unzip("2.Datos/redatosoan.zip", exdir = "2.Datos/oan_data")
```

### csv integration

Function to read several csv file. The columns are set to character by
default because there special characters (like “&lt;LD”) Three new
columns are generated \* river: stores `river` name from file name \*
date\_time = parse old `fecha` to date/time object \* date = stroe only
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

``` r
glimpse(oan_complet)
```

    ## Rows: 2,059
    ## Columns: 18
    ## $ estacion                              <chr> "RC60", "RC35", "RC40", "RC35", …
    ## $ alcalinidad_total_mg_ca_co3_l         <chr> "84", "80", "79", "85", "110", "…
    ## $ clorofila_a_mg_l                      <chr> NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ conductividad_m_s_cm                  <chr> "193.5", "154.7", "155.8", "185"…
    ## $ fosfato_ortofosfato_mg_po4_p_l        <chr> NA, "< LC", "< LC", NA, "41", "<…
    ## $ fosforo_total_mg_p_l                  <chr> NA, "42", "5", NA, "12", "51", "…
    ## $ ion_nitrito_mg_no2_n_l                <chr> "0.096", "< LC", "< LC", "0.007"…
    ## $ nitrato_mg_no3_n_l                    <chr> "0.9", "< 0.05", "0.067", "0.31"…
    ## $ nitrogeno_amoniacal_amonio_mg_nh4_n_l <chr> "0.11", "< LD", "0.19", "0.049",…
    ## $ nitrogeno_total_mg_n_l                <chr> "5.7", "< 0.5", "< 0.5", "0.65",…
    ## $ oxigeno_disuelto_mg_l                 <chr> "8.05", "7.3", "5.94", "7.24", "…
    ## $ potencial_de_hidrogeno_p_h_sin_unid   <chr> "7.1", "6.71", "6.27", "8.22", "…
    ## $ solidos_suspendidos_totales_mg_l      <chr> "16", "23", "120", "< 10", "< 10…
    ## $ temperatura_o_c                       <chr> "30.9", "32", "32.2", "28", "28.…
    ## $ turbidez_ntu                          <chr> "3.1", "3", "37", "3.8", "5.1", …
    ## $ river                                 <chr> "Cuareim", "Cuareim", "Cuareim",…
    ## $ date_time                             <dttm> 2008-01-08, 2008-01-09, 2008-01…
    ## $ date                                  <date> 2008-01-08, 2008-01-09, 2008-01…

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

Function to detect a patter of any special character like: \* &gt;
Bellow \* &lt; Under \* *L**D* Limit Detection \* *L**C* Limit
Quantification

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
| clorofila\_a\_mg\_l                  |                  167 | 0.11 |
| solidos\_suspendidos\_totales\_mg\_l |                  367 | 0.25 |

# From now Convert all variables to numeric

# Substitute all special characters (“&lt;,&gt;, LC, LD”) values by NA

# The warnings advice this

bc\_numeric &lt;- oan\_BC %&gt;% dplyr::select(!all\_of(id\_vars))
%&gt;% \# Drop id vars wich will remain as a characters columns
mutate\_if(is.character,as.numeric)

# Add id vars that not were transformed into numeric

bc\_tidy &lt;- oan\_BC %&gt;% dplyr::select(all\_of(id\_vars)) %&gt;%
bind\_cols(bc\_numeric)

# Take a look

glimpse(bc\_tidy)

# What about NA values now?

# Is there an entire row with all numeric variable with NA values?

bc\_tidy %&gt;% dplyr::select(where(is.numeric)) %&gt;%
filter(if\_all(is.numeric, is.na)) %&gt;% count()

# So, how many data per river is available?

bc\_tidy %&gt;% group\_by(river) %&gt;% count()

# Database with all rivers and extra variables than used in BC2021

\#write\_csv(bc\_tidy, file =
“2.Datos/working\_data/OAN\_complet\_data.csv”)

# Same database as BC 2021

bc2021 &lt;- bc\_tidy %&gt;%
dplyr::select(all\_of(c(bc\_vars,id\_vars))) %&gt;% filter (data\_model
!= “NotUsed”)

\#write\_csv(bc2021, file = “2.Datos/working\_data/bc2021\_data.csv”)

# How many data per river considering only BC2021 Variables

bc2021 %&gt;% group\_by(river) %&gt;% count()

# GIS data —————————————————————-

library(readxl)

sitios &lt;- read\_excel(“2.Datos/gis\_data/sites\_references.xls”,
.name\_repair = make\_clean\_names) %&gt;% mutate(programa =
to\_snake\_case(programa), estacion = to\_snake\_case (estacion), status
= ifelse(codigo\_estacion %in%
bc\_tidy$estacion &  str\_detect(codigo\_estacion, "RN"), "Train\_NR",  ifelse(codigo\_estacion %in% bc\_tidy$estacion
& str\_detect(codigo\_estacion, “RU”), “Train\_UR”,
ifelse(codigo\_estacion %in% bc\_tidy$estacion &
str\_detect(codigo\_estacion, “RC”), “Test\_CR”,“Not\_used”)))) %&gt;%
arrange(latitud, .desc = TRUE)

\#write\_csv(sitios, file = “2.Datos/gis\_data/coordenadas\_sitios.csv”)
