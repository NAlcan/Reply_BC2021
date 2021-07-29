# Function load -----------------------------------------------------
library(tidyverse) # Data load/manipulation and graphics build
library(RColorBrewer) # Color Palettes
library(lubridate) # Day/Time Manipulation
library(patchwork) # Plot Layout
library(png) # Import `png` images

theme_set(theme_minimal()) #  Graphic theme used for visualizations


# Data Load ---------------------------------------------------------------

data_oan <- read_csv("2.Datos/working_data/OAN_complet_data.csv")

# glimpse(data_oan)

# Define de row names that belongs to ID of data like, date, site, river...

id_vars <- c("date","date_time", "estacion","river")

# Rename Variables according to BC2021

data_oan <- data_oan %>% 
  rename(chla = "clorofila_a_mg_l",
         alk = "alcalinidad_total_mg_ca_co3_l",
         cond = "conductividad_m_s_cm",
         tot_phos = "fosforo_total_mg_p_l",
         tss = "solidos_suspendidos_totales_mg_l",
         pH = "potencial_de_hidrogeno_p_h_sin_unid",
         temp = "temperatura_o_c",
         po4 = "fosfato_ortofosfato_mg_po4_p_l",
         no2 = "ion_nitrito_mg_no2_n_l",
         no3 = "nitrato_mg_no3_n_l",
         nh4 = "nitrogeno_amoniacal_amonio_mg_nh4_n_l",
         tot_nit = "nitrogeno_total_mg_n_l",
         o2 = "oxigeno_disuelto_mg_l",
         ntu = "turbidez_ntu")


# Variables used only in BC2021

bc_vars <- c(
  "chla",
  "alk",
  "cond",
  "tot_phos",
  "tss",
  "pH",
  "temp")

# 99.5 percentile of Chl-a -------------
# First assume that it was calculated for each river
# How many `chla` values are eliminated if one value for each river

# Function that calcultaes the number of elements that exceed 99.5 limit
  q99.5_exceed <- function (x){
    q <- quantile(x, probs = c(0.95), na.rm =T)
  sum(x > q, na.rm = T)
    }

# Group by River and calculate the number of data exceeds 99.5 for each variable
data_oan %>% group_by(river)  %>% 
summarize(across(where(is.numeric), q99.5_exceed)) 
  
# Q99.5 limit values
 data_oan %>% group_by(river) %>% 
  dplyr::select(date, estacion, chla, river) %>% 
  summarise(p99.5 = quantile(chla, probs = c(0.995), na.rm = T))

# I Assume that BC2021 Calculated the 99.5 limit using Negro and Uruguay together
 # Lets see how many data exceed with this criteria

  data_oan %>% 
  filter(river %in% c("Negro","Uruguay")) %>% 
  filter(chla >= quantile(chla, probs = c(0.995), na.rm = T)) %>%
  dplyr::select(date, estacion, chla)
# Looks similar that they did, but with this criteria the RN12 for 2018-04-17 gets not excluded



