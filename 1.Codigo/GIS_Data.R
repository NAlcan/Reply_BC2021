# GIS data ----------------------------------------------------------------
library(readxl)
library(tidyverse) # Data load/manipulation and graphics build
library(janitor)# Data headers make clean
library(lubridate) # Day/Time Manipulation
library(snakecase) # Factor levels and names manipulation


# Load sampling site coordinates table
sites <- read_excel("2.Datos/gis_data/sites_references.xls",
                    .name_repair = make_clean_names) %>%
  mutate(
    programa = to_snake_case(programa),
    nombre_punto = to_snake_case (estacion),
    .keep = "unused"
  )

# OAN Database, to match the station used by
oan_data <- read_csv("2.Datos/working_data/OAN_complet_data.csv")


# Eliminate station from the bottom idetified by and `F` and the code end label
sites <- sites %>% filter(!str_detect(codigo_estacion, "F"))


# Filter by data points from oan data.
sites_reply <- sites %>%
  filter(codigo_estacion %in% oan_data$estacion)


sites_reply <- sites_reply %>%
  mutate(type = ifelse(
    str_detect(codigo_estacion, c("RN", "RU")),
    "train",
    ifelse(str_detect(codigo_estacion, "RC"), "test", "not_used")
  )) %>%
  arrange(latitud, .desc = TRUE) %>%
  mutate(site_number = row_number()) # Number code for each site

#write_csv(sites_reply, file = "2.Datos/gis_data/coordenadas_sitios.csv")


# Table A1 variables ------------------------------------------------------

# * `type` :  Negro or Uruguay = "train", Cuareim = "test", others = "not used"
# * `basin` : Uruguay, Cuareim or SanSalvador  = "Uruguay", other = "negro"
# *

# Calculates n data per site and starting end dates
sites_oan_data <- oan_data %>%
  group_by(estacion) %>%
  summarize (
    N_Station = length(estacion),
    Start_Date = min(date),
    End_Date = max(date)
  ) %>%
  rename (Sampling_Point = "estacion")



table1a_data <-  sites_reply %>%
  rename (
    Sampling_Point = "codigo_estacion",
    Latitude = "latitud",
    Longitude = "longitud",
    `Site Code` = "site_number"
  ) %>%
  mutate (Basin = ifelse(str_detect(Sampling_Point, c("RC", "RU", "SSS")), "Uruguay", "Negro")) %>%
  full_join(sites_oan_data, by = "Sampling_Point") %>%
  dplyr::select(
    Basin,
    Sampling_Point,
    Latitude,
    Longitude,
    type,
    N_Station,
    Start_Date,
    End_Date,
    `Site Code`
  )
