# Functions load -----------------------------------------------------
library(tidyverse) # Data load/manipulation and graphics build
library(janitor)# Data headers make clean
library(lubridate) # Day/Time Manipulation
library(snakecase) # Factor levels and names manipulation


# Zip extraction --------------------------------------------------------

# Data from OAN ere downloaded in separated .csv for each river.
# Large rivers like Uruguay and Negro, are split into two files due to max size allowed to OAN download
# Files are compressed into zip folder.
# This code intends to unzip them, extract and unify unique Database

# Zip load and decompression (It only need to be done the first)

#unzip("2.Datos/redatosoan.zip", exdir = "2.Datos/oan_data")


# Csv integration ---------------------------------------------------------
# Function to read several csv file
# The columns are set to character by default because there special characters (like "<LD")
# Three new columns
# river: stores river name from file name
# date_time = parse old `fecha` to date/time object
# date = stroe only date from date_time

read_multiple_csv <- function (file) {
  file1 <- read_csv(file, col_types = cols(.default = "c")) %>%
    clean_names() %>%
    mutate (
      river = str_sub(file, start = 21, end = -15),
      date_time = dmy_hm(fecha),
      date = date(date_time),
      .keep = "unused"
    ) # Delete old `fecha`
}

# List all csv files y the directory
# And then load and integrate into single data frame with read_multiple_csv function

oan_complet <- list.files(path = "2.Datos/oan_data",
                          pattern = "*.csv",
                          full.names = TRUE)  %>%
  map_df( ~ read_multiple_csv(.))


# Quality control ----------------------------------------------------
dim(oan_complet) # 2059 rows by 19 columns
glimpse(oan_complet)

# Define de row names that belongs to ID of data like, date, site, river...

id_vars <- c("date", "date_time", "estacion", "river")

# Variables used only in BC2021

bc_vars <- c(
  "clorofila_a_mg_l",
  "alcalinidad_total_mg_ca_co3_l",
  "conductividad_m_s_cm",
  "fosforo_total_mg_p_l",
  "solidos_suspendidos_totales_mg_l",
  "potencial_de_hidrogeno_p_h_sin_unid",
  "temperatura_o_c"
)

# Date range
oan_complet %>%
  group_by(river) %>%
  summarise (start = min(date),
             end = max(date))

# Date filtering according to BC2021
# For Cuareim  (used in BC2021), Tacuarembo and San Salvador (used here)
# End date are choose the same as Uruguay a Negro river
oan_BC <- oan_complet %>%
  filter (date <= "2018-11-30")


# Check if samples are from bottom.
# This is identified by an `F` at the end of the site code (`estacion`)

oan_BC %>% filter(str_detect(estacion, "F")) %>%
  dplyr::select(estacion) %>% unique()

# Remove the 109 rows that contains data from bottom samples
oan_BC <- oan_BC %>%
  filter(!(str_detect(estacion, "F")))

# New variable that identifies if date comes from Train, Test or Not Used

oan_BC <- oan_BC %>%
  mutate(data_model = ifelse (
    river %in% c("Negro", "Uruguay"),
    "Train",
    ifelse(river == "Cuareim", "Test", "NotUsed")
  ))
id_vars[5] <- "data_model" # Add it to the id_vars labels container
# Special characters -------------------------------------------------------

# Function to detect a patter of any special character like:
# Bellow (>) or Under (<) of Limit Detection (LD) or Limit Quantification (LC)
# And counts how many of this special character are in a vector
spec_det <- function (x) {
  x2 <- str_detect(x, pattern =  "[<>LCLD]")
  sum(x2, na.rm = T)
}

# How many special characters only in Negro and Uruguay rivers
oan_BC %>% filter (river %in% c("Negro", "Uruguay")) %>%
  dplyr::select(all_of(bc_vars)) %>%
  summarise(across(everything(), ~ spec_det(.))) %>% # Apply spec_detect function across all data columns
  pivot_longer(everything() , names_to = "variable",
               values_to = "Special_Characaters") %>% # Format in long table with one variable to each row and its count value
  filter (Special_Characaters > 0) %>% #
  mutate (prop = round(Special_Characaters / nrow(oan_BC), 2)) %>%
  arrange (Special_Characaters)


# From now Convert all variables to numeric
# Substitute all special characters ("<,>, LC, LD") values by NA
# The warnings advice this
bc_numeric <-
  oan_BC %>%
  dplyr::select(!all_of(id_vars)) %>% # Drop id vars wich will remain as a characters columns
  mutate_if(is.character, as.numeric)

# Add id vars that not were transformed into numeric
bc_tidy <- oan_BC %>% dplyr::select(all_of(id_vars)) %>%
  bind_cols(bc_numeric)

# Take a look
glimpse(bc_tidy)


# What about NA values now?
# Is there an entire row with all numeric variable with NA values?
bc_tidy %>% dplyr::select(where(is.numeric)) %>%
  filter(if_all(is.numeric, is.na)) %>%
  count()



# So, how many data per river is available?
bc_tidy %>%
  group_by(river) %>%
  count()

# Database with all rivers and extra variables than used in BC2021
#write_csv(bc_tidy, file = "2.Datos/working_data/OAN_complet_data.csv")

# Same database as BC 2021

bc2021 <- bc_tidy %>%
  dplyr::select(all_of(c(bc_vars, id_vars))) %>%
  filter (data_model != "NotUsed")

#write_csv(bc2021, file = "2.Datos/working_data/bc2021_data.csv")

# How many data per river considering only BC2021 Variables
bc2021 %>%
  group_by(river) %>%
  count()


# GIS data ----------------------------------------------------------------
library(readxl)

sitios <- read_excel("2.Datos/gis_data/sites_references.xls",
                     .name_repair = make_clean_names) %>%
  mutate(
    programa = to_snake_case(programa),
    estacion = to_snake_case (estacion),
    status = ifelse(
      codigo_estacion %in% bc_tidy$estacion &
        str_detect(codigo_estacion, "RN"),
      "Train_NR",
      ifelse(
        codigo_estacion %in% bc_tidy$estacion &
          str_detect(codigo_estacion, "RU"),
        "Train_UR",
        ifelse(
          codigo_estacion %in% bc_tidy$estacion &
            str_detect(codigo_estacion, "RC"),
          "Test_CR",
          "Not_used"
        )
      )
    )
  ) %>%
  arrange(latitud, .desc = TRUE)

#write_csv(sitios, file = "2.Datos/gis_data/coordenadas_sitios.csv")
