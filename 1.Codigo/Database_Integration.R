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

# Zip load and decompression

unzip("2.Datos/redatosoan.zip", exdir = "2.Datos/oan_data")


# Csv integration ---------------------------------------------------------
# Function to read several csv file
# The columns are set to character by default because there special characters (like "<LD")
# The new column river store river name from file name

read_multiple_csv <- function (file) {
  file1 <- read_csv(file, col_types = cols(.default = "c")) %>% 
    clean_names() %>% 
    mutate (river = str_sub(file, start = 18, end = -15))
}

# List all csv files y the directory 
# And then load and integrate into single data frame with read_multiple_csv function

oan_complet <- list.files(
  path = "2.Datos/oan_data",
  pattern = "*.csv",
  full.names = TRUE
)  %>%
  map_df(~ read_multiple_csv(.))

# 
