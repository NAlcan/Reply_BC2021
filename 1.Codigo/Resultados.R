# Cargo las funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos
library(readxl) # para importar especificamente archivos .xlsx
library(janitor)# algunas funciones mas para limpiar datos 
# Cargo los datos de OAN --------------------------------------------------
RN_RU <- read_excel("Datos 2009 - 2018 RN.RU.1.xlsx",
                    .name_repair = make_clean_names  )

# Un vistazo rapido. 315 Variables 1297 Observaciones
#glimpse( RN_RU)

      ## Limpieza de la Base ##
# Hay caracteres especiales en columnas numericas ej:  
lev_raros<-levels(factor(RN_RU$clorofila_a_mg_l))[c(1:5)]


# Acomodos a la base:
# 1. Eliminar valores especiales "< LD" (criterio conservador, se asumen NA)
# 2. Editar comas por puntos para que tome valores numericos
# 3. Seleccionar sitios y fehcas indicados en el articulo (primer parrafo de MyM y Tabla 1)
# 4. Seleccionar variables usadas en el articulos

