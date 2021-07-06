# Cargo las funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos
library(readxl) # para importar especificamente archivos .xlsx
library(janitor)# algunas funciones mas para limpiar datos
library(lubridate) # para manipular obetos Fecha/Hora
# Cargo los datos de OAN --------------------------------------------------
RN_RU <- read_excel("Datos 2009 - 2018 RN.RU.1.xlsx",
                    .name_repair = make_clean_names) # .name_repar = make_clean_names transforma los titulos de columnas todos uniformes

# Un vistazo rapido. 315 Variables 1297 Observaciones
glimpse(RN_RU)


# Limpieza de la Base -----------------------------------------------------
# Hay caracteres especiales en columnas numericas ej:
RN_RU %>%  dplyr::select (clorofila_a_mg_l) %>%
  filter(str_detect(clorofila_a_mg_l , "<"))

# Cuantos valores "<" hay por variable

RN_RU %>% filter (nombre_programa %in% c("Rio Uruguay", "Río Negro")) %>% 
 filter(str_detect(sst_mg_l , "<"))

RN_RU %>% filter (nombre_programa %in% c("Rio Uruguay", "Río Negro")) %>% 
  filter(str_detect( clorofila_a_mg_l, "<"))


# Pasos para Acomodar la base:
# 1. Eliminar valores especiales "< LD" (criterio conservador, se asumen NA)

# Excluyo las variables factores y fechas que le voy a llamar variables "id"

id_variables <-
  c(
    "nombre_programa",
    "codigo_pto",
    "departamento",
    "fecha_muestra",
    "fecha_hora",
    "id_muestra"
  )

# Con las que deberian ser pero muchas son caracteres por la presencia de los "LD",
# las paso a numericas
# los warnings avisan que se sustiuyeron caracteres raros por NA

rios_numericas <-
  RN_RU %>% dplyr::select(!(all_of(id_variables))) %>%
  mutate_if (is.character, as.numeric)

glimpse(rios_numericas) # son numericas todas(dbl o int) efectivamente

# Pego las numericas a los factores y fechas de la original
data_rios <- RN_RU %>% dplyr::select(all_of(id_variables)) %>%
  bind_cols (rios_numericas)

# Elimino las aclaraciones de los limites LD y LC ya que no retuve esos valores en las originales

data_rios <-
  data_rios %>%  dplyr::select (!ends_with(c("_ld", "_lc"))) %>%
  mutate(fecha_muestra = ymd(fecha_muestra))

glimpse(data_rios)


# BBDD 1. OANCompleto_tidy ------------------------------------------------

#write_csv(data_rios, file = "2.Datos/tidyOAN_datacomplet.csv")


# 2. Seleccionar sitios y fechas indicados en el articulo (primer parrafo de MyM y Tabla 1)

data_articulo <-
  data_rios %>% filter (
    nombre_programa == "Río Negro" & fecha_muestra >= "2009-05-01" |
      nombre_programa == "Río Negro" &
      fecha_muestra <= "2018-11-30" |
      nombre_programa == "Rio Uruguay" &
      fecha_muestra >= "2014-06-01" |
      nombre_programa == "Rio Uruguay" &
      fecha_muestra <= "2018-11-30" |
      nombre_programa == "Rio Cuareim"
  ) # El articulo original no especifica las fechas del Rio Cuareim


# 3. Seleccionar variables usadas en el articulos

# Variables usada en Bereta
bereta_variables <-
  c(
    "clorofila_a_mg_l",
    "alc_t_mg_ca_co3_l",
    "conduc_m_s_cm",
    "pt_mg_p_l",
    "sst_mg_l",
    "p_h_sin_unid",
    "t_o_c"
  )


data_articulo <-
  data_articulo %>% dplyr::select(all_of(id_variables), all_of(bereta_variables)) %>%
  rename(
    "Clorofila_a" = clorofila_a_mg_l,
    "Alcalinidad" = alc_t_mg_ca_co3_l,
    "Conductividad" = conduc_m_s_cm,
    "FosforoTotal" = pt_mg_p_l,
    "SolidosTotales" = sst_mg_l,
    "Ph" = p_h_sin_unid,
    "TempAgua" = `t_o_c`
  )

# BBDD 2. Data articulo ------------------------------------------------

#write_csv(data_articulo, file = "2.Datos/data_articulo.csv")


# Umbral del 99.5 de clo a. ¿Para todos los rios o por rio?
bereta_out1 <-
  data_articulo %>% filter (nombre_programa != "Rio Cuareim") %>%  
  summarize (Q99.5 = quantile(Clorofila_a, probs = c(0.995), na.rm =
                                                   T))

data_articulo %>% filter (Clorofila_a >= bereta_out1$Q99.5) %>%
  select(codigo_pto, fecha_muestra, Clorofila_a)

# Me quedan cuatro puntos. En articulo son 5. QUe pasa con RN12?
data_articulo %>% filter (codigo_pto == "RN12" &
                            fecha_muestra == "2018-04-17") %>%
  dplyr::select(codigo_pto, fecha_muestra, Clorofila_a)


# Si calculo el 99.5 por rio... como el RU es muy bajo, me quedan muchos extremos!

bereta_out2 <- data_articulo %>% 
  filter (nombre_programa != "Rio Cuareim") %>%  
  group_by(nombre_programa) %>%
  summarize (Q99.5 = quantile(Clorofila_a, probs = c(0.995), na.rm = T))

data_articulo %>%  filter (nombre_programa != "Rio Cuareim") %>%  
  filter (Clorofila_a >= bereta_out2$Q99.5[[1]]   |
                            Clorofila_a >= bereta_out2$Q99.5[[2]]) %>%
  select(codigo_pto,nombre_programa, fecha_muestra, Clorofila_a)


data_articulo %>% filter (
  !(
    codigo_pto == "RN12" & fecha_muestra == "2018-04-17" |
      codigo_pto == "RN5" &
      fecha_muestra == "2012-01-19" |
      codigo_pto == "RN5" &
      fecha_muestra == "2012-12-06" |
      codigo_pto == "RN6" &
      fecha_muestra == "2012-08-15" |
      codigo_pto == "RN3" &
      fecha_muestra == "2012-12-05"
  )
)

# BBDD 3. Beretta data (99.5 out) -----------------------------------------

data_bereta <-
  data_articulo %>% filter (
    !(
      codigo_pto == "RN12" & fecha_muestra == "2018-04-17" |
        codigo_pto == "RN5" &
        fecha_muestra == "2012-01-19" |
        codigo_pto == "RN5" &
        fecha_muestra == "2012-12-06" |
        codigo_pto == "RN6" &
        fecha_muestra == "2012-08-15" |
        codigo_pto == "RN3" &
        fecha_muestra == "2012-12-05"
    )
  )

#write_csv(data_bereta, file = "2.Datos/data_bereta.csv")


# Gis Data ----------------------------------------------------------------



sitios <- read_excel("2.Datos/sites_references.xls", 
                     .name_repair = make_clean_names) %>% 
  mutate(programa = to_snake_case(programa),
         estacion = to_snake_case (estacion),
         status = ifelse(codigo_estacion %in% data_bereta$codigo_pto & (
                           str_detect(codigo_estacion, "RN") |
                             str_detect(codigo_estacion, "RU")), "Train",
                         ifelse(codigo_estacion %in% data_bereta$codigo_pto &
                                  str_detect(codigo_estacion, "RC"), "Test","Not_used")))

#write_csv(sitios, file = "2.Datos/coordenadas_sitios.csv")
