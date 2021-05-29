# Funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos
library(RColorBrewer) #Paleta de colores
library(lubridate) # para manipular fechas
theme_set(theme_minimal())
# Datos -------------------------------------------------------------------

data_articulo <- read_csv("2.Datos/data_articulo.csv")

glimpse(data_articulo)

# Genero las variable Year y Mes

data_articulo <-
  data_articulo %>%
  mutate (
    Year = year(fecha_muestra),
    Mes = month (fecha_muestra),
    nombre_programa = fct_recode(
      nombre_programa,
      RN = "Río Negro",
      RU = "Rio Uruguay",
      RC = "Rio Cuareim"
    )
  )



# Umbral del 99.5 de clo a. ¿Para todos los rios o por rio?
bereta_out1 <- data_articulo %>%
  summarize (Q99.5 = quantile(Clorofila_a, probs = c(0.995), na.rm = T))

data_articulo %>% filter (Clorofila_a >= bereta_out1$Q99.5) %>%
  select(codigo_pto, fecha_muestra, Clorofila_a)


# Voy a generar la variable "beret" que identifica los datos excluidos en articulos

data_articulo <- data_articulo %>%
  mutate(BeretOut = factor(ifelse ((
    codigo_pto == "RN12" & fecha_muestra == "2018-04-17" |
      codigo_pto == "RN5" &
      fecha_muestra == "2012-01-19" |
      codigo_pto == "RN5" &
      fecha_muestra == "2012-12-06" |
      codigo_pto == "RN6" &
      fecha_muestra == "2012-08-15" |
      codigo_pto == "RN3" &
      fecha_muestra == "2012-12-05"
  ),
  "out",
  "inn"
  )))

id_variables <-
  c(
    "nombre_programa",
    "codigo_pto",
    "departamento",
    "fecha_muestra",
    "fecha_hora",
    "id_muestra",
    "BeretOut"
  )

data_articulo %>% filter (BeretOut != "out") %>%
  summarise (
    max = max(Clorofila_a, na.rm = T),
    min = min(Clorofila_a, na.rm = T),
    median = median(Clorofila_a, na.rm = T),
    media = mean (Clorofila_a, na.rm = T),
    n = n()
  )


Data_out <-
  ggplot(data_articulo, aes(y = Clorofila_a, x = nombre_programa)) +
  geom_jitter(aes(color = BeretOut)) +
  theme_minimal() + labs(x = NULL)

ggplot(data_articulo, aes(y = Clorofila_a, x = FosforoTotal)) +
  geom_point(aes(color = BeretOut)) + facet_wrap(. ~ nombre_programa) +
  theme_minimal()

Plot_pred <- data_articulo %>%
  pivot_longer(-c(all_of(id_variables), Clorofila_a),
               names_to = "xvar",
               values_to = "value") %>%
  ggplot(aes(x = value, y = Clorofila_a)) + geom_point(aes(color = BeretOut)) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) +
  facet_wrap(nombre_programa ~ xvar , scales = "free")

Plot_descr_inn <- data_articulo %>%
  filter (BeretOut == "inn") %>%
  pivot_longer(-c(all_of(id_variables), Clorofila_a),
               names_to = "xvar",
               values_to = "value") %>%
  ggplot(aes(x = value, y = Clorofila_a)) + geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) +
  facet_grid(nombre_programa ~ xvar , scales = "free")

Plot_descr_out <- data_articulo %>%
  #  filter (BeretOut == "inn") %>%
  pivot_longer(-c(all_of(id_variables), Clorofila_a),
               names_to = "xvar",
               values_to = "value") %>%
  ggplot(aes(x = value, y = Clorofila_a)) + geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) +
  facet_grid(nombre_programa ~ xvar , scales = "free")

# Dato extremo de Temp
data_articulo %>%  filter (TempAgua > 200) %>%
  select(codigo_pto, fecha_muestra, Clorofila_a, TempAgua)

# Datos de pH cortados respecto al articulo. Figura 3.C
summary(data_articulo$Ph)


ggplot(data_articulo, aes(y = Ph, x = 1)) + geom_violin()


# Recrear la figura 3 completa --------------------------------------------

data_articulo %>%   filter (BeretOut == "inn") %>%
  # filter (TempAgua < 200) %>%
  # filter (Alcalinidad < 500) %>%
  dplyr::select(Clorofila_a,
                Alcalinidad,
                Conductividad,
                FosforoTotal,
                SolidosTotales,
                Ph,
                TempAgua) %>%
  pivot_longer(
    cols = !(Clorofila_a),
    names_to = "vars",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor , y = Clorofila_a)) + geom_point(alpha = 0.5) +
  facet_wrap(~ vars , scales = "free_x", ncol = 2) +
  labs(x = NULL)

# Hay muchos datos extremos o "outlayers" que el articulo no es claro como los corrigio

# Descriptivas de Ph, Temp Y PT
data_articulo %>%  group_by (nombre_programa) %>%
  filter (TempAgua < 200) %>% # El datos extremo de T
  summarize (
    mediapH = mean (Ph, na.rm = T),
    meanTemp = mean(TempAgua, na.rm = T),
    meanPT = mean (FosforoTotal , na.rm = T)
  )


# 2.2. Analysis of variables by linear model ---------------------------------

# Funciones para extraer vaores p/variable
library(nlme)
lme_model <-  function (df) {
  lme <- lme(
    log10(valor) ~ Year,
    random =  ~ 1 |
      codigo_pto / Mes,
    data = df,
    na.action = "na.omit"
  )
}


mean_month <- function(data) {
  round(mean(data$valor, na.rm = T), 2)
}
variance_month <- function(data) {
  round(var(data$valor, na.rm = T), 2)
}

d_freedom <- function (model) {
  res <- model$fixDF[[1]][[2]]
}


beta_year <- function(model) {
  round(fixed.effects(model)[[2]], 3)
}

# Rio negro
rn <- data_articulo %>%
  filter (BeretOut == "inn" & nombre_programa == "RN") %>%
  dplyr::select(
    codigo_pto,
    Mes,
    Year,
    Clorofila_a,
    Alcalinidad,
    Conductividad,
    FosforoTotal,
    SolidosTotales,
    Ph,
    TempAgua
  )  %>%
  pivot_longer(
    cols = !c(codigo_pto, Mes, Year),
    names_to = "vars" ,
    values_to = "valor"
  ) %>%
  group_by (vars) %>%
  nest()

rn_list <- rn %>%
  mutate (
    models = map(data, lme_model),
    grados_libertad = map(models, d_freedom),
    beta = map(models, beta_year),
    mean = map(data, mean_month),
    var = map(data, variance_month)
  )


rn_list %>% dplyr::select(!c(data, models)) %>%
  unnest(cols  = c(grados_libertad, beta, mean, var))

# Rio Uruguay

ru <- data_articulo %>%
  filter (BeretOut == "inn" & nombre_programa == "RU") %>%
  dplyr::select(
    codigo_pto,
    Mes,
    Year,
    Clorofila_a,
    Alcalinidad,
    Conductividad,
    FosforoTotal,
    SolidosTotales,
    Ph,
    TempAgua
  )  %>%
  pivot_longer(
    cols = !c(codigo_pto, Mes, Year),
    names_to = "vars" ,
    values_to = "valor"
  ) %>%
  group_by (vars) %>%
  nest()

ru_list <- ru %>%
  mutate (
    models = map(data, lme_model),
    grados_libertad = map(models, d_freedom),
    beta = map(models, beta_year),
    mean = map(data, mean_month),
    var = map(data, variance_month)
  )


ru_list %>% dplyr::select(!c(data, models)) %>%
  unnest(cols  = c(beta, mean, var, grados_libertad))

# Render RMarkdown -------------------------------------------------
rmarkdown::render(
  input = "3.Resultados/Resultados_preliminares.Rmd",
  output_format = "pdf_document",
  output_dir = "3.Resultados",
  run_pandoc = TRUE,
  output_file = paste0("Resultados_preliminares.pdf")
)
