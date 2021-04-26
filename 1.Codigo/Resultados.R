# Funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos
library(RColorBrewer) #Paleta de colores
library(lubridate) # para manipular fechas
# Datos -------------------------------------------------------------------

data_articulo <- read_csv("2.Datos/data_articulo.csv")

glimpse(data_articulo)

# Elimino el Cuareim que no se usa en este analisis
# Edito los nombres de los programas del RN y RU
# Y genero las variable Year y Mes

data_articulo <-
  data_articulo %>% filter (nombre_programa != ("Rio Cuareim")) %>%
  mutate (
    nombre_programa = fct_recode(nombre_programa,
                                 RN = "Río Negro",
                                 RU = "Rio Uruguay"),
    Year = year(fecha_muestra),
    Mes = month (fecha_muestra)
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

# Datos de pH cortados respecto al articulo
summary(data_articulo$Ph)

ggplot(data_articulo, aes(y = Ph, x = 1)) + geom_violin()

data_articulo %>%  group_by (nombre_programa) %>%
  filter (TempAgua < 200) %>% # Datos extremos
  summarize (
    mediapH = mean (Ph, na.rm = T),
    meanTemp = mean(TempAgua, na.rm = T),
    meanPT = mean (FosforoTotal , na.rm = T)
  )


# 2.2. Analysis of variables by linear model ---------------------------------

ph_rn <- data_articulo %>%
  filter (BeretOut == "inn" & nombre_programa == "RN") %>%
  dplyr::select(Ph, codigo_pto, Year, Mes)

ph_rn_model <-
  lme4::lmer(Ph ~ Year  + (1 | codigo_pto / Mes), data = ph_rn)

df.residual(ph_rn_model)

summary(ph_rn_model)

#Alcalinidad

Alc_rn <- data_articulo %>%
  filter (BeretOut == "inn" & nombre_programa == "RN") %>%
  dplyr::select(Alcalinidad, codigo_pto, Year, Mes)

Alc_rn_model <-
  lme4::lmer(Alcalinidad ~ Year  + (1 |
                                      codigo_pto / Mes), data = Alc_rn)

summary(Alc_rn_model)


# NO COINCIDEN LOS Grados de Libertad!!


# Render RMarkdown ------------------------------------------------- 
rmarkdown::render(
  input="3.Resultados/Resultados_preliminares.Rmd",
  output_format = "pdf_document",
  output_dir = "3.Resultados",
  run_pandoc = TRUE,
  output_file = paste0("Resultados_preliminares.pdf")
)
