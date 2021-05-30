# Funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos
library(RColorBrewer) #Paleta de colores
library(lubridate) # para manipular fechas
theme_set(theme_minimal()) # Theme por defecto de los graficos
library(patchwork) # Para pegar plots
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
bereta_out1 <- data_articulo %>% filter( nombre_programa != ("RC")) %>% 
  summarize (Q99.5 = quantile(Clorofila_a, probs = c(0.995), na.rm = T))

data_articulo %>% filter (Clorofila_a >= bereta_out1$Q99.5) %>%
 dplyr::select(codigo_pto, fecha_muestra, Clorofila_a)


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

data_articulo %>% filter (BeretOut != "out" & nombre_programa != "RC") %>%
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
  dplyr::select(codigo_pto, fecha_muestra, Clorofila_a, TempAgua)

# Datos de pH cortados respecto al articulo. Figura 3.C
summary(data_articulo$Ph)


ggplot(data_articulo, aes(y = Ph, x = 1)) + geom_violin()

# Dato outliers en otras variables ----------------------------------------

# Los valores referencia de cada variable al P99.5%
limits <- data_articulo %>% filter ( BeretOut == "inn" & nombre_programa !=("RC")) %>%
  summarise(across(where(is.numeric) , ~ quantile(., probs = c(0.995), na.rm = T))) %>% 
  dplyr::select(!c(id_muestra,Year,Mes, Clorofila_a))

# Lista de los nombres de las variables(facilitar sacarlas para graficar)
id_article_vars <- colnames(limits)

# Objeto nuvo donde si la variable supera el limita el dato se cambia por NA
data_limits <- data_articulo %>% filter (BeretOut == "inn" & nombre_programa !=("RC")) %>% 
    mutate(Alcalinidad = ifelse(Alcalinidad <= limits$Alcalinidad, Alcalinidad, NaN),
            Conductividad = ifelse(Conductividad <= limits$Conductividad,Conductividad, NaN),
            FosforoTotal = ifelse(FosforoTotal <=  limits$FosforoTotal, FosforoTotal, NaN),
            SolidosTotales = ifelse(SolidosTotales <= limits$SolidosTotales, SolidosTotales,NaN),
            Ph = ifelse (Ph <= limits$Ph,Ph, NaN),
            TempAgua = ifelse(TempAgua <= limits$TempAgua, TempAgua,  NaN) )

# Cuantos datos se pasan de valor en cada variable
data_limits %>% dplyr::select( all_of(id_article_vars)) %>% 
  map_df(~ sum(is.na(.))) - 
  data_articulo %>%  filter ( BeretOut == "inn" & nombre_programa !=("RC")) %>% 
  dplyr::select( all_of(id_article_vars)) %>% 
  map_df(~ sum(is.na(.)))

# Recrear la figura 3 completa --------------------------------------------

# limites maximos de los ejes x, por cada plot (BBLL2020,Fig3)
plot_x_limits <- tribble(
~vars, ~lmin, ~lmax,
"Alcalinidad", 0, 150,
"Conductividad",0, 300,
"FosforoTotal", 0, 1000,
"Ph", 5,8,
"SolidosTotales", 0, 600,
"TempAgua", 0 , 40
)

# Un plot por variable y luego los pego. Para poner mismo rango en ejes, etc.
# Datos identicos a  BBLL2020 Fig3. Se usan los mismo rangos de x sin criterio para remover mas que ese
data_figbbll <- data_articulo %>%   filter (BeretOut == "inn" & nombre_programa != "RC") %>% 
    dplyr::select(!(c(all_of(id_variables),Year, Mes))) %>%
  pivot_longer(
    cols = !(Clorofila_a),
    names_to = "vars",
    values_to = "valor"
  ) %>% group_by(vars) %>% 
  nest()

# Le agrego los rangos de x para plot

data_figbbll2<-  data_figbbll %>% 
  left_join(plot_x_limits, id = "vars" )

# LA funcion que va hacer un plot para cada variable
plot_function <- function(data, xmin,xmax) {
 p1 <- ggplot(data, aes(x = valor , y = Clorofila_a)) + geom_point(alpha = 0.5) +
  scale_x_continuous(limits = c(xmin,xmax)) +
   scale_y_continuous(limits = c(0,45)) +
  labs( x = NULL, y = "Clorofila a ug.L") 
}

# Cada plot lo almaceno en una columna nueva

data_figbbll3 <- data_figbbll2 %>% 
  mutate(gg = pmap(list(data,lmin,lmax), plot_function))

# Extraigo la lista 
plots<-data_figbbll3 %>%  ungroup() %>% 
dplyr::select(gg) 

# Todos Juntos
plots_juntos <- wrap_plots(plots$gg, ncol = 2)

# Los voy a extraer para nombrar adecuadament eje x
p1_alc <- plots[[1]][[1]] +
  labs(x = id_article_vars[[1]] )

  
p2_ec <- plots[[1]][[2]] +
  labs(x = id_article_vars[[2]] )

p3_tp <- plots[[1]][[3]] +
  labs(x = id_article_vars[[3]] )

# Ojo que ph esta en distinto orden po eso 5
p4_ph <- plots[[1]][[5]] +
  labs(x = id_article_vars[[5]] )

p5_sst <- plots[[1]][[4]] +
  labs(x = id_article_vars[[4]] )

p6_ta <- plots[[1]][[6]] +
  labs(x = id_article_vars[[6]] )

plots_juntos2 <- wrap_plots(p1_alc,p2_ec,p3_tp,p4_ph,p5_sst,p6_ta, ncol = 2)


# Hay muchos datos extremos o "outlaiers" que el articulo no es claro bajo que criterio

# Este no recorta eje x, el y se le deja el mismo que BBLL se podria cambiar a umbral995
plot_function2 <- function(data) {
  p1 <- ggplot(data, aes(x = valor , y = Clorofila_a)) + geom_point(alpha = 0.5) +
    scale_y_continuous(limits = c(0,45)) +
    labs( x = NULL, y = "Clorofila a ug.L") 
}


data_figlarge <- data_figbbll2 %>% 
  mutate(gg = map(data, plot_function2)) # uso map xq ahora solo depende de data

# Extraigo la lista 
plots2<-data_figlarge %>%  ungroup() %>% 
  dplyr::select(gg) 

# Todos Juntos
plots_juntos2 <- wrap_plots(plots2$gg, ncol = 2)

# Los voy a extraer para nombrar adecuadament eje x 
# y ademas tengo los valores limites ej: limits$Alcalinidad

p1.2_alc <- plots[[1]][[1]]$data %>% 
    mutate(extra = ifelse (valor > limits$Alcalinidad[[1]], "0", "1" )) %>% 
   rename(  "Alcalinidad" = valor ) %>% 
     ggplot(aes(x = Alcalinidad , y = Clorofila_a, color = extra)) +
     geom_point(alpha = 0.8) +
     scale_y_continuous(limits = c(0,45)) +
   scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
     labs(y = "Clorofila a ug.L", color = "> 99.5") +
  theme(legend.position = "bottom")
  

p2.2_ec <- plots[[1]][[2]]$data %>% 
  mutate(extra = ifelse (valor > limits$Conductividad[[1]], "0", "1" )) %>% 
  rename(  "Conductividad" = valor ) %>% 
  ggplot(aes(x = Conductividad , y = Clorofila_a, color = extra)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(limits = c(0,45)) +
  scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
  labs(y = "Clorofila a ug.L", color = "> 99.5") +
  theme(legend.position = "bottom")


p3.2_tp <- plots[[1]][[3]]$data %>% 
  mutate(extra = ifelse (valor > limits$FosforoTotal[[1]], "0", "1" )) %>% 
  rename(  "FosforoTotal" = valor ) %>% 
  ggplot(aes(x = FosforoTotal , y = Clorofila_a, color = extra)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(limits = c(0,45)) +
  scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
  labs(y = "Clorofila a ug.L", color = "> 99.5") +
  theme(legend.position = "bottom")

# Ojo que ph esta en distinto orden po eso 5
p4.2_ph <- plots[[1]][[5]]$data %>% 
  mutate(extra = ifelse (valor > limits$Ph[[1]], "0", "1" )) %>% 
  rename(  "Ph" = valor ) %>% 
  ggplot(aes(x = Ph , y = Clorofila_a, color = extra)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(limits = c(0,45)) +
  scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
  labs(y = "Clorofila a ug.L", color = "> 99.5") +
  theme(legend.position = "bottom")

p5.2_sst <- plots[[1]][[4]]$data %>% 
  mutate(extra = ifelse (valor > limits$SolidosTotales[[1]], "0", "1" )) %>% 
  rename(  "SolidosTotales" = valor ) %>% 
  ggplot(aes(x = SolidosTotales , y = Clorofila_a, color = extra)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(limits = c(0,45)) +
  scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
  labs(y = "Clorofila a ug.L", color = "> 99.5") +
  theme(legend.position = "bottom")

p6.2_ta <- plots[[1]][[6]]$data %>% 
  mutate(extra = ifelse (valor > limits$TempAgua[[1]], "0", "1" )) %>% 
  rename(  "TempAgua" = valor ) %>% 
  ggplot(aes(x = TempAgua , y = Clorofila_a, color = extra)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(limits = c(0,45)) +
  scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
  labs(y = "Clorofila a ug.L", color = "> 99.5") +
  theme(legend.position = "bottom")

plots_juntos2.2 <- wrap_plots(p1.2_alc,p2.2_ec,p3.2_tp,p4.2_ph,p5.2_sst,p6.2_ta, ncol = 2)


## Aca van los violins

viol1_alc <- plots[[1]][[1]]$data %>% 
  mutate(extra = ifelse (valor > limits$Alcalinidad[[1]], "0", "1" )) %>% 
  rename(  "Alcalinidad" = valor ) %>% 
  ggplot(aes(y = Alcalinidad , x = 1)) +
  geom_violin() +
  geom_point(aes(color = extra)) +
  scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
  #labs(color = "> 99.5") +
  theme(legend.position = "bottom")






# Sino recorto nada
data_articulo %>%   filter (BeretOut == "inn" & nombre_programa != "RC") %>% 
  dplyr::select(c(all_of(id_article_vars), Clorofila_a)) %>%
  pivot_longer(
    cols = !(Clorofila_a),
    names_to = "vars",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor , y = Clorofila_a)) + geom_point(alpha = 0.5) +
  facet_wrap(~ vars , scales = "free_x", ncol = 2) +
  labs(x = NULL)




# Violin Plots 
## ESto HAY QUE ARREGLARLO.HACER UN PLOT POR VAIABLE
# Abajo el GEOM VIOLIN,
# ARIIBA los PUNTOS EXTREMOS EN ROJO
data_limits %>% 
  ggplot() +  
  geom_point(data = data_limits %>%   filter (BeretOut == "inn" & nombre_programa != "RC") %>% 
               dplyr::select(!(c(all_of(id_variables),Year, Mes, all_of(id_limits_vars)))) %>% 
               pivot_longer(
                 cols = !(Clorofila_a),
                 names_to = "vars",
                 values_to = "valor"
               ), aes (x = vars, y = valor), color = "red" ) +
  facet_wrap(~ vars , scales = "free", ncol = 2) +
    geom_violin( data = data_limits %>%   filter (BeretOut == "inn" & nombre_programa != "RC") %>% 
                   dplyr::select(all_of(id_limits_vars)) %>% 
                   pivot_longer(
                     cols = everything(),
                     names_to = "vars",
                     values_to = "valor"
                   ), aes ( x = vars, y = valor) ) +
    facet_wrap(~ vars , scales = "free", ncol = 2) +
    labs(x = NULL) 




data_articulo %>%   filter (BeretOut == "inn" & nombre_programa != "RC") %>% 
  dplyr::select(!(c(all_of(id_variables),Year, Mes))) %>%
  pivot_longer(
    cols = !(Clorofila_a),
    names_to = "vars",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = vars , y = valor)) + geom_violin(alpha = 0.5) +
  facet_wrap(~ vars , scales = "free", ncol = 2) +
  labs(x = NULL)



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
    valor ~ Year,
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
rn <- data_limits %>%
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

ru <- data_limits %>%
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
