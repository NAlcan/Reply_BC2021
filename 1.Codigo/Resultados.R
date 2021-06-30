# Funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos
library(RColorBrewer) #Paleta de colores
library(lubridate) # para manipular fechas
theme_set(theme_minimal()) # Theme por defecto de los graficos
library(patchwork) # Para pegar plots
library(png) # para importar figuras externas
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



# Descriptivos ------------------------------------------------------------

data_articulo %>% filter (BeretOut != "out" & nombre_programa != "RC") %>%
  summarise (
    max = max(Clorofila_a, na.rm = T),
    min = min(Clorofila_a, na.rm = T),
    median = median(Clorofila_a, na.rm = T),
    media = mean (Clorofila_a, na.rm = T),
    n = n()
  )


# Graficos

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

# outliers ----------------------------------------

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

#Recreación --------------------------------------------

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
  labs( x = NULL, y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")"))) 
}

# Cada plot lo almaceno en una columna nueva

data_figbbll2 <- data_figbbll2 %>% 
  mutate(gg = pmap(list(data,lmin,lmax), plot_function))

# Extraigo la lista 
plots<-data_figbbll2 %>%  ungroup() %>% 
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


# Hay muchos datos extremos o "outliers" que el articulo no es claro bajo que criterio

# Este no recorta eje x, el y se le deja el mismo que BBLL se podria cambiar a umbral995
plot_function2 <- function(data) {
  p1 <- ggplot(data, aes(x = valor , y = Clorofila_a)) +
    geom_point(alpha = 0.5, size = 2 ) +
    scale_y_continuous(limits = c(0,45)) +
    labs( x = NULL, y = NULL)
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

p1.2_alc <- plots2[[1]][[1]]$data %>% 
    mutate(extra = ifelse (valor > limits$Alcalinidad[[1]], "0", "1" )) %>% 
     ggplot(aes(x = valor , y = Clorofila_a, fill = extra)) +
     geom_point(alpha = 0.8, size = 2.5, pch = 21) +
     scale_y_continuous(limits = c(0,45)) +
   scale_fill_manual(na.translate = FALSE , values = c("#984ea3", "#66a61e")) +
     labs(y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")")),
          x = expression(paste("Alkalinity (mg L"^-1,")"))) +
  guides(fill = F)
  

p2.2_ec <- plots2[[1]][[2]]$data %>% 
  mutate(extra = ifelse (valor > limits$Conductividad[[1]], "0", "1" )) %>% 
  ggplot(aes(x = valor , y = Clorofila_a, fill = extra)) +
  geom_point(alpha = 0.8, size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,45)) +
  scale_fill_manual(na.translate = FALSE , values = c("#984ea3", "#66a61e")) +
  labs(y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")")),
       x = expression(paste("EC"[w] ," (", mu,"S cm"^-1,")")) ) +
  guides (fill = F)


p3.2_tp <- plots2[[1]][[3]]$data %>% 
  mutate(extra = ifelse (valor > limits$FosforoTotal[[1]], "0", "1" )) %>% 
  ggplot(aes(x = valor , y = Clorofila_a, fill = extra)) +
  geom_point(alpha = 0.8, size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,45)) +
  scale_fill_manual(na.translate = FALSE , values = c("#984ea3", "#66a61e")) +
  labs(y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")")), 
       x = expression(paste("Total phosphorus (", mu,"g L"^-1,")")) ) +
  guides(fill = F)

# Ojo que ph esta en distinto orden po eso 5
p4.2_ph <- plots2[[1]][[5]]$data %>% 
  mutate(extra = ifelse (valor > limits$Ph[[1]], "0", "1" )) %>% 
  rename(  "pH" = valor ) %>% 
  ggplot(aes(x = pH , y = Clorofila_a, fill = extra)) +
  geom_point(alpha = 0.8, size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,45)) +
  scale_fill_manual(na.translate = FALSE , values = c("#984ea3", "#66a61e")) +
  labs(y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")")) ) +
  guides (fill = F)

p5.2_sst <- plots2[[1]][[4]]$data %>% 
  mutate(extra = ifelse (valor > limits$SolidosTotales[[1]], "0", "1" )) %>% 
  ggplot(aes(x = valor , y = Clorofila_a, fill = extra)) +
  geom_point(alpha = 0.8, size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,45)) +
  scale_fill_manual(na.translate = FALSE , values = c("#984ea3", "#66a61e")) +
  labs(y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")")), 
  x = expression(paste("Total Suspended Solid ( mg L"^-1,")"))) +
  guides (fill = F)

p6.2_ta <- plots2[[1]][[6]]$data %>% 
  mutate(extra = ifelse (valor > limits$TempAgua[[1]], "No", "Yes" )) %>% 
  ggplot(aes(x = valor , y = Clorofila_a, fill = extra)) +
  geom_point(alpha = 0.8, size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,45)) +
  scale_fill_manual(na.translate = FALSE , values = c("#984ea3", "#66a61e")) +
  labs(fill = "> 99.5",
       y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")")),
       x = expression(paste("T (", degree,"C)"))) +
  theme(legend.position = "bottom")

plots_juntos2.2 <- wrap_plots(p1.2_alc,p2.2_ec,p3.2_tp,p4.2_ph,p5.2_sst,p6.2_ta, ncol = 2, widths = 1)

# Figura 3 Nuestras vs BC --------------------------------------------------
# Pero ya lo trabajo con limits

data_figPosta <- data_limits %>%   filter (BeretOut == "inn" & nombre_programa != "RC") %>% 
  dplyr::select(!(c(all_of(id_variables),Year, Mes))) %>%
  pivot_longer(
    cols = !(Clorofila_a),
    names_to = "vars",
    values_to = "valor"
  ) %>% group_by(vars) %>% 
  nest()

data_figPosta <- data_figPosta %>% 
  left_join(plot_x_limits, id = "vars" )


plot_function2.2 <- function(data, lmin, lmax) {
  p1 <- ggplot(data, aes(x = valor , y = Clorofila_a)) +
    geom_point(alpha = 0.5, size = 2 ) +
    scale_x_continuous(limits = c( xmin = lmin , xmax = lmax)) +
    scale_y_continuous(limits = c(0,45)) +
    labs( x = NULL, y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")"))) 
}

fig3_limited <-data_figPosta %>% 
  mutate(gg = pmap(list(data,lmin,lmax), plot_function2.2))

# Extraigo la lista 
plots3<-fig3_limited %>%  ungroup() %>% 
  dplyr::select(gg) 

plot3_limits <- wrap_plots(plots3$gg, ncol = 2)

## Agrego nombre de variables en eje x
p1.3_alc <- plots3[[1]][[1]] +
  labs(x = expression(paste("Alkalinity (mg L"^-1,")")) )

p2.3_ec <- plots3[[1]][[2]] +
  labs(x = expression(paste("EC"[w] ," (", mu,"S cm"^-1,")")) )

p3.3_tp <- plots3[[1]][[3]] +
  labs(x = expression(paste("Total phosphorus (", mu,"g L"^-1,")")) )

# Ojo que ph esta en distinto orden po eso 5
p4.3_ph <- plots3[[1]][[5]] +
  labs(x = "pH" )

p5.3_sst <- plots3[[1]][[4]] +
  labs(x = expression(paste("Total Suspended Solid ( mg L"^-1,")")) )

p6.3_ta <- plots3[[1]][[6]] +
  labs(x = expression(paste("T (", degree,"C)")) )

library(png) # To upload png images from BL
library(grid) # Convert PNG to Grob
library(ggplotify) # Convert Grob to GGPLOT
library(cowplot) # Paste plots toghetr

f3a<-readPNG("2.Datos/Fig3BC/3a.png")
fig3a <- as.ggplot(grid::rasterGrob(f3a, interpolate=TRUE))
f3b<-readPNG("2.Datos/Fig3BC/3b.png")
fig3b <-as.ggplot(grid::rasterGrob(f3b, interpolate=TRUE))
f3c<-readPNG("2.Datos/Fig3BC/3c.png")
fig3c <- as.ggplot(grid::rasterGrob(f3c, interpolate=TRUE))
f3d<-readPNG("2.Datos/Fig3BC/3d.png")
fig3d <- as.ggplot(grid::rasterGrob(f3d, interpolate=TRUE))
f3e<-readPNG("2.Datos/Fig3BC/3e.png")
fig3e <- as.ggplot(grid::rasterGrob(f3e, interpolate=TRUE))
f3f<-readPNG("2.Datos/Fig3BC/3f.png")
fig3f <- as.ggplot(grid::rasterGrob(f3f, interpolate=TRUE))

figura3a <- p1.3_alc + fig3a +
  plot_layout(widths = 2, heights = unit(4, 'cm')) +
  theme(plot.margin = margin(2, 2, 2, 2))

plots_juntos3 <- plot_grid(fig3a, p1.3_alc,
                           fig3b, p2.3_ec,
                           fig3c, p3.3_tp,
                           fig3d, p4.3_ph,
                           fig3e, p5.3_sst,
                           fig3f, p6.3_ta)


### Repito el plot pero separo por programa

plot_function3 <- function(data) {
  p1 <- ggplot(data, aes(x = valor , y = Clorofila_a)) + 
    geom_point(aes(color = nombre_programa), alpha = 0.5) +
    scale_y_continuous(limits = c(0,45)) +
    facet_grid( nombre_programa ~. ) +
    labs( x = NULL, y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")"))) +
    scale_color_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77")) +
    theme(legend.position = "none")
}


# Programas rios ----------------------------------------------------------


data_programas <- data_limits %>%   filter (BeretOut == "inn" & nombre_programa != "RC") %>% 
  dplyr::select(!(c(all_of(id_variables[-1]),Year, Mes))) %>%
  pivot_longer(
    cols = !c(Clorofila_a,nombre_programa),
    names_to = "vars",
    values_to = "valor"
  ) %>% group_by(vars) %>% 
  nest()

fig4_programas <- data_programas %>% 
  mutate(gg = map(data, plot_function3))
# Extraigo la lista 
plots4<-fig4_programas %>%  ungroup() %>% 
  dplyr::select(gg) 

plot4_programas <- wrap_plots(plots4$gg, ncol = 2)

## Agrego nombre de variables en eje x
p1.4_alc <- plots4[[1]][[1]] +
  labs(x = id_article_vars[[1]] )

p2.4_ec <- plots4[[1]][[2]] +
  labs(x = id_article_vars[[2]] )

p3.4_tp <- plots4[[1]][[3]] +
  labs(x = id_article_vars[[3]] )

# Ojo que ph esta en distinto orden po eso 5
p4.4_ph <- plots4[[1]][[5]] +
  labs(x = id_article_vars[[5]] )

p5.4_sst <- plots4[[1]][[4]] +
  labs(x = id_article_vars[[4]] )

p6.4_ta <- plots4[[1]][[6]] +
  labs(x = id_article_vars[[6]] )

plots_juntos4 <- wrap_plots(p1.4_alc,p2.4_ec,p3.4_tp,p4.4_ph,p5.4_sst,p6.4_ta, ncol = 2)


# FQ entre rios -----------------------------------------------------------
# Agrego la clorofila como una variable mas a ser testeada.

diff_programas <- data_limits %>% 
    dplyr::select(!(c(all_of(id_variables[-1]),Year, Mes))) %>%
  pivot_longer(
    cols = !c(nombre_programa),
    names_to = "vars",
    values_to = "x"
  ) %>% 
  rename("group" = nombre_programa) %>% 
  mutate(group = fct_recode(group,
    `Uruguay river` = "RU",
    `Negro river` = "RN"
  )) %>% 
  group_by(vars) %>% 
    nest()

# GLS entre rios --------------------------------
### Gls para comparar diferencias en medias o desvios
library(nlme)
gls.var.test<-function(data){
  x = data$x
  group = data$group
    if(is.null(group)){ data.gls<-x; colnames(data.gls)<-c("x","group")} else data.gls<-data.frame(x,group)
  
  if(any(!is.finite(data.gls[,1]))){ data.gls<-data.gls[which(is.finite(data.gls[,1])),]; warning("there were NaNs in the original x data")}# SACA LOS NAN
  if(any(!is.finite(data.gls[,2]))){ data.gls<-data.gls[which(is.finite(data.gls[,2])),]; warning("there were NaNs in the original group data")}# SACA LOS NAN
  
  modHeteroVar = gls(x~group, data=data.gls, weights = varIdent(form = ~1|group), method="ML") # Heterogeneous variance
  modHomoVar   = gls(x~group, data=data.gls, method="ML") # Homogeneous Variance
  modEqualMean = gls(x~1,     data=data.gls, weights = varIdent(form = ~1|group), method="ML")# Same mean all groups, different variance
  gls_data <- data.frame(VarTest=anova(modHeteroVar,modHomoVar)$`p-value`[2],
                    MeanTest=anova(modEqualMean,modHeteroVar)$`p-value`[2]) # Evaluar el loglikelyhood ratio test. p>0.01
    }


gls_porgramas <- diff_programas %>% 
  mutate(gls = map(data, gls.var.test)) %>% 
  unnest(gls) %>% 
  mutate( VarTest = ifelse( VarTest <= 0.05, "p<0.05","n.s"),
          MeanTest = ifelse( MeanTest <= 0.05, "p<0.05","n.s")) %>% 
  dplyr::select(vars,VarTest, MeanTest)


# Violines

plot_function4 <- function(data) {
  p1 <- ggplot(data, aes(y = x, x = group)) + 
    geom_violin(aes(fill = group)) +
    scale_fill_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77","#984ea3"))+
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12)) +
    labs(x = NULL)
  
}

fig5_programas <- diff_programas %>% 
  mutate(gg = map(data, plot_function4))

# Extraigo la lista 
plots5<-fig5_programas %>%  ungroup() %>% 
  dplyr::select(gg) 

plot5_programas <- wrap_plots(plots5$gg, ncol = 2)

## Agrego nombre de variables en eje y
p1.5_clo <- plots5[[1]][[1]] +
  labs(y =  expression(paste("Chlorophyll a (", mu,"g L"^-1,")")) )


p2.5_alc <- plots5[[1]][[2]] +
  labs(y = expression(paste("Alkalinity (mg L"^-1,")")) )

p3.5_ec <- plots5[[1]][[3]] +
  labs(y = expression(paste("EC"[w] ," (", mu,"S cm"^-1,")")) )

p4.5_tp <- plots5[[1]][[4]] +
  labs(y = expression(paste("Total phosphorus (", mu,"g L"^-1,")")) )

p5.5_sst <- plots5[[1]][[5]] +
  labs(y = expression(paste("Total Suspended Solid ( mg L"^-1,")")))

p6.5_ph <- plots5[[1]][[6]] +
  labs(y = "pH" )

p7.5_ta <- plots5[[1]][[7]] +
  labs(y = expression(paste("T (", degree,"C)")) )

plots_juntos5 <- wrap_plots(p1.5_clo, p2.5_alc,p3.5_ec,p4.5_tp,p5.5_sst,p6.5_ph,p7.5_ta, ncol = 2)

ggsave(plots_juntos5, filename = "3.Resultados/Violines.png", height = 6.7  , width = 6)

## Diferencias entre programas: histograma
plot_function5 <- function(data) {
  p1 <- ggplot(data, aes(x = valor)) + 
    geom_histogram(aes(fill = nombre_programa), alpha = 0.7) +
    theme(legend.position = "none") +
    scale_fill_manual(na.translate = FALSE , values = c("#d95f02", "#1b9e77"))+
    labs(x = NULL)
}

fig6_programas <- data_programas %>% 
  mutate(gg = map(data, plot_function5))
# Extraigo la lista 
plots6<-fig6_programas %>%  ungroup() %>% 
  dplyr::select(gg) 

plot6_programas <- wrap_plots(plots6$gg, ncol = 2)

## Agrego nombre de variables en eje x
p1.6_alc <- plots6[[1]][[1]] +
  labs(x = id_article_vars[[1]] )

p2.6_ec <- plots6[[1]][[2]] +
  labs(x = id_article_vars[[2]] )

p3.6_tp <- plots6[[1]][[3]] +
  labs(x = id_article_vars[[3]] )

# Ojo que ph esta en distinto orden po eso 5
p4.6_ph <- plots6[[1]][[5]] +
  labs(x = id_article_vars[[5]] )

p5.6_sst <- plots6[[1]][[4]] +
  labs(x = id_article_vars[[4]] )

p6.6_ta <- plots6[[1]][[6]] +
  labs(x = id_article_vars[[6]], 
       fill = "Sistema") + 
  theme(legend.position = "bottom")

plots_juntos6 <- wrap_plots(p1.6_alc,p2.6_ec,p3.6_tp,p4.6_ph,p5.6_sst,p6.6_ta, ncol = 2)



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




# Pairs -------------------------------------------------------------------

# Matriz de correlaciones de todas las variables del OAN
data_complet <- read_csv("2.Datos/tidyOAN_datacomplet.csv")
glimpse(data_complet)


data_complet <- data_complet %>%  filter (
  nombre_programa == "Río Negro" & fecha_muestra >= "2009-05-01" |
    nombre_programa == "Río Negro" &
    fecha_muestra <= "2018-11-30" |
    nombre_programa == "Rio Uruguay" &
    fecha_muestra >= "2014-06-01" |
    nombre_programa == "Rio Uruguay" &
    fecha_muestra <= "2018-11-30" ) %>% 
  dplyr::select("clorofila_a_mg_l",
    "alc_t_mg_ca_co3_l",
    "conduc_m_s_cm",
    "pt_mg_p_l",
    "sst_mg_l",
    "p_h_sin_unid",
    "t_o_c",
    "od_mg_l",
    "po4_mg_po4_p_l",
    "no3_mg_no3_n_l",
    "no2_mg_no2_n_l",
    "n_amoniacal_mg_nh4_n_l",
    "nt_mg_n_l")


# Remuevo Outliers al 99.5
limits2 <- data_complet %>%
  summarise(across(where(is.numeric) , ~ quantile(., probs = c(0.995), na.rm = T)))

# Objeto nuvo donde si la variable supera el limita el dato se cambia por NA
data_limits2<- 
  data_complet %>% 
  mutate(clorfila = ifelse(clorofila_a_mg_l <= limits2$clorofila_a_mg_l, clorofila_a_mg_l, NaN),
         Alcalinidad = ifelse(alc_t_mg_ca_co3_l <= limits2$alc_t_mg_ca_co3_l, alc_t_mg_ca_co3_l, NaN),
         Conductividad = ifelse(conduc_m_s_cm <= limits2$conduc_m_s_cm,conduc_m_s_cm, NaN),
         FosforoTotal = ifelse(pt_mg_p_l <=  limits2$pt_mg_p_l, pt_mg_p_l, NaN),
         SolidosTotales = ifelse(sst_mg_l <= limits2$sst_mg_l, sst_mg_l,NaN),
         Ph = ifelse (p_h_sin_unid <= limits2$p_h_sin_unid,p_h_sin_unid, NaN),
         TempAgua = ifelse(t_o_c <= limits2$t_o_c, t_o_c,  NaN),
         O2 = ifelse(od_mg_l <= limits2$od_mg_l, od_mg_l,  NaN),
         po4 = ifelse(po4_mg_po4_p_l <= limits2$po4_mg_po4_p_l, po4_mg_po4_p_l,  NaN),
         no3 = ifelse(no3_mg_no3_n_l <= limits2$no3_mg_no3_n_l, no3_mg_no3_n_l,  NaN),
         no2 = ifelse(no2_mg_no2_n_l <= limits2$no2_mg_no2_n_l, no2_mg_no2_n_l,  NaN),
         nh4 = ifelse(n_amoniacal_mg_nh4_n_l <= limits2$n_amoniacal_mg_nh4_n_l, n_amoniacal_mg_nh4_n_l,  NaN),
         nt = ifelse(nt_mg_n_l <= limits2$nt_mg_n_l, nt_mg_n_l,  NaN),
         .keep = "unused")

# # #

data_limits2 %>% 
  map_df(~ sum(is.na(.))) - 
  data_complet %>% 
  map_df(~ sum(is.na(.)))


library(GGally)
ggpairs(data_limits2)

# Render RMarkdown -------------------------------------------------
rmarkdown::render(
  input = "3.Resultados/Resultados_preliminares.Rmd",
  output_format = "pdf_document",
  output_dir = "3.Resultados",
  run_pandoc = TRUE,
  output_file = paste0("Resultados_preliminares.pdf")
)
