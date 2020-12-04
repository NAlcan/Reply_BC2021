# Funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos


# Datos -------------------------------------------------------------------

data_articulo <- read_csv("2.Datos/data_articulo.csv")

glimpse(data_articulo)

# Voy a generar la variable "beret" que identifica los datos excluidos en articulos

data_articulo <- data_articulo %>% 
  mutate(beret = factor( ifelse ((codigo_pto == "RN12" & fecha_muestra == "2018-04-17"| 
                            codigo_pto == "RN5" & fecha_muestra == "2012-01-19" |
                            codigo_pto == "RN5" & fecha_muestra == "2012-12-06" |
                            codigo_pto == "RN6" & fecha_muestra == "2012-08-15" |
                            codigo_pto == "RN3" & fecha_muestra == "2012-12-05"  ), "out","inn")))

 
ggplot(data_articulo,aes(y=clorofila_a_mg_l, x=nombre_programa)) +   geom_jitter(aes(color = beret)) + 
  theme_classic()
  
ggplot(data_articulo, aes(y=clorofila_a_mg_l, x = pt_mg_p_l)) +
  geom_point(aes(color = beret)) + facet_wrap( .~ nombre_programa) +
  theme_classic()
