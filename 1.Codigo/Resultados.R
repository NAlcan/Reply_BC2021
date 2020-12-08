# Funciones -----------------------------------------------------
library(tidyverse) # Funciones de manipulacion de la base y graficos
library(RColorBrewer) #Paleta de colores

# Datos -------------------------------------------------------------------

data_articulo <- read_csv("2.Datos/data_articulo.csv")

glimpse(data_articulo)

id_variables <- c("nombre_programa","codigo_pto","departamento","fecha_muestra",
                  "fecha_hora","id_muestra") 
# Voy a generar la variable "beret" que identifica los datos excluidos en articulos

data_articulo <- data_articulo %>% 
  mutate(BeretOut = factor( ifelse ((codigo_pto == "RN12" & fecha_muestra == "2018-04-17"| 
                            codigo_pto == "RN5" & fecha_muestra == "2012-01-19" |
                            codigo_pto == "RN5" & fecha_muestra == "2012-12-06" |
                            codigo_pto == "RN6" & fecha_muestra == "2012-08-15" |
                            codigo_pto == "RN3" & fecha_muestra == "2012-12-05"  ), "out","inn")))

id_variables <- c("nombre_programa","codigo_pto","departamento","fecha_muestra",
                  "fecha_hora","id_muestra","BeretOut") 

ggplot(data_articulo,aes(y=Clorofila_a, x=nombre_programa)) +   geom_jitter(aes(color = BeretOut)) + 
  theme_minimal()
  
ggplot(data_articulo, aes(y=Clorofila_a, x = FosforoTotal)) +
  geom_point(aes(color = BeretOut)) + facet_wrap( .~ nombre_programa) +
  theme_minimal()


Plot_pred <- data_articulo %>%
  pivot_longer(-c(all_of(id_variables), Clorofila_a),
               names_to="xvar",values_to="value") %>%
  ggplot(aes(x = value, y = Clorofila_a)) + geom_point(aes(color=BeretOut)) +
  scale_color_manual(values = c("#d95f02","#1b9e77")) +
  facet_wrap( nombre_programa ~ xvar , scales="free") 
 
