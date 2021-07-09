## Llamar paquetes ----
library(tidyverse) # arreglar datos (dplyr) y graficar (ggplot2)
library(sf) # simple features para trabajar con datos espaciales
library(mapsf) # helps to design various cartographic representations 
library(geouy) # easily access official spatial data sets of Uruguay
library(ggspatial)
library(viridis) # color palette
library(snakecase) # clean levels names of categorical variables
###

# Data sites --------------------------------------------------------------
# Load data site and project as points
sites <- read_csv("2.Datos/coordenadas_sitios.csv")

sites_longlat <- sitios %>% 
  st_as_sf (coords = c("longitud","latitud")) %>% 
  st_sf (crs = 4326)


# Load Uruguay border catrography from package `geouy`
borders <- load_geouy("Uruguay")
 
rivers <- load_geouy("Cursos de agua navegables y flotables") %>% # Rivers cartography 
  st_cast("MULTILINESTRING") # Convert multicurve to multistring https://github.com/r-spatial/sf/issues/1194

# Selected rivers to plot
# A vector with river names separated by conditional symbol "|"
this_rivers <- paste("uruguay",
                       "cuareim",
                       "negro",
                       "tacuarembó",
                       "tacuarembó_chico",
                       "de_la_plata",
                       "san_salvador", sep = "|")

#
cursos <- cursos %>%
  mutate(nombre = to_snake_case(nombre))# %>% 
cursos %>% filter(str_detect( nombre, c()))

# Reservoirs and lagoons cartography dowloaded from DIva-Gis Free Spatial Data at country level `http://diva-gis.org/gdata`
espejos <- st_read ("2.Datos/gis_data/ury_inlandwater") %>%  # Uruguay/InlandWater
filter(NAME %in% c("RIO URUGUAY (URUGUAY)","EMBALSE DEL RIO NEGRO"))


# Mapa base
base_map <- ggplot() +
  geom_sf (data = contorno, fill = "NA", color = "grey40") +
  geom_sf (data = cursos2, color = "#1f78b4") + 
  geom_sf (data = espejos, color = "#1f78b4") + 
  theme_bw() +
  theme(panel.grid = element_blank())

## Voy a cargar los puntos con las coordenadas

nombres_rios <- espejos %>% 
  filter(NAME %in% c("RIO URUGUAY (URUGUAY)","EMBALSE DEL RIO NEGRO")) %>% 
  mutate(NAME = fct_recode (NAME, 
                        `Uruguay River` = "RIO URUGUAY (URUGUAY)",
                        `Negro River` = "EMBALSE DEL RIO NEGRO"))



base_map +
  geom_sf(data = sitios_longlat, aes(fill = status), pch = 21 ,size = 2, color= "black") + 
  scale_fill_brewer( palette = "Spectral") +
  labs (fill = "Point Status") + 
  geom_sf_text(data = nombres_rios, aes( label = NAME), size = 4) +
  annotation_scale(location = "br", width_hint = 0.2,
                   text_cex = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(2.2, "in"), pad_y = unit(2.4, "in"),
                         style = north_arrow_fancy_orienteering) 


  

# leaflet::leaflet(data = sitios_longlat) %>% 
#   leaflet::addTiles() %>%
#   leaflet::addMarkers( group = ~ status)



# #
# ## Con `mapsf`
# mf_init(x = rios_1)
# # Plot a shadow
# mf_shadow(rios_1, col = "grey10", add = TRUE)
# 
# mf_map(x = rios_1,  add = TRUE )
# mf_map(x = rios_2,  add = TRUE, alpha = 0.3)
# 
# # Start an inset map
# mf_inset_on(x = "worldmap", pos = "right")
# # Plot the position of the sample dataset on a worlmap
# mf_worldmap(depart, col = "#0E3F5C")
# # Close the inset
# mf_inset_off()
# # Plot a title
# mf_title("")
# # Plot credits
# mf_credits("")
# # Plot a scale bar
# mf_scale(size = 5)
# # Plot a north arrow
# mf_arrow('topleft')
# 
