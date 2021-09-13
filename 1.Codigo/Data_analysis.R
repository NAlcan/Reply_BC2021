# Functions load -----------------------------------------------------
library(tidyverse) # Data load/manipulation and graphics build
library(RColorBrewer) # Color Palettes
library(lubridate) # Day/Time Manipulation
library(patchwork) # Plot Layout
library(png) # Import `png` images

theme_set(theme_minimal()) #  Graphic theme used for visualizations


# Data Load ---------------------------------------------------------------

data_oan <- read_csv("2.Datos/working_data/bc2021_data.csv")

# glimpse(data_oan)

# Define de row names that belongs to ID of data like, date, site, river...

id_vars <- c("date","date_time", "estacion","river","data_model")

# Rename Variables for easy coding

data_oan <- data_oan %>% 
  rename(chla = "clorofila_a_mg_l",
         alk = "alcalinidad_total_mg_ca_co3_l",
         cond = "conductividad_m_s_cm",
         tot_phos = "fosforo_total_mg_p_l",
         tss = "solidos_suspendidos_totales_mg_l",
         pH = "potencial_de_hidrogeno_p_h_sin_unid",
         temp = "temperatura_o_c")

# 99.5 percentile of Chl-a -------------
# We Assume that BC2021 Calculated the 99.5 limit using Negro and Uruguay together
# Lets see how many data exceed with this criteria

data_oan %>% 
  filter(river %in% c("Negro","Uruguay")) %>% 
  filter(chla >= quantile(chla, probs = c(0.995), na.rm = T)) %>%
  dplyr::select(date, estacion, chla)
# Looks similar that they did, but with this criteria the RN12 for 2018-04-17 gets not excluded

# Function that calculates the number of elements that exceed 99.5 limit
q99.5_exceed <- function (x){
  q <- quantile(x, probs = c(0.995), na.rm =T)
  sum(x > q, na.rm = T)
}


# Number of data exceeds 99.5 for each variable in Uruguay and Negro
data_oan %>% filter(river %in% c("Uruguay", "Negro"))  %>% 
  summarize(across(where(is.numeric), q99.5_exceed)) 

# Group by River and calculate the number of data exceeds 99.5 for each variable
data_oan %>% group_by(river)  %>% 
summarize(across(where(is.numeric), q99.5_exceed)) 
  
# Q99.5 limit values for chla by river
data_oan %>% group_by(river) %>% 
  dplyr::select(date, estacion, chla, river) %>% 
  summarise(Chla_q99.5 = quantile(chla, probs = c(0.995), na.rm = T))

# Function that returns de value of Q99.5  
q_calc<- function(x) {
 q <- quantile(x, probs = c(0.995), names = F,na.rm = T)
}

# Apply de q_calc for each numeric column from Negro and Uruguay rivers
  
data_oan %>% 
    filter(river %in% c("Negro","Uruguay")) %>% 
  summarise( across(where(is.numeric), q_calc))


# Chla vs All all values  ---------------------------------------------------
# Generate de data frame to make one plot for each variable
# With a data-list each var transform into single data frame to make the plot

chla_allvalues <- data_oan %>%
  filter (river %in% c("Negro", "Uruguay")) %>% 
    pivot_longer(
    cols = !c(all_of(id_vars), chla)) %>% 
  group_by(name) %>% 
  nest()

# Function that make a plot for each variable 
figa1_func <- function (data) { 
  # Define de q99.5 limit  
q <- quantile(data$value, probs = c(0.995), names = F,na.rm = T)
  data %>% 
    mutate(extra = ifelse(value > q, "1","0") ) %>% 
  ggplot(aes(x = value , y = chla, fill = extra)) +
    geom_point(alpha = 0.8, size = 1.5, pch = 21) +
    scale_y_continuous(limits = c(0,60)) +
    scale_fill_manual(na.translate = FALSE , values = c("#984ea3", "#66a61e")) +
    labs(y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")"))) +
    theme(axis.title.y = element_text(size = 8)) +
    guides (fill = "none")
         }
# Apply figa1_function for each variable

data_chla_all <- chla_allvalues %>% 
  mutate(plots = map(data, figa1_func)) 

# Extract the plots information obly 
chla_plots<- data_chla_all %>%  ungroup() %>% 
  dplyr::select(plots) 

# Extract each plot for label variables adequately

figa1_alk <- chla_plots[[1]][[1]] +
  labs(x = expression(paste("Alkalinity (mg L"^-1,")")) )

figa1_ec <- chla_plots[[1]][[2]] +
  labs(x = expression(paste("EC"[w] ," (", mu,"S cm"^-1,")")) )

figa1_tp <- chla_plots[[1]][[3]] +
  labs(x = expression(paste("Total Phosphorus (", mu,"g L"^-1,")")) )

figa1_sst <- chla_plots[[1]][[4]] +
  labs(x = expression(paste("Total Suspended Solids ( mg L"^-1,")")) ) 

figa1_ph <- chla_plots[[1]][[5]] +
  labs(x = "pH" )

figa1_ta <- chla_plots[[1]][[6]] +
  labs(x = expression(paste("T (", degree,"C)")) ) 


fig_A1 <- wrap_plots( figa1_alk,figa1_ec,
                     figa1_tp,figa1_sst,figa1_ph,
                     figa1_ta, ncol = 2) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

ggsave(fig_A1, filename = "3.Resultados/FigureA1.tiff",
       dpi = "print",
       height =5.94  , width = 8.35)

# Will substitute all values that exceed 99.5 and replace by NA
# For Each variable in  Negro and Uruguay together (for similarity with the BC2021 procedure)
#  And for Cuareim alone

# Function that replace a values with NaN if it exceed 99.5 limit
q99.5_remove <- function(x){
  q = quantile(x, probs = c(0.995), na.rm = T)
  x_c = ifelse(x <= q , x , NaN)
}

# For Negro and Uruguay together
data_cut_NU <- data_oan %>% 
  filter (river %in% c("Negro","Uruguay")) %>% 
  mutate(across(where(is.numeric), q99.5_remove))

# For Cuareim only
data_cut_C <- data_oan %>% 
  filter (river == "Cuareim") %>% 
  mutate(across(where(is.numeric), q99.5_remove))

bc_data_limit <- bind_rows(data_cut_NU,data_cut_C) 

# chla vs environment  -----------------------------------------------------

# Figure 3 from BC 2021 recreation
# Maximum values for X axes according to BC2021(Figure 3)
plot_x_limits <- tribble(
  ~name, ~lmin, ~lmax,
  "alk", 0, 150,
  "cond",0, 300,
  "tot_phos", 0, 1000,
  "pH", 5,10,# The original BC2021 is 8
  "tss", 0, 600,
  "temp", 0 , 40
)

# Generate de data frame to make one plot for each variable
data_fig3 <- bc_data_limit %>% 
  filter (river != "Cuareim") %>% 
  dplyr::select(!all_of(id_vars)) %>% 
  pivot_longer (cols =! chla) %>% 
  group_by (name) %>% 
  nest()

## Add x limits
data_fig3 <- data_fig3 %>% 
  left_join(plot_x_limits, id = "name")

fig3_function_plot <- function (data, lmin,lmax,xlab) {
  ggplot(data, aes(x = value , y = chla)) +
    geom_point(alpha = 0.8, size = rel(0.5) ) +
    scale_x_continuous(limits = c( xmin = lmin , xmax = lmax)) +
    scale_y_continuous(limits = c(0,45),
                       breaks = seq(from = 0,to=48,by = 5)) +
    labs( x = xlab, y = expression(paste("Chlorophyll a (", mu,"g L"^-1,")"))) +
    theme_classic() +
    theme(axis.title = element_text(size = rel(0.7)),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
    
}

# Apply the plot function for each variable

data_fig3 <- data_fig3 %>% 
  mutate (plot = pmap(list(data,lmin,lmax,name), fig3_function_plot))

plots3<- data_fig3 %>%  ungroup() %>% 
  dplyr::select(plot) 

## Extract each plot for label variables accordingly

fig3_alk <- plots3[[1]][[1]] +
  labs(x = expression(paste("Alkalinity (mg L"^-1,")")) )+
  coord_fixed(ratio = 3)

fig3_ec <- plots3[[1]][[2]] +
  labs(x = expression(paste("EC"[w] ," (", mu,"S cm"^-1,")")) )+
  coord_fixed(ratio = 6)

fig3_tp <- plots3[[1]][[3]] +
  labs(x = expression(paste("Total Phosphorus (", mu,"g L"^-1,")")) ) +
  coord_fixed(ratio = 20)

fig3_sst <- plots3[[1]][[4]] +
  labs(x = expression(paste("Total Suspended Solids ( mg L"^-1,")")) ) +
  coord_fixed(ratio = 12)

fig3_ph <- plots3[[1]][[5]] +
  labs(x = "pH" ) +
  coord_fixed(ratio = 0.1)

fig3_ta <- plots3[[1]][[6]] +
  labs(x = expression(paste("T (", degree,"C)")) ) +
  coord_fixed(ratio = 0.8)  

# Load all panel figures copied from BC2021
library(png) # To upload png images scanned from BC2021
library(grid) # Convert PNG to Grob
library(ggplotify) # Convert Grob to GGPLOT
library(cowplot) # Paste plots together

f3a<-readPNG("2.Datos/Fig3BC/3a.png")
fig3a <- as.ggplot(grid::rasterGrob(f3a, interpolate=TRUE)) +
  theme(plot.margin = unit(c(0, -1, 0, -1), "cm"))

f3b<-readPNG("2.Datos/Fig3BC/3b.png")
fig3b <-as.ggplot(grid::rasterGrob(f3b, interpolate=TRUE))+
  theme(plot.margin = unit(c(0, -1, 0, -1), "cm"))

f3c<-readPNG("2.Datos/Fig3BC/3c.png")
fig3c <- as.ggplot(grid::rasterGrob(f3c, interpolate=TRUE)) +
  theme(plot.margin = unit(c(0, -1, 0, -1), "cm"))

f3d<-readPNG("2.Datos/Fig3BC/3d.png")
fig3d <- as.ggplot(grid::rasterGrob(f3d, interpolate=TRUE)) +
  theme(plot.margin = unit(c(0, -1, 0, -1), "cm"))

f3e<-readPNG("2.Datos/Fig3BC/3e.png")
fig3e <- as.ggplot(grid::rasterGrob(f3e, interpolate=TRUE))+
  theme(plot.margin = unit(c(0, -1, 0, -1), "cm"))

f3f<-readPNG("2.Datos/Fig3BC/3f.png")
fig3f <- as.ggplot(grid::rasterGrob(f3f, interpolate=TRUE))+
  theme(plot.margin = unit(c(0, -1, -.2, -1), "cm"))


plots_juntos3 <- plot_grid(fig3a, fig3_alk ,
                           fig3b, fig3_ec ,
                           fig3c, fig3_tp ,
                           fig3d, fig3_ph ,
                           fig3e, fig3_sst ,
                           fig3f, fig3_ta ,
                           labels = c("", "a'",
                              "", "b'",
                              "", "c'",
                              "", "d'",
                              "", "e'",
                              "", "f'"),
                           label_x=0.8,
                           label_fontface = "plain",
                           label_fontfamily = "times", ncol = 2)
                          


png(filename = "3.Resultados/Figure3.png", pointsize = 18, width = 10, height = 20, 
    res = 300 , units = "cm")
plots_juntos3
dev.off()


# By rivers ---------------------------------------------------------------

# Lets compare differences within rivers used for train model (Negro and Uruguay)

diff_rivers_data <- bc_data_limit %>% 
  mutate(logChla = log10(chla)) %>% 
  pivot_longer (!all_of(id_vars)) %>% 
  mutate (river_label = fct_recode(river,
                                   UR = "Uruguay",
                                   NR = "Negro",
                                   CR = "Cuareim")) %>% 
  group_by(name) %>% 
  nest()
  
fig4_function <- function(data) {
  p1 <- ggplot(data, aes(y = value, x = river_label)) + 
    geom_violin(aes(fill = river)) +
    scale_fill_manual(na.translate = FALSE , 
                      values = c("#d95f02", "#1b9e77","#984ea3"))+
    theme(legend.position = "none",
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(0.7))) +
    labs(x = NULL) +
    facet_grid( ~ fct_relevel(data_model,"Train","Test"),
                scales = "free_x",
                space = "free_x")
  
}

# Apply the plot function for each variable 
fig4_rivers <- diff_rivers_data %>% 
  mutate(plots = map(data, fig4_function))

# Extract only plots information
plots4<-fig4_rivers %>%  ungroup() %>% 
  dplyr::select(plots) 

##

fig4_chla <- plots4[[1]][[1]] +
  labs(y =  expression(paste("Chlorophyll a (", mu,"g L"^-1,")")) )


fig4_alk <- plots4[[1]][[2]] +
  labs(y = expression(paste("Alkalinity (mg L"^-1,")")) )

fig4_ec <- plots4[[1]][[3]] +
  labs(y = expression(paste("EC"[w] ," (", mu,"S cm"^-1,")")) )

fig4_tp <- plots4[[1]][[4]] +
  labs(y = expression(paste("Total phosphorus (", mu,"g L"^-1,")")) )

fig4_sst <- plots4[[1]][[5]] +
  labs(y = expression(paste("Total Suspended Solid ( mg L"^-1,")"))) +
  theme(axis.title.y =  element_text(size = 10))

fig4_ph <- plots4[[1]][[6]] +
  labs(y = "pH" )

fig4_ta <- plots4[[1]][[7]] +
  labs(y = expression(paste("T (", degree,"C)")) )

fig4_logchla <- plots4[[1]][[8]] +
  labs(y =  expression(paste("log"[10], "(Chlorophyll a)")) )


figure4 <- wrap_plots(fig4_chla, fig4_logchla,
                      fig4_alk,fig4_ec,
                      fig4_tp,fig4_sst,fig4_ph,
                      fig4_ta, ncol = 2)

ggsave(figure4, filename = "3.Resultados/figure4_rivercomp.tiff", 
       dpi = "print",
       height =5.94  , width = 8.35)


# Generalized Least Squares for river comparision ---------------------------------------------------------
## This GLS function allows for compare differences between mean and variances
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


gls_rivers <- diff_rivers_data %>% 
  unnest(cols = data) %>% 
  filter (river != "Cuareim") %>% 
  rename(x = value ,
        group = river ) %>% 
mutate (group = factor(group) )%>% 
  nest() %>% 
  mutate(gls = map(data, gls.var.test)) %>% 
  unnest(gls) %>% 
  mutate( VarTest = ifelse( VarTest <= 0.05, "p<0.05","n.s"),
          MeanTest = ifelse( MeanTest <= 0.05, "p<0.05","n.s")) %>% 
  dplyr::select(name,VarTest, MeanTest)

gls_rivers
