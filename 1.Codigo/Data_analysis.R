################################################################################
# This script include all analysis of the paper:
# "A reply to" Relevant factors in the eutrophication of the Uruguay River and the Río Negro"
# This script contain two main sections: Part 1: Replication of BC2021
#                                       Part 2: Analysis of river comparisons

################################################################################

# Functions load -----------------------------------------------------
library(tidyverse) # Data load/manipulation and graphics build
library(RColorBrewer) # Color Palettes
library(lubridate) # Day/Time Manipulation
library(patchwork) # Plot Layout
library(png) # Import `png` images

theme_set(theme_minimal()) #  Graphic theme used for visualizations


# Part 1: Replication of BC2021:

# 1.1 Data loading ---------------------------------------------------------------
urlfile <-"https://raw.githubusercontent.com/NAlcan/Reply_BC2021/master/2.Datos/working_data/bc2021_data.csv"
data_oan <- read.csv(urlfile)
str(data_oan) # [Sugerencia para simplificar]

# Definition of the columns of the type "ID" (names and dates). 
id_vars <- c("date", "date_time", "estacion", "river", "data_model")

# Renaming of variables for easy coding
data_oan <- data_oan %>%
  rename(
    chla = "clorofila_a_mg_l",
    alk = "alcalinidad_total_mg_ca_co3_l",
    cond = "conductividad_m_s_cm",
    tot_phos = "fosforo_total_mg_p_l",
    tss = "solidos_suspendidos_totales_mg_l",
    pH = "potencial_de_hidrogeno_p_h_sin_unid",
    temp = "temperatura_o_c"
  )

# 1.2 Data cleaning ---------------------------------------------------------------
# Calculation of the 99.5 percentiles of chlorophyll a for exclusion of outliers
# For this calculation it was assumed that BC2021 used Rio Negro and Rio Uruguay data altogether.

data_oan %>%
  filter(river %in% c("Negro", "Uruguay")) %>%
  filter(chla >= quantile(chla, probs = c(0.995), na.rm = T)) %>%
  dplyr::select(date, estacion, chla)

# Calculation of the number of data that exceeds the 99.5 percentile:
# Definition of function for this calculation:
q99.5_exceed <- function (x) {
  q <- quantile(x, probs = c(0.995), na.rm = T)
  sum(x > q, na.rm = T)
}


# The number of data that exceeds 99.5 for each variable (Rio Negro and Rio Uruguay altogether):
data_oan %>% filter(river %in% c("Uruguay", "Negro"))  %>%
  summarize(across(where(is.numeric), q99.5_exceed))

# Data that exceeds the 99.5 quantile for each variable grouped by river systems
data_oan %>% group_by(river)  %>%
  summarize(across(where(is.numeric), q99.5_exceed))

# Calculation of the 99.5 limit values for chla by each river system
data_oan %>% group_by(river) %>%
  dplyr::select(date, estacion, chla, river) %>%
  summarise(Chla_q99.5 = quantile(chla, probs = c(0.995), na.rm = T))

# Definition of function that returns the 99.5 quantile
q_calc <- function(x) {
  q <- quantile(x,
                probs = c(0.995),
                names = F,
                na.rm = T)
}

# Calculation of the 99.5 quantile for all variables using the q_calc function for Negro and Uruguay river separately.

data_oan %>%
  filter(river %in% c("Negro", "Uruguay")) %>%
  summarise(across(where(is.numeric), q_calc))


# 1.3 Chla versus environmental variables ---------------------------------------------------

# Comparison of bi-plots reported by BC2021 
# Creation of dataframe for each variable:

chla_allvalues <- data_oan %>%
  filter (river %in% c("Negro", "Uruguay")) %>%
  pivot_longer(cols = !c(all_of(id_vars), chla)) %>%
  group_by(name) %>%
  nest()

# Definition of function to create a plot for each variable

figa1_func <- function (data) {
  # Define de q99.5 limit
  q <- quantile(
    data$value,
    probs = c(0.995),
    names = F,
    na.rm = T
  )
  data %>%
    mutate(extra = ifelse(value > q, "1", "0")) %>%
    ggplot(aes(x = value , y = chla, fill = extra)) +
    geom_point(alpha = 0.8,
               size = 1.5,
               pch = 21) +
    scale_y_continuous(limits = c(0, 60)) +
    scale_fill_manual(na.translate = FALSE ,
                      values = c("#984ea3", "#66a61e")) +
    labs(y = expression(paste("Chlorophyll a (", mu, "g L" ^ -1, ")"))) +
    theme(axis.title.y = element_text(size = 8)) +
    guides (fill = "none")
}

# Application of the "figa1_func" function to each variable

data_chla_all <- chla_allvalues %>%
  mutate(plots = map(data, figa1_func))

# Extraction of each plot and labelling: 
chla_plots <- data_chla_all %>%  ungroup() %>%
  dplyr::select(plots)

figa1_alk <- chla_plots[[1]][[1]] +
  labs(x = expression(paste("Alkalinity (mg L" ^ -1, ")")))

figa1_ec <- chla_plots[[1]][[2]] +
  labs(x = expression(paste("EC"[w] , " (", mu, "S cm" ^ -1, ")")))

figa1_tp <- chla_plots[[1]][[3]] +
  labs(x = expression(paste("Total Phosphorus (", mu, "g L" ^ -1, ")")))

figa1_sst <- chla_plots[[1]][[4]] +
  labs(x = expression(paste("Total Suspended Solids ( mg L" ^ -1, ")")))

figa1_ph <- chla_plots[[1]][[5]] +
  labs(x = "pH")

figa1_ta <- chla_plots[[1]][[6]] +
  labs(x = expression(paste("T (", degree, "C)")))

# Creation and saving of Figure 1A:
fig_A1 <- wrap_plots(figa1_alk,
                     figa1_ec,
                     figa1_tp,
                     figa1_sst,
                     figa1_ph,
                     figa1_ta,
                     ncol = 2) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(
          size = 12,
          hjust = 0,
          vjust = 0
        ))

# ggsave(
#   fig_A1,
#   filename = "FigureA1.tiff",
#   dpi = "print",
#   height = 5.94  ,
#   width = 8.35
# )

# For replication purposes all values exceeding the 99.5 percentile was replaced with NA values. 
# For the same reason Negro and Uruguay were analyzed altogether, while Cuareim alone

# Definition of function that replaces data above the 99.5 percentile with NaN:
q99.5_remove <- function(x) {
  q = quantile(x, probs = c(0.995), na.rm = T)
  x_c = ifelse(x <= q , x , NaN)
}

# Replacement for Negro and Uruguay:
data_cut_NU <- data_oan %>%
  filter (river %in% c("Negro", "Uruguay")) %>%
  mutate(across(where(is.numeric), q99.5_remove))

# Replacement for Cuareim:
data_cut_C <- data_oan %>%
  filter (river == "Cuareim") %>%
  mutate(across(where(is.numeric), q99.5_remove))

bc_data_limit <- bind_rows(data_cut_NU, data_cut_C)

# Replication of figure 3 (bi-plots)

# Maximum values for X axis according to BC2021 (Figure 3)
plot_x_limits <- tribble(
  ~ name,
  ~ lmin,
  ~ lmax,
  "alk",
  0,
  150,
  "cond",
  0,
  300,
  "tot_phos",
  0,
  1000,
  "pH",
  5,
  10, # This value differ from the BC2020. They report "8" and not "10".
  "tss",
  0,
  600,
  "temp",
  0 ,
  40
)

# Creation of dataframe to make plots for each variable (excluding Rio Cuareim)

data_fig3 <- bc_data_limit %>%
  filter (river != "Cuareim") %>%
  dplyr::select(!all_of(id_vars)) %>%
  pivot_longer (cols = !chla) %>%
  group_by (name) %>%
  nest()

## Addition of limits for x-axis
data_fig3 <- data_fig3 %>%
  left_join(plot_x_limits, by = "name")

## Definition of function to plot variables
fig3_function_plot <- function (data, lmin, lmax, xlab) {
  ggplot(data, aes(x = value , y = chla)) +
    geom_point(alpha = 0.8, size = rel(0.5)) +
    scale_x_continuous(limits = c(xmin = lmin , xmax = lmax)) +
    scale_y_continuous(limits = c(0, 45),
                       breaks = seq(from = 0, to = 48, by = 5)) +
    labs(x = xlab, y = expression(paste("Chlorophyll a (", mu, "g L" ^ -1, ")"))) +
    theme_classic() +
    theme(axis.title = element_text(size = rel(0.7)),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
}

## Creation of plot for each variable using the plot function defined:

data_fig3 <- data_fig3 %>%
  mutate (plot = pmap(list(data, lmin, lmax, name), fig3_function_plot))

plots3 <- data_fig3 %>%  ungroup() %>%
  dplyr::select(plot)

## Extraction of each plot to label variables accordingly

fig3_alk <- plots3[[1]][[1]] +
  labs(x = expression(paste("Alkalinity (mg L" ^ -1, ")"))) +
  coord_fixed(ratio = 3)

fig3_ec <- plots3[[1]][[2]] +
  labs(x = expression(paste("EC"[w] , " (", mu, "S cm" ^ -1, ")"))) +
  coord_fixed(ratio = 6)

fig3_tp <- plots3[[1]][[3]] +
  labs(x = expression(paste("Total Phosphorus (", mu, "g L" ^ -1, ")"))) +
  coord_fixed(ratio = 20)

fig3_sst <- plots3[[1]][[4]] +
  labs(x = expression(paste("Total Suspended Solids ( mg L" ^ -1, ")"))) +
  coord_fixed(ratio = 12)

fig3_ph <- plots3[[1]][[5]] +
  labs(x = "pH") +
  coord_fixed(ratio = 0.1)

fig3_ta <- plots3[[1]][[6]] +
  labs(x = expression(paste("T (", degree, "C)"))) +
  coord_fixed(ratio = 0.8)

# Loading of all panel figures copied from BC2021
library(png) # Upload png images scanned from BC2021
library(grid) # Convert PNG to Grob
library(ggplotify) # Convert Grob to GGPLOT
library(cowplot) # Paste plots together

f3a <-"https://raw.githubusercontent.com/NAlcan/Reply_BC2021/bca34bc1ac7399cd2197d2f986ab8f1c3a1a5a8d/2.Datos/Fig3BC/3a.png"
download.file(f3a, "3a.png",mode="wb")
f3a <- readPNG("3a.png")
fig3a <- as.ggplot(grid::rasterGrob(f3a, interpolate = TRUE)) +
  theme(plot.margin = unit(c(0,-1, 0,-1), "cm"))

f3b <-"https://raw.githubusercontent.com/NAlcan/Reply_BC2021/bca34bc1ac7399cd2197d2f986ab8f1c3a1a5a8d/2.Datos/Fig3BC/3b.png"
download.file(f3b, "3b.png",mode="wb")
f3b <- readPNG("3b.png")
fig3b <- as.ggplot(grid::rasterGrob(f3b, interpolate = TRUE)) +
  theme(plot.margin = unit(c(0,-1, 0,-1), "cm"))

f3c <-"https://raw.githubusercontent.com/NAlcan/Reply_BC2021/bca34bc1ac7399cd2197d2f986ab8f1c3a1a5a8d/2.Datos/Fig3BC/3c.png"
download.file(f3c, "3c.png",mode="wb")
f3c <- readPNG("3c.png")
fig3c <- as.ggplot(grid::rasterGrob(f3c, interpolate = TRUE)) +
  theme(plot.margin = unit(c(0,-1, 0,-1), "cm"))

f3d <-"https://raw.githubusercontent.com/NAlcan/Reply_BC2021/bca34bc1ac7399cd2197d2f986ab8f1c3a1a5a8d/2.Datos/Fig3BC/3d.png"
download.file(f3d, "3d.png",mode="wb")
f3d <- readPNG("3d.png")
fig3d <- as.ggplot(grid::rasterGrob(f3d, interpolate = TRUE)) +
  theme(plot.margin = unit(c(0,-1, 0,-1), "cm"))

f3e <-"https://raw.githubusercontent.com/NAlcan/Reply_BC2021/bca34bc1ac7399cd2197d2f986ab8f1c3a1a5a8d/2.Datos/Fig3BC/3e.png"
download.file(f3e, "3e.png",mode="wb")
f3e <- readPNG("3e.png")
fig3e <- as.ggplot(grid::rasterGrob(f3e, interpolate = TRUE)) +
  theme(plot.margin = unit(c(0,-1, 0,-1), "cm"))

f3f <-"https://raw.githubusercontent.com/NAlcan/Reply_BC2021/bca34bc1ac7399cd2197d2f986ab8f1c3a1a5a8d/2.Datos/Fig3BC/3f.png"
download.file(f3f, "3f.png",mode="wb")
f3f <- readPNG("3f.png")
fig3f <- as.ggplot(grid::rasterGrob(f3f, interpolate = TRUE)) +
  theme(plot.margin = unit(c(0,-1,-.2,-1), "cm"))


plots_juntos3 <- plot_grid(
  fig3a,
  fig3_alk ,
  fig3b,
  fig3_ec ,
  fig3c,
  fig3_tp ,
  fig3d,
  fig3_ph ,
  fig3e,
  fig3_sst ,
  fig3f,
  fig3_ta ,
  labels = c("", "a'",
             "", "b'",
             "", "c'",
             "", "d'",
             "", "e'",
             "", "f'"),
  label_x = 0.8,
  label_fontface = "plain",
  label_fontfamily = "times",
  ncol = 2
)



# Save figure

# tiff(
#   filename = "Figure3.tiff",
#   width = 600, height = 580)
# 
# plots_juntos3
# 
# dev.off()

#Part 2: Analysis of river comparisons

# 2.1 Comparison of environmental variables between Uruguay and Negro River ---------------------------------------------------------------
# Figure 4 in the paper


diff_rivers_data <- bc_data_limit %>%
  mutate(logChla = log10(chla)) %>%
  pivot_longer (!all_of(id_vars)) %>%
  mutate (river_label = fct_recode(river,
                                   UR = "Uruguay",
                                   NR = "Negro",
                                   CR = "Cuareim")) %>%
  group_by(name) %>%
  nest()

#Definition of function to plot the data
fig4_function <- function(data) {
  p1 <- ggplot(data, aes(y = value, x = river_label)) +
    geom_violin(aes(fill = river)) +
    scale_fill_manual(na.translate = FALSE ,
                      values = c("#d95f02", "#1b9e77", "#984ea3")) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = rel(1.2)),
      axis.text.y = element_text(size = rel(0.7))
    ) +
    labs(x = NULL) +
    facet_grid(~ fct_relevel(data_model, "Train", "Test"),
               scales = "free_x",
               space = "free_x")
  
}

## Application of the plot function defined
fig4_rivers <- diff_rivers_data %>%
  mutate(plots = map(data, fig4_function))

## Extraction of each plot to label variables accordingly
plots4 <- fig4_rivers %>%  ungroup() %>%
  dplyr::select(plots)

fig4_chla <- plots4[[1]][[1]] +
  labs(y =  expression(paste("Chl-a (", mu, "g L" ^ -1, ")"))) +
  theme(axis.text.x = element_blank())


fig4_alk <- plots4[[1]][[2]] +
  labs(y = expression(paste("Alkalinity (mg L" ^ -1, ")")))+
  theme(axis.text.x = element_blank())


fig4_ec <- plots4[[1]][[3]] +
  labs(y = expression(paste("EC"[w] , " (", mu, "S cm" ^ -1, ")")))+
  theme(axis.text.x = element_blank())


fig4_tp <- plots4[[1]][[4]] +
  labs(y = expression(paste("Total phosphorus (", mu, "g L" ^ -1, ")")))+
  theme(axis.text.x = element_blank())


fig4_sst <- plots4[[1]][[5]] +
  labs(y = expression(paste("TSS ( mg L" ^ -1, ")"))) +
  theme(axis.title.y =  element_text(size = 10))+
  theme(axis.text.x = element_blank())


fig4_ph <- plots4[[1]][[6]] +
  labs(y = "pH") 


fig4_ta <- plots4[[1]][[7]] +
  labs(y = expression(paste("T (", degree, "C)")))

fig4_logchla <- plots4[[1]][[8]] +
  labs(y =  expression(paste("log"[10], "(Chl-a)"))) +
  theme(axis.text.x = element_blank())



figure4 <- wrap_plots(fig4_chla,
                      fig4_logchla,
                      fig4_alk,
                      fig4_ec,
                      fig4_tp,
                      fig4_sst,
                      fig4_ph,
                      fig4_ta,
                      ncol = 2)

# Save Figure
# ggsave(
#   figure4,
#   filename = "figure4_rivercomp.tiff",
#   dpi = "print",
#   height = 8  ,
#   width = 8
# )


# 2.2 Generalized Least Squares for river comparison ---------------------------------------------------------
## This GLS function allows to compare differences between mean and variances

library(nlme)
gls.var.test <- function(data) {
  x = data$x
  group = data$group
  if (is.null(group)) {
    data.gls <-
      x
    colnames(data.gls) <-
      c("x", "group")
  } else
    data.gls <- data.frame(x, group)
  
  if (any(!is.finite(data.gls[, 1]))) {
    data.gls <-
      data.gls[which(is.finite(data.gls[, 1])), ]
    warning("there were NaNs in the original x data")
  } #NANs are removed
  if (any(!is.finite(data.gls[, 2]))) {
    data.gls <-
      data.gls[which(is.finite(data.gls[, 2])), ]
    warning("there were NaNs in the original group data")
  }#NANs are removed
  
  modHeteroVar = gls(
    x ~ group,
    data = data.gls,
    weights = varIdent(form = ~ 1 |
                         group),
    method = "ML"
  ) # Heterogeneous variance
  modHomoVar   = gls(x ~ group, data = data.gls, method = "ML") # Homogeneous Variance
  modEqualMean = gls(
    x ~ 1,
    data = data.gls,
    weights = varIdent(form = ~ 1 |
                         group),
    method = "ML"
  )# Same mean all groups, different variance
  gls_data <-
    data.frame(
      VarTest = anova(modHeteroVar, modHomoVar)$`p-value`[2],
      MeanTest = anova(modEqualMean, modHeteroVar)$`p-value`[2]
    ) # Evaluar el loglikelyhood ratio test. p>0.01
}


gls_rivers <- diff_rivers_data %>%
  unnest(cols = data) %>%
  filter (river != "Cuareim") %>%
  rename(x = value ,
         group = river) %>%
  mutate (group = factor(group)) %>%
  nest() %>%
  mutate(gls = map(data, gls.var.test)) %>%
  unnest(gls) %>%
  mutate(
    VarTest = ifelse(VarTest <= 0.05, "p<0.05", "n.s"),
    MeanTest = ifelse(MeanTest <= 0.05, "p<0.05", "n.s")
  ) %>%
  dplyr::select(name, VarTest, MeanTest)

gls_rivers
