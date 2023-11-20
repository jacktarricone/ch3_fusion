# dswe variabilty analysis
# june 13th, 2023
# jack tarricone

library(terra)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(dgof)

theme_classic <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # show axes
      # axis.line      = element_line(colour = "black", linewidth = rel(1)),
      
      # match legend key to panel.background
      legend.key       = element_blank(),
      
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      
      complete = TRUE
    )
}

theme_set(theme_classic(16))

setwd("~/ch3_fusion")

# load in 80 m insar dswe products
modscag_cm <-rast("./rasters/uavsar/dswe/modscag_dswe_v2.tif")
modis_cm <-rast("./rasters/uavsar/dswe/modis_dswe_v2.tif")
viirs_cm <-rast("./rasters/uavsar/dswe/viirs_dswe_v2.tif")
landsat_cm <-rast("./rasters/uavsar/dswe/landsat_dswe_v2.tif")
flm_cm <-rast("./rasters/uavsar/dswe/flm_dswe_v2.tif")
ims_cm <-rast("./rasters/uavsar/dswe/ims_dswe_v2.tif")

# stack
stack_cm <-c(ims_cm,modis_cm,modscag_cm,viirs_cm,flm_cm,landsat_cm)
names(stack_cm) <-c("ims","modis","modscag","viirs","flm","landsat")
stack_cm
hist(stack_cm, breaks = 100)


# convert to df
df <-as.data.frame(stack_cm)
head(df)

# test plot
hist_fig <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(df, mapping = aes(x=ims, y=stat(count),color = "IMS"), linewidth=1) +
  geom_density(df, mapping = aes(x=landsat, y=stat(count),color = "Landsat"), linewidth=1) +
  geom_density(df, mapping = aes(x=modis, y=stat(count), color = "MODIS fSCA"), linewidth=1) +
  geom_density(df, mapping = aes(x=modscag, y=stat(count),color = "MODSCAG"), linewidth=1) +
  geom_density(df, mapping = aes(x=viirs, y=stat(count), color = "VIIRS fSCA"), linewidth=1) +
  geom_density(df, mapping = aes(x=flm, y=stat(count), color = "FLM"), linewidth=1) +
  scale_colour_manual(name = "Snow cover data",
                      labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
                      values = c("#d95f02","#1b9e77",'#7570b3','#e7298a','#e6ab02','#66a61e'),
                      breaks = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"))+
  scale_x_continuous(limits = c(-8,4), 
                     breaks = seq(-8,4,2), 
                     expand = c(0,0)) + 
  ylab("Count") +
  xlab(expression(Delta~'SWE (cm)'))+
  scale_y_continuous(expand = c(0,0), limits = c(0,50000)) +
  theme(legend.position = c(.21,.72)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

ggsave2("./plots/dswe_hist_v1.pdf",
        hist_fig,
        width = 7,
        height = 5,
        units = "in",
        dpi = 500)

system("open ./plots/dswe_hist_v1.pdf")


# conert to meters
stack_m <-stack_cm/100
# writeRaster(stack_m, "./rasters/uavsar/dswe/dswe_stack_m.tif")

# bring in cc, mask, and resample
cc_v2 <-rast("~/ch3_fusion/rasters/geo_layers/cc_domain.tif")
sierra <-vect("./uavsar_shape_files/sierra_17305_20014-000_20016-005_0014d_s01_L090HH_01.cor.grd .shp")
cc_v1 <-mask(cc_v2, sierra)
cc <-resample(cc_v1, stack_cm, method = 'bilinear')
plot(cc)
cc

# calculate average cell area in cubic meters
cell_size_v1 <-cellSize(stack_m, unit = "m")
cell_size_m2 <-as.numeric(global(cell_size_v1, 'max') + global(cell_size_v1, 'min'))/2

#### calculate SWE dswes in cubic meters
dswe_stack_m <-ifel(stack_m < 0, NA, stack_m)
dswe_df <-(dswe_stack_m*cell_size_m2)
hist(dswe_df[[5]])
dswe_df <-as.data.frame(dswe_df)

ks.test(dswe_df$modis, dswe_df$ims)

# test plot
dswe_hist <-ggplot()+
  # geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(dswe_df, mapping = aes(x=ims, y=stat(count),color = "IMS"), linewidth=1) +
  geom_density(dswe_df, mapping = aes(x=modscag, y=stat(count),color = "MODSCAG"), linewidth=1) +
  geom_density(dswe_df, mapping = aes(x=modis, y=stat(count), color = "MODIS fSCA"), linewidth=1) +
  geom_density(dswe_df, mapping = aes(x=viirs, y=stat(count), color = "VIIRS fSCA"), linewidth=1) +
  geom_density(dswe_df, mapping = aes(x=flm, y=stat(count), color = "FLM"), linewidth=1) +
  geom_density(dswe_df, mapping = aes(x=landsat, y=stat(count),color = "Landsat"), linewidth=1) +
  scale_colour_manual(name = "Snow cover data",
                      labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
                      values = c("#d95f02","#1b9e77",'#7570b3','#e7298a','#e6ab02','#66a61e'),
                      breaks = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"))+
  #scale_fill_manual(name = "Data",
  #                  labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
  #                  values = c("darkorchid4","goldenrod",'darkred','darkgreen','darkblue','cyan'))+
  scale_x_continuous(limits = c(0,250), 
                     breaks = seq(0,250,50), 
                     expand = c(0,2)) + 
  ylab("Count") +
  xlab(expression(Delta~'SWE (m^3)'))+
  scale_y_continuous(expand = c(0,0), limits = c(0,700)) +
  theme(legend.position = c(.75,.72)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

dswe_hist

ggsave2("./plots/dswe_dswe_v1.pdf",
        dswe_hist,
        width = 7,
        height = 5,
        units = "in",
        dpi = 500)

system("open ./plots/dswe_dswe_v1.pdf")




#### calculate SWE losss in cubic meters
loss_stack_m <-ifel(stack_m > 0, NA, stack_m)
loss_stack_m3 <-loss_stack_m*cell_size_m2
hist(loss_stack_m3[[5]])
loss_df <-as.data.frame(loss_stack_m3)


# test plot
loss_hist <-ggplot() +
  geom_density(loss_df, mapping = aes(x=ims, y=stat(count),color = "IMS"), linewidth=1) +
  geom_density(loss_df, mapping = aes(x=modscag, y=stat(count),color = "MODSCAG"), linewidth=1) +
  geom_density(loss_df, mapping = aes(x=modis, y=stat(count), color = "MODIS fSCA"), linewidth=1) +
  geom_density(loss_df, mapping = aes(x=viirs, y=stat(count), color = "VIIRS fSCA"), linewidth=1) +
  geom_density(loss_df, mapping = aes(x=flm, y=stat(count), color = "FLM"), linewidth=1) +
  geom_density(loss_df, mapping = aes(x=landsat, y=stat(count),color = "Landsat"), linewidth=1) +
  scale_colour_manual(name = "Snow cover data",
                      labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
                      values = c("#d95f02","#1b9e77",'#7570b3','#e7298a','#e6ab02','#66a61e'),
                      breaks = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"))+
  scale_x_continuous(limits = c(-500,0), 
                     breaks = seq(-500,0,100), 
                     expand = c(0,2)) + 
  ylab("Count") +
  xlab(expression(Delta~'SWE (m^3)'))+
  scale_y_continuous(expand = c(0,0), limits = c(0,700)) +
  theme(legend.position = c(.30,.72)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

loss_hist

ggsave2("./plots/dswe_loss_v1.pdf",
        loss_hist,
        width = 7,
        height = 5,
        units = "in",
        dpi = 500)

system("open ./plots/dswe_loss_v1.pdf")







# fulls stack
dswe_41x41 <-focal(stack_m, c(41,41), na.rm=TRUE, fun = "sum")
plot(dswe_41x41[[6]]) 
dswe_41x41[[3]]
hist(dswe_41x41[[1]])
head(dswe_41x41_df)

dswe_41x41_df <-as.data.frame(dswe_41x41)
head(dswe_41x41_df)

# writeRaster(dswe_41x41[[6]], "./rasters/dswe_variabilty_analysis/testing/landsat_test.tif")
# writeRaster(dswe_41x41[[3]], "./rasters/dswe_variabilty_analysis/testing/modscag_test.tif")

ecdf_41 <-ggplot()+
  stat_ecdf(dswe_41x41_df, mapping = aes(x=ims, color = "IMS"), linewidth=1) +
  stat_ecdf(dswe_41x41_df, mapping = aes(x=modscag, color = "MODSCAG"), linewidth=1) +
  stat_ecdf(dswe_41x41_df, mapping = aes(x=modis, color = "MODIS fSCA"), linewidth=1) +
  stat_ecdf(dswe_41x41_df, mapping = aes(x=viirs, color = "VIIRS fSCA"), linewidth=1) +
  stat_ecdf(dswe_41x41_df, mapping = aes(x=flm, color = "FLM"), linewidth=1) +
  stat_ecdf(dswe_41x41_df, mapping = aes(x=landsat,color = "Landsat"), linewidth=1) +
  scale_colour_manual(name = "Snow cover data",
                      labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
                      values = c("#d95f02","#1b9e77",'#7570b3','#e7298a','#e6ab02','#66a61e'),
                      breaks = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"))+
  scale_x_continuous(limits = c(-75,50), 
                     breaks = seq(-75,50,25), 
                     expand = c(0,2)) + 
  ylab("Count") +
  xlab(expression(Delta~SWE~(m^3)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme(legend.position = c(.25,.72)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


ecdf_41

ggsave2("./plots/ecdf_41x41_v1.pdf",
        ecdf_41,s
        
        width = 7,
        height = 5,
        units = "in",
        dpi = 500)

system("open ./plots/ecdf_41x41_v1.pdf")


dswe_hist_41 <-ggplot()+
  geom_density(dswe_41x41_df, mapping = aes(x=ims, y=stat(density),color = "IMS"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=modscag, y=stat(density),color = "MODSCAG"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=modis, y=stat(density), color = "MODIS fSCA"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=viirs, y=stat(density), color = "VIIRS fSCA"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=flm, y=stat(density), color = "FLM"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=landsat, y=stat(density),color = "Landsat"), linewidth=1) +
  scale_colour_manual(name = "Snow cover data",
                      labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
                      values = c("#d95f02","#1b9e77",'#7570b3','#e7298a','#e6ab02','#66a61e'),
                      breaks = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"))+
  scale_x_continuous(limits = c(-75,50), 
                     breaks = seq(-75,50,25), 
                     expand = c(0,2)) + 
  ylab("Count") +
  xlab(expression(Delta~SWE~(m^3)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,.08)) +
  theme(legend.position = c(.25,.72)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


dswe_hist_41

ggsave2("./plots/dswe_hist_41x41_v1.pdf",
        dswe_hist_41,
        width = 7,
        height = 5,
        units = "in",
        dpi = 500)

system("open ./plots/dswe_hist_41x41_v1.pdf")


####################
###################
# 55 x 55 moving window sum in cubic meters
# dswe
dswe_mw_41x41 <-focal(dswe_df, c(41,41), na.rm=TRUE, fun = "sum")
dswe_dam3_41x41 <-dswe_mw_41x41/1e3 
dswe_dam3_41x41 
hist(dswe_dam3_41x41[[1]])
head(dswe_41x41_df)

data.table::fwrite(dswe_41x41_df, "./csvs/swe_dswe_dam3_41x41.csv")

# have to take random sample
# p-value is correlated to sample size: high numbers = low p-values
# ross used 100
# repeat test 100ish times in loop
# divide confidence by number of repeat samples
samp <-sample_n(dswe_41x41_df, 150, na.rm = TRUE)
ks.test(samp$modscag, samp$landsat)

samp <-sample_n(loss_41x41_df, 150, na.rm = TRUE)
ks.test(samp$modscag, samp$ims)




# test plot
dswe_hist_41 <-ggplot()+
  geom_density(dswe_41x41_df, mapping = aes(x=ims, y=stat(count),color = "IMS"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=modscag, y=stat(count),color = "MODSCAG"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=modis, y=stat(count), color = "MODIS fSCA"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=viirs, y=stat(count), color = "VIIRS fSCA"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=flm, y=stat(count), color = "FLM"), linewidth=1) +
  geom_density(dswe_41x41_df, mapping = aes(x=landsat, y=stat(count),color = "Landsat"), linewidth=1) +
  scale_colour_manual(name = "Snow cover data",
                      labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
                      values = c("#d95f02","#1b9e77",'#7570b3','#e7298a','#e6ab02','#66a61e'),
                      breaks = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"))+
  scale_x_continuous(limits = c(0,100), 
                     breaks = seq(0,100,25), 
                     expand = c(0,2)) + 
  ylab("Count") +
  xlab(expression(Delta~SWE~(dam^3)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,20000)) +
  theme(legend.position = c(.75,.72)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

dswe_hist_41

ggsave2("./plots/dswe_hist_41x41_v1.pdf",
        dswe_hist,
        width = 7,
        height = 5,
        units = "in",
        dpi = 500)








# writeRaster(dswe_mw_41x41, "./rasters/dswe_variabilty_analysis/dswe_df_41x41_v1.tif")

# loss
loss_mw_41x41 <-focal(loss_stack_m3, c(41,41), na.rm=TRUE, fun = "sum")
plot(loss_mw_41x41[[4]])

loss_mw_41x41 <-focal(loss_df, c(41,41), na.rm=TRUE, fun = "sum")
loss_dam3_41x41 <-loss_mw_41x41/1e3 
loss_dam3_41x41 
hist(loss_dam3_41x41[[1]])
loss_41x41_df <-as.data.frame(loss_dam3_41x41)

# test plot
loss_hist_41 <-ggplot()+
  geom_density(loss_41x41_df, mapping = aes(x=ims, y=stat(count),color = "IMS"), linewidth=1) +
  geom_density(loss_41x41_df, mapping = aes(x=modscag, y=stat(count),color = "MODSCAG"), linewidth=1) +
  geom_density(loss_41x41_df, mapping = aes(x=modis, y=stat(count), color = "MODIS fSCA"), linewidth=1) +
  geom_density(loss_41x41_df, mapping = aes(x=viirs, y=stat(count), color = "VIIRS fSCA"), linewidth=1) +
  geom_density(loss_41x41_df, mapping = aes(x=flm, y=stat(count), color = "FLM"), linewidth=1) +
  geom_density(loss_41x41_df, mapping = aes(x=landsat, y=stat(count),color = "Landsat"), linewidth=1) +
  scale_colour_manual(name = "Snow cover data",
                      labels = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"),
                      values = c("#d95f02","#1b9e77",'#7570b3','#e7298a','#e6ab02','#66a61e'),
                      breaks = c("IMS","MODSCAG","MODIS fSCA", "VIIRS fSCA","FLM","Landsat"))+
  scale_x_continuous(limits = c(-400,0), 
                     breaks = seq(-400,0,100), 
                     expand = c(0,2)) + 
  ylab("Count") +
  xlab(expression(Delta~SWE~SD~(dam^3)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,6000)) +
  theme(legend.position = c(.35,.72)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

loss_hist_41

ggsave2("./plots/loss_hist_41x41_v1.pdf",
        loss_hist_41,
        width = 7,
        height = 5,
        units = "in",
        dpi = 500)





# writeRaster(loss_mw_41x41, "./rasters/dswe_variabilty_analysis/loss_stack_m3_41x41_v1.tif")

# define sd with na remove
sd_na_rm <-function(x){sd(x, na.rm = TRUE)}

# calculate pixelwise standard deviation
# dswe
dswe_sd <-app(dswe_mw_41x41, fun = sd_na_rm)
dswe_sd_dam3 <-dswe_sd/1e3
plot(dswe_sd_dam3)
# writeRaster(dswe_sd_dam3, "./rasters/dswe_variabilty_analysis/dswe_sd_dam3_41x41_v1.tif")

# loss
loss_sd <-app(loss_mw_41x41, fun = sd_na_rm)
loss_sd_dam3 <-loss_sd/1e3
plot(loss_sd_dam3)
# writeRaster(loss_sd_dam3, "./rasters/dswe_variabilty_analysis/loss_sd_dam3_41x41_v1.tif")

## cc focal
cc_mw <-focal(cc, c(41,41), na.rm=TRUE, fun = "mean")
plot(cc_mw)
# writeRaster(cc_mw, "./rasters/dswe_variabilty_analysis/cc_mw_mean_41x41.tif")

######### cc vs sd scatter plot
# mask out low change values
cc_sd <-c(cc_mw, dswe_sd_dam3, loss_sd_dam3)

# convert to df
cc_sd_df <-as.data.frame(cc_sd, xy = TRUE, cells = TRUE)
colnames(cc_sd_df)[4:6] <-c("cc_mean", "dswe_sd_dam3","loss_sd_dam3")
head(cc_sd_df)
# data.table::fwrite(cc_sd_df, "./csvs/cc_sd_df_dam3_41x41.csv")

cc_sd_df <-data.table::fread("./csvs/cc_sd_df_dam3_41x41.csv")

# quick hists
hist(cc_sd_df$cc_mean, breaks = 100)
hist(cc_sd_df$dswe_sd_dam3, breaks = 100)
hist(cc_sd_df$loss_sd_dam3, breaks = 100)
max(cc_sd_df$dswe_sd_dam3, na.rm = TRUE)
max(cc_sd_df$loss_sd_dam3, na.rm = TRUE)

# # filter cc of 0
# df_v2 <-dplyr::filter(cc_sd_df, cc_mean >= 0)
# hist(df_v2$cc_mean, breaks = 100)

# add bins col for box plot
df_v3 <-cc_sd_df %>%
  mutate(bin = cut_width(cc_mean, width = 5, boundary=0))

# cc scale
cc_scale <-colorRampPalette(c("#f7fcf5", "#00441b"))
cc_scale(13)


# starting plot
### dswes
dswes_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = dswe_sd_dam3, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .6, varwidth = TRUE, outlier.size = .001, outlier.shape = 4, 
               outlier.colour = "grey80", outlier.alpha  = .01) +
  xlab("CC (%)") + ylab(expression(SWE~SD~(dam^3)))+ 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,25)) +
  scale_fill_discrete(type = cc_scale(10)) +
  theme_classic(10) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))
dswes_plot

# loss
loss_plot <-ggplot(df_v3, mapping = aes(x = cc_mean, y = loss_sd_dam3, fill = as.factor(bin))) +
  geom_boxplot(linewidth = .6, varwidth = TRUE, outlier.size = .001, outlier.shape = 4,
               outlier.colour = "grey80", outlier.alpha  = .01) +
  xlab("CC (%)") + ylab(expression(SWE~SD~(dam^3)))+ 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,65,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_discrete(type = cc_scale(13)) +
  theme_classic(10) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))
loss_plot

# cowplot test
cow <-plot_grid(dswes_plot, loss_plot,
                 labels = c("(a) SWE dswe", "(b) SWE Loss"),
                 nrow = 2, 
                 align = "hv",
                 label_size = 15,
                 vjust =  3,
                 hjust = -.7,
                 rel_heights = c(1/2,1/2))
# test save
# make tighter together
ggsave(cow,
       file = "./plots/swe_sd_bp_dam3_41x41_v1.png",
       width = 8, 
       height = 6,
       dpi = 300)

system("open ./plots/swe_sd_bp_dam3_41x41_v1.png")
        
