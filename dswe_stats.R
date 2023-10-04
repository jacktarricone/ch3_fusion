# fsca mask stats
# jack tarricone
# december 6th, 2022

library(terra)
library(ggplot2)

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

setwd("~/ch3_fusion")
list.files()

# load in rasters
dswe_stack_m <-rast("./rasters/uavsar/dswe/dswe_stack_m.tif")

# calculate average cell area
cell_size_v1 <-cellSize(dswe_stack_m, unit = "m")
cell_size_m2 <-as.numeric(global(cell_size_v1, 'max') + global(cell_size_v1, 'min'))/2

# define stat generator funciton

swe_stats <-function(x){
  
  ### swe_gain in dam^3
  gain_dam3 <-(x*cell_size_m2)/1000
  values(gain_dam3)[values(gain_dam3) < 0] = NA
  # plot(gain_dam3)
  swe_gain_dam3 <-as.integer(global(gain_dam3, "sum", na.rm = TRUE))
  
  ### swe_loss
  loss_dam3 <-(x*cell_size_m2)/1000
  values(loss_dam3)[values(loss_dam3) > 0] = NA
  #plot(loss_dam3)
  swe_loss_dam3 <-as.integer(global(loss_dam3, "sum", na.rm = TRUE))
  
  # net
  swe_net_dam3 <- swe_loss_dam3+ swe_gain_dam3
  
  string <-c(swe_gain_dam3,swe_loss_dam3,swe_net_dam3)
  return(string)
}

# test <-0.028673721*1e9
# hmmt <-test*1e-7

# calculate stats
ims_stats <-swe_stats(dswe_stack_m[[1]])
modis_stats <-swe_stats(dswe_stack_m[[2]])
modscag_stats <-swe_stats(dswe_stack_m[[3]])
viirs_stats <-swe_stats(dswe_stack_m[[4]])
flm_stats <-swe_stats(dswe_stack_m[[5]])
landsat_stats <-swe_stats(dswe_stack_m[[6]])

# create df for plotting
swe_change <-c(ims_stats, modscag_stats,modis_stats,viirs_stats,flm_stats,landsat_stats) # bind cols
sensor <-c(rep("IMS",3), rep("MODSCAG",3), rep("MODIS",3), rep("VIIRS",3), rep("FLM",3), rep("Landsat",3)) # create sensor col
stat <-c(rep(c("gain","loss","net"),6)) # create stat col
stats_df <-as.data.frame(cbind(sensor,swe_change,stat)) # bind as df
stats_df$swe_change <-as.numeric(stats_df$swe_change) # convert to numeric
stats_df$sensor <-factor(stats_df$sensor, levels = c('IMS','MODSCAG','MODIS','VIIRS','FLM','Landsat')) # conver to facor for plotting
stats_df

# make group bar plot
ggplot(stats_df, aes(fill=stat, x = sensor, y=swe_change)) + 
  geom_bar(position="dodge", stat="identity", color = "black", width = .5)+
  geom_hline(yintercept = 0) +
  # geom_text(aes(label=swe_change), position=position_dodge(width=0.5), vjust=1.5)+
  scale_y_continuous(breaks = seq(-20000,5000,5000),limits = c(-20000,5000))+
  scale_fill_manual(values=c('darkblue','darkred','grey90'),name="")+
  ylab(expression(Delta~SWE~(dam^3)))+ 
  xlab("fSCA Product") +
  theme_classic(12) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "center", 
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank())

# saves
ggsave(file = "/Users/jacktarricone/ch3_fusion/plots/dswe_stats_dam3_v1.png",
       width = 6,
       height = 3,
       dpi = 300)

# make table for poster
data <-rbind(ims_stats,modscag_stats,modis_stats,viirs_stats,flm_stats,landsat_stats)
data2 <-round(data, digits = 0)
write.csv(data2, "/Users/jacktarricone/ch3_fusion/csvs/dswe_stats_table_dam_v1.csv")

system("open /Users/jacktarricone/ch3_fusion/csvs/dswe_stats_table_dam_v1.csv")
