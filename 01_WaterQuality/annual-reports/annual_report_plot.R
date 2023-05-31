setwd("C:/Users/ahtran/OneDrive - California Department of Water Resources/Documents - DWR Continuous Environmental Monitoring Program/Annual Reports/2022")

library(tidyverse)
library(lubridate)
library(ggplot2)

#Import data
data_avg_all <- read.csv("./data/data_avg_all.csv")

####Create plots ####
year <- year(Sys.Date())-1
startdate = paste0(year,"-01-01")
enddate = paste0(year,"-12-31")

#set theme
my_theme <-  theme(
  title = element_text(color = "black", size = 12), plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(color = "black", size = 12, hjust = .5, vjust = .5),
  axis.text.y = element_text(color = "black", size = 12, hjust = 1),  
  axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
  axis.line = element_line(colour = "black"),
  legend.position = "top",
  legend.title = element_blank(),
  legend.text = element_text(color = "black", size = 12),
  panel.grid.minor = element_blank(), 
  panel.background = element_blank()) 

theme_set(my_theme)

plot <- list()
######Central Delta ----
CD <- list()
CD$SpC <- data_avg_all %>% filter(region == "Central Delta", par == "SpC", !endsWith(site_code, "bottom"))
CD$temp <- data_avg_all %>% filter(region == "Central Delta", par == "WaterTemperature", !endsWith(site_code, "bottom"))
CD$turb <- data_avg_all %>% filter(region == "Central Delta", par == "Turbidity")
CD$ph <- data_avg_all %>% filter(region == "Central Delta", par == "pH")
CD$fl <- data_avg_all %>% filter(region == "Central Delta", par == "Fluorescence")
CD$do <- data_avg_all %>% filter(region == "Central Delta", par == "DissolvedOxygen")

plot$CD_SpC_plot <- ggplot(CD$SpC, aes(x = time, y = value, color = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Specific Conductivity Central Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Specific Conductivity (µS/cm)", limits = c(0,3000),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#4CF075", "#4B31F5", "#E68656")) 

plot$CD_temp_plot <- ggplot(CD$temp, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Water Temperature Central Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Water Temperature (°C)", limits = c(0,30),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#4CF075", "#4B31F5", "#E68656"))

plot$CD_turb_plot <- ggplot(CD$turb, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Turbidity Central Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Turbidity (FNU)", limits = c(0,40),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#4CF075", "#4B31F5", "#E68656"))

plot$CD_ph_plot <- ggplot(CD$ph, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily pH Central Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("pH Units", limits = c(0,12),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#4CF075", "#4B31F5", "#E68656"))

plot$CD_fl_plot <- ggplot(CD$fl, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Fluorescence Central Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Fluorescence (µg/L)", limits = c(0,30),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#4CF075", "#4B31F5", "#E68656"))

plot$CD_do_plot <- ggplot(CD$do, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Dissolved Oxygen Central Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Dissolved Oxygen (mg/L)", limits = c(0,16),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#4CF075", "#4B31F5", "#E68656"))

plot$CD_SpC_plot
plot$CD_temp_plot
plot$CD_turb_plot
plot$CD_ph_plot
plot$CD_fl_plot
plot$CD_do_plot

ggsave("CD_SpC.png",plot$CD_SpC_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CD_temp.png",plot$CD_temp_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CD_turb.png",plot$CD_turb_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CD_ph.png",plot$CD_ph_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CD_fl.png",plot$CD_fl_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CD_do.png",plot$CD_do_plot,"png","./plots", width = 6, height = 4,units = "in")


##### Northern Interior Delta ----------------
ND <- list()
ND$SpC <- data_avg_all %>% filter(region == "Northern Interior Delta", par == "SpC", !endsWith(site_code, "bottom"))
ND$temp <- data_avg_all %>% filter(region == "Northern Interior Delta", par == "WaterTemperature", !endsWith(site_code, "bottom"))
ND$turb <- data_avg_all %>% filter(region == "Northern Interior Delta", par == "Turbidity")
ND$ph <- data_avg_all %>% filter(region == "Northern Interior Delta", par == "pH")
ND$fl <- data_avg_all %>% filter(region == "Northern Interior Delta", par == "Fluorescence")
ND$do <- data_avg_all %>% filter(region == "Northern Interior Delta", par == "DissolvedOxygen")

plot$ND_SpC_plot <- ggplot(ND$SpC, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Specific Conductivity Northern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Specific Conductivity (µS/cm)", limits = c(0,1600),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#33E0A7", "#FAC33D", "#E68656"))

plot$ND_temp_plot <- ggplot(ND$temp, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Water Temperature Northern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Water Temperature (°C)", limits = c(0,30),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#33E0A7", "#FAC33D", "#E68656"))

plot$ND_turb_plot <- ggplot(ND$turb, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  ggtitle(paste("Average Daily Turbidity Northern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Turbidity (FNU)", limits = c(0,80),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#33E0A7", "#FAC33D", "#E68656"))

plot$ND_ph_plot <- ggplot(ND$ph, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily pH Northern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("pH Units", limits = c(0,10),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#33E0A7", "#FAC33D", "#E68656"))

plot$ND_fl_plot <- ggplot(ND$fl, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Fluorescence Northern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Fluorescence (µg/L)", limits = c(0,10),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#33E0A7", "#FAC33D", "#E68656"))

plot$ND_do_plot <- ggplot(ND$do, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Dissolved Oxygen Northern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Dissolved Oxygen (mg/L)", limits = c(0,14), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#33E0A7", "#FAC33D", "#E68656"))

plot$ND_SpC_plot
plot$ND_temp_plot
plot$ND_turb_plot
plot$ND_ph_plot
plot$ND_fl_plot
plot$ND_do_plot

ggsave("ND_SpC.png",plot$ND_SpC_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("ND_temp.png",plot$ND_temp_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("ND_turb.png",plot$ND_turb_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("ND_ph.png",plot$ND_ph_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("ND_fl.png",plot$ND_fl_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("ND_do.png",plot$ND_do_plot,"png","./plots", width = 6, height = 4,units = "in")

##### Southern Interior Delta ----------------
SI <- list()
SI$SpC <- data_avg_all %>% filter(region == "Southern Interior Delta", par == "SpC", !endsWith(site_code, "bottom"))
SI$temp <- data_avg_all %>% filter(region == "Southern Interior Delta", par == "WaterTemperature", !endsWith(site_code, "bottom"))
SI$turb <- data_avg_all %>% filter(region == "Southern Interior Delta", par == "Turbidity")
SI$ph <- data_avg_all %>% filter(region == "Southern Interior Delta", par == "pH")
SI$fl <- data_avg_all %>% filter(region == "Southern Interior Delta", par == "Fluorescence")
SI$do <- data_avg_all %>% filter(region == "Southern Interior Delta", par == "DissolvedOxygen")

plot$SI_SpC_plot <- ggplot(SI$SpC, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Specific Conductivity Southern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Specific Conductivity (µS/cm)", limits = c(0,1000),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#54DE74", "#E84A8F", "#61CEFA"))

plot$SI_temp_plot <- ggplot(SI$temp, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Water Temperature Southern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Water Temperature (°C)", limits = c(0,30),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#54DE74", "#E84A8F", "#61CEFA"))

plot$SI_turb_plot <-ggplot(SI$turb, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Turbidity Southern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Turbidity (FNU)", limits = c(0,50),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#54DE74", "#E84A8F", "#61CEFA"))

plot$SI_ph_plot <- ggplot(SI$ph, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily pH Southern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("pH Units", limits = c(0,10),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#54DE74", "#E84A8F", "#61CEFA"))

plot$SI_fl_plot <- ggplot(SI$fl, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Fluorescence Southern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Fluorescence (µg/L)", limits = c(0,20),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#54DE74", "#E84A8F", "#61CEFA"))

plot$SI_do_plot <- ggplot(SI$do, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Dissolved Oxygen Southern Interior Delta", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Dissolved Oxygen (mg/L)", limits = c(0,16),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#54DE74", "#E84A8F", "#61CEFA"))

plot$SI_SpC_plot
plot$SI_temp_plot
plot$SI_turb_plot
plot$SI_ph_plot
plot$SI_fl_plot
plot$SI_do_plot

ggsave("SI_SpC.png",plot$SI_SpC_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("SI_temp.png",plot$SI_temp_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("SI_turb.png",plot$SI_turb_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("SI_ph.png",plot$SI_ph_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("SI_fl.png",plot$SI_fl_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("SI_do.png",plot$SI_do_plot,"png","./plots", width = 6, height = 4,units = "in")

##### Confluence ----------------
CON <- list()
CON$SpC <- data_avg_all %>% filter(region == "Confluence", par == "SpC", !endsWith(site_code, "bottom"))
CON$temp <- data_avg_all %>% filter(region == "Confluence", par == "WaterTemperature", !endsWith(site_code, "bottom"))
CON$turb <- data_avg_all %>% filter(region == "Confluence", par == "Turbidity")
CON$ph <- data_avg_all %>% filter(region == "Confluence", par == "pH")
CON$fl <- data_avg_all %>% filter(region == "Confluence", par == "Fluorescence")
CON$do <- data_avg_all %>% filter(region == "Confluence", par == "DissolvedOxygen")

plot$CON_SpC_plot <- ggplot(CON$SpC, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Specific Conductivity Confluence", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Specific Conductivity (µS/cm)", limits = c(0,20000),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#9866FA", "#FC956F", "#C7E04A"))

plot$CON_temp_plot <- ggplot(CON$temp, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Water Temperature Confluence", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Water Temperature (°C)", limits = c(0,30),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#9866FA", "#FC956F", "#C7E04A"))

plot$CON_turb_plot <- ggplot(CON$turb, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Turbidity Confluence", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Turbidity (FNU)", limits = c(0,40),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#9866FA", "#FC956F", "#C7E04A"))

plot$CON_ph_plot <- ggplot(CON$ph, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily pH Confluence", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("pH Units", limits = c(0,10),  n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#9866FA", "#FC956F", "#C7E04A"))

plot$CON_fl_plot <- ggplot(CON$fl, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Fluorescence Confluence", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Fluorescence (µg/L)", limits = c(0,12), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#9866FA", "#FC956F", "#C7E04A"))

plot$CON_do_plot <- ggplot(CON$do, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Dissolved Oxygen Confluence", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Dissolved Oxygen (mg/L)", limits = c(0,14),  n.breaks = 10, label = comma, expand = c(0,0)) +
  scale_color_manual(values = c("#9866FA", "#FC956F", "#C7E04A") )

plot$CON_SpC_plot
plot$CON_temp_plot
plot$CON_turb_plot
plot$CON_ph_plot
plot$CON_fl_plot
plot$CON_do_plot

ggsave("CON_SpC.png",plot$CON_SpC_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CON_temp.png",plot$CON_temp_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CON_turb.png",plot$CON_turb_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CON_ph.png",plot$CON_ph_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CON_fl.png",plot$CON_fl_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("CON_do.png",plot$CON_do_plot,"png","./plots", width = 6, height = 4,units = "in")

##### Grizzly and Suisun Bay ----------------
GS <- list()
GS$SpC <- data_avg_all %>% filter(region == "Grizzly and Suisun Bays", par == "SpC", !endsWith(site_code, "bottom"))
GS$temp <- data_avg_all %>% filter(region == "Grizzly and Suisun Bays", par == "WaterTemperature", !endsWith(site_code, "bottom"))
GS$turb <- data_avg_all %>% filter(region == "Grizzly and Suisun Bays", par == "Turbidity")
GS$ph <- data_avg_all %>% filter(region == "Grizzly and Suisun Bays", par == "pH")
GS$fl <- data_avg_all %>% filter(region == "Grizzly and Suisun Bays", par == "Fluorescence")
GS$do <- data_avg_all %>% filter(region == "Grizzly and Suisun Bays", par == "DissolvedOxygen")

plot$GS_SpC_plot <- ggplot(GS$SpC, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Specific Conductivity Grizzly and Suisun Bay", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Specific Conductivity (µS/cm)", limits = c(0,35000), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA"))

plot$GS_temp_plot <- ggplot(GS$temp, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Water Temperature Grizzly and Suisun Bay", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Water Temperature (°C)", limits = c(0,30), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA"))

plot$GS_turb_plot <- ggplot(GS$turb, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Turbidity Grizzly and Suisun Bay", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Turbidity (FNU)", limits = c(0,120),   n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA"))

plot$GS_ph_plot <- ggplot(GS$ph, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily pH Grizzly and Suisun Bay", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("pH Units", limits = c(0,10), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA"))

plot$GS_fl_plot <- ggplot(GS$fl, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Fluorescence Grizzly and Suisun Bay", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Fluorescence (µg/L)", limits = c(0,20), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA"))

plot$GS_do_plot <- ggplot(GS$do, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Dissolved Oxygen Grizzly and Suisun Bay", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Dissolved Oxygen (mg/L)", limits = c(0,14), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA"))

plot$GS_SpC_plot
plot$GS_temp_plot
plot$GS_turb_plot
plot$GS_ph_plot
plot$GS_fl_plot
plot$GS_do_plot

ggsave("GS_SpC.png",plot$GS_SpC_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("GS_temp.png",plot$GS_temp_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("GS_turb.png",plot$GS_turb_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("GS_ph.png",plot$GS_ph_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("GS_fl.png",plot$GS_fl_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("GS_do.png",GS_do_plot,"png","./plots", width = 6, height = 4,units = "in")


##Bottom sonde data####
BOT <- list()
BOT$MAL_SpC_bottom <- files$MAL_SpC_bottom %>% mutate(site_code = replace(site_code, site_code == 'D10A_bottom', 'D10A Bottom'))
BOT$MRZ_SpC_bottom <- files$MRZ_SpC_bottom %>% mutate(site_code = replace(site_code, site_code == 'D6A_bottom', 'D6A Bottom'))
BOT$ANH_SpC_bottom <- files$ANH_SpC_bottom %>% mutate(site_code = replace(site_code, site_code == 'D12A_bottom', 'D12A Bottom'))
BOT$MAL_WaterTemperature_bottom <- files$MAL_WaterTemperature_bottom %>% mutate(site_code = replace(site_code, site_code == 'D10A_bottom', 'D10A Bottom'))
BOT$MRZ_WaterTemperature_bottom <- files$MRZ_WaterTemperature_bottom %>% mutate(site_code = replace(site_code, site_code == 'D6A_bottom', 'D6A Bottom'))
BOT$ANH_WaterTemperature_bottom <- files$ANH_WaterTemperature_bottom %>% mutate(site_code = replace(site_code, site_code == 'D12A_bottom', 'D12A Bottom'))

BOT$SpC_both <- bind_rows(files$MAL_SpC,files$MRZ_SpC,files$ANH_SpC,files$MAL_SpC_bottom,files$MRZ_SpC_bottom,files$ANH_SpC_bottom)
BOT$temp_both <- bind_rows(files$MAL_WaterTemperature,files$MRZ_WaterTemperature,files$ANH_WaterTemperature,files$MAL_WaterTemperature_bottom,files$MRZ_WaterTemperature_bottom,files$ANH_WaterTemperature_bottom)

plot$BOT_SpC_plot <- ggplot(BOT$SpC_both, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Specific Conductivity Surface-Bottom", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Specific Conductivity (µS/cm)", limits = c(0,35000), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA","#5F4D11","#e6a017"))

plot$BOT_temp_plot <- ggplot(BOT$temp_both, aes(x = as.Date(time), y = value, color = site_code, linetype = site_code)) +
  geom_line(linewidth = 1) +
  ggtitle(paste("Average Daily Water Temperature Surface-Bottom", year)) +
  scale_x_date("",breaks="month", labels=date_format("%b"), limits = as.Date(c(startdate,enddate)), expand = c(0,0)) +
  scale_y_continuous("Water Temperature (°C)", limits = c(0,30), n.breaks = 10, label=comma, expand = c(0,0)) +
  scale_color_manual(values = c("#1DE600", "#FF5BED", "#F04435","#9653FA","#5F4D11","#e6a017"))


plot$BOT_SpC_plot
plot$BOT_temp_plot
ggsave("GS_SpC_bottom.png",plot$BOT_SpC_plot,"png","./plots", width = 6, height = 4,units = "in")
ggsave("GS_temp_bottom.png",plot$BOT_temp_plot,"png","./plots", width = 6, height = 4,units = "in")

###RRI DO PLOT####
RRI_DO<- files$RRI_DissolvedOxygen %>% mutate(month = month(time, label = TRUE, abbr = TRUE))

plot$RRI_DO_plot <- ggplot(RRI_DO, aes(x = month,y = value)) +
  geom_boxplot() +
  scale_y_continuous("Dissolved Oxygen (mg/L)", limits = c(0,12), n.breaks = 10, label=comma, expand = c(0,0)) +
  theme(legend.position="top", legend.title = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black")) +
  geom_segment(aes(x = "Jan", y = 5, xend = "Sep", yend = 5), linewidth = 1, color = "blue") +
  geom_segment(aes(x = "Sep", y = 6, xend = "Nov", yend = 6),linewidth = 1, color = "blue") +
  geom_segment(aes(x = "Nov", y = 5, xend = "Dec", yend = 5),linewidth = 1, color = "blue") +
  annotate("text", x= 6, y = 4.7, label = "Minimum DO Standard") 

plot$RRI_DO_plot
ggsave("RRI_DO.png",plot$RRI_DO_plot,"png","./plots", height = 4, width = 6, units = "in")

###Generate min-max####
summary_list <- list()

for (region in c("CD", "CON", "ND", "SI", "GS")) {
  list <- get(region) 
  # Combines all dataframes
  df <- bind_rows(list, .id = "par")
  
  # Adds a new column with the region name
  df <- df %>%
    mutate(region = region)
  
  df <- df %>%
    group_by(region, par) %>%
    summarize(
      min_value = round(min(value, na.rm = TRUE), 2),
      min_site = site_code[which.min(value)],
      min_month = month[which.min(value)],
      max_value = round(max(value, na.rm = TRUE), 2),
      max_site = site_code[which.max(value)],
      max_month = month[which.max(value)],
      average = round(mean(value, na.rm = TRUE), 2)
    )
  
  # Appends summary df to list
  summary_list[[length(summary_list) + 1]] <- df
}

# Combines into one df
df_minmax <- do.call(rbind, summary_list)
rm(summary_list,list,df)

write.csv(df_minmax, "minmax_table.csv", row.names = FALSE)
