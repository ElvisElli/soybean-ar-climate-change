rm(list=ls())

library(ggplot2)
library(sf)
library(viridis)
library(stars)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(rstudioapi)
library(lme4)
library(emmeans)
library(ggridges)
library(cowplot)
library(gghalves)

## external codes ====
source("code/plot_theme.R")

#Original dataset====

simulated0 <- readRDS("intermediate-data/simulated-scenarios-df.rds") %>% 
  as_tibble()



#How many final simulations

# Define treatment columns
treatment_cols <- c("cultivar", "sowing", "scenario", "climate.control", "co2", "rowSpacing")

# Count rows per treatment
simulated0 %>%
  group_by(across(all_of(treatment_cols))) %>%
  summarise(
    n_rows     = n(),
    n_cells    = n_distinct(cellid),
    n_years    = n_distinct(date),
    .groups = "drop"
  ) %>%
  arrange(desc(n_rows))

##p1 - climate change without adaptation====

p1 <- simulated0 %>%
  filter(scenario %in% c("baseline","climate_change")) %>% 
  group_by(x,y,scenario,co2) %>% 
  summarise(Yield_kgha=mean(Yield_kgha,na.rm=T)) %>% 
  mutate(scenario_co2=paste0(scenario,co2),
         scenario_co2=as.factor(scenario_co2),
         scenario_co2=factor(scenario_co2,levels=c('baseline350','climate_change350','climate_change540'),
                         labels=c("Baseline","2°C-increase","2°C-increase with CO2")),
         yield_bin = cut(
           Yield_kgha,
           breaks = c(-Inf, 2500, 3500, 4500, 5500, Inf),
           labels = c("<2500", "2500-3500", "3500-4500", "4500-5500", ">5500"),
           include.lowest = TRUE)) %>% 
  ungroup() %>% 
  select(-scenario,-co2)

##for discussion
summary(p1 %>% filter(scenario_co2 %in% "Baseline"))


#Plot 1A
plot1a <- p1 %>% 
  mutate(scenario_co2 = factor(scenario_co2,
                               labels = c("Baseline",
                                          "2°C-increase",
                                          "2°C-increase\nwith elevated CO2"))) %>%
  ggplot(aes(x=Yield_kgha,y=scenario_co2,colour=scenario_co2)) +
  geom_density_ridges(scale = .9, fill=NA,linewidth=1)+
  temp+
  labs(x = "Yield (kg/ha)", y = element_blank())+
  scale_colour_manual(values = c("black","#bd0026","#1a9850"))+
  theme(legend.position = "none")+
  scale_y_discrete(expand = expansion(mult = c(0, 0.4)))


# Convert to stars object
df <- st_as_stars(p1, dims = c('x', 'y','scenario_co2'), xy = c('x', 'y'), proxy = TRUE)
st_crs(df) <- 'epsg:4326'
df <- st_transform(df, 5070)

# Load and transform Arkansas shapefile
ark <- st_read('raw-data/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
ark <- subset(ark, STUSPS == 'AR')
ark <- st_transform(ark, 5070)

# Create raster grid for biomass
df2 <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)
df <- st_warp(df, df2, no_data_value = NA)

levels(df$yield_bin)
summary(df$yield_bin)

usa_counties <- st_read("raw-data/Elvis-Crop-Data/Arkansas_Counties_4269.shp")

#plot 2B
plot1b <- ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = yield_bin)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  facet_wrap(~scenario_co2,nrow = 1)+
  temp +
  scale_fill_manual(
    values = c(
      "<2500" = "#bd0026",
      "2500-3500" = "#e31a1c",
      "3500-4500" = "#fc8d59",
      "4500-5500" = "#91cf60",
      ">5500"     = "#1a9850",
      "NA"= "grey50"),
    na.value = "grey50",
    na.translate = FALSE,
    name = "Yield (kg/ha)") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))+
  labs(fill="Yield (kg/ha)")
  
plot1 <- plot_grid(plot1a,plot1b,
                   ncol = 1,
                   align = "v",
                   axis = "r",
                   labels = "AUTO",
                   rel_heights = c(0.4,1))

ggsave("plots/p1 - climate change without adaptation.tiff",width=20,height=15,units ="cm",dpi=600,compression="lzw",bg="white")


##p2 - temperature impacts====
weather.char <- simulated0 %>% 
  filter(scenario %in% c("climate_change"),
         co2 %in% c("350")) %>%
  mutate(year=year(date)) %>% 
  ungroup() %>% 
  mutate(Rain.avg=mean(SeasonRain),
         Meat.avg=mean(SeasonMeanT),
         Rain.sd=sd(SeasonRain),
         Meat.sd=sd(SeasonMeanT),
         Normal=ifelse(SeasonRain<Rain.avg+Rain.sd*1,
                       ifelse(SeasonRain>Rain.avg-Rain.sd*1,
                              ifelse(SeasonMeanT<Meat.avg+Meat.sd*1,
                                     ifelse(SeasonMeanT>Meat.avg-Meat.sd*1,"Yes","No"),"No"),"No"),"No"),
         Rain.class=ifelse(SeasonRain>Rain.avg,"Wet","Dry"),
         Meat_class=ifelse(SeasonMeanT>Meat.avg,"Warm","Cool"),
         weather.class=ifelse(Normal=="Yes","Normal",paste0(Meat_class,"/",Rain.class)),
         weather.class=as.factor(weather.class),
         weather.class=factor(weather.class,levels = c("Cool/Wet","Warm/Wet","Normal","Cool/Dry","Warm/Dry"))) %>% 
  select(x,y,year,weather.class,Meat.avg,Rain.avg,Meat.sd,Rain.sd,Yield_kgha,SeasonMeanT,SeasonRain)

weather.char %>% 
  group_by(weather.class) %>% 
  summarise(Yield_kgha = mean(Yield_kgha))

## for discussion, baseline season temperature and rainfall

simulated0 %>% 
  filter(scenario %in% c("baseline"),
         co2 %in% c("350")) %>%
  mutate(year=year(date)) %>% 
  ungroup() %>% 
  summarise(Rain.avg=mean(SeasonRain),
         Meat.avg=mean(SeasonMeanT))

## for discussion, temp range

temp.range <- simulated0 %>% 
  filter(scenario %in% c("baseline"),
         co2 %in% c("350")) %>%
  mutate(year=year(date)) %>% 
  group_by(x,y) %>% 
  summarise(Meat.avg=mean(SeasonMeanT))

## for discussion, SWHC in mm/mm and SOC range

soil.range <- simulated0 %>% 
  filter(scenario %in% c("baseline"),
         co2 %in% c("350")) %>%
  mutate(year=year(date)) %>% 
  group_by(x,y) %>% 
  summarise(swhc_mm_mm=mean((swhc_12in*2.54)/30))

lm.cool.wet <- lm(Yield_kgha ~ SeasonMeanT, data = weather.char %>% filter(weather.class %in% c("Cool/Wet")))
slope.cool.wet <- coef(lm.cool.wet)["SeasonMeanT"]

lm.normal <- lm(Yield_kgha ~ SeasonMeanT, data = weather.char)
slope.overall <- coef(lm.normal)["SeasonMeanT"]
slope.overall/mean(weather.char$Yield_kgha)*100


lm.warm.dry <- lm(Yield_kgha ~ SeasonMeanT, data = weather.char %>% filter(weather.class %in% c("Warm/Dry")))
slope.warm.dry <- coef(lm.warm.dry)["SeasonMeanT"]

#plot 1
min_y <- min(weather.char$y)
mean_y <- mean(weather.char$y)
max_y <- max(weather.char$y)

min(weather.char$SeasonMeanT)
mean(weather.char$SeasonMeanT)
max(weather.char$SeasonMeanT)

min(weather.char$Yield_kgha)
mean(weather.char$Yield_kgha)
max(weather.char$Yield_kgha)

temp.plot <- weather.char %>% 
  filter(weather.class %in% c("Cool/Wet","Normal","Warm/Dry")) %>%
  #slice_sample(prop = 0.1) %>%
  ggplot(aes(x=SeasonMeanT,y=Yield_kgha))+
  geom_point(aes(colour=y),
             size=2,alpha=0.3)+
  temp+
  geom_smooth(method = "lm",se=F,colour="black",size=0.8)+
  geom_smooth(data=weather.char %>% filter(weather.class %in% c("Warm/Dry")),
              aes(group=weather.class),
              #linetype="dashed",
              size=1.2,
              method = "lm",se=F,colour="#008837") +
  labs(x="Season temperature (°C)",y="Seed yield (kg/ha)")+
  scale_y_continuous(limits = c(1000,6300),breaks = c(1000,2000,3000,4000,5000,6000))+
  scale_x_continuous(limits = c(21,31),breaks = c(21,23,25,27,29,31))+
  scale_color_gradientn(
    colors = c("#a50f15", "#fb6a4a", "white"),
    values = scales::rescale(c(min_y, mean_y, max_y)),
    breaks = c(min_y, mean_y, max_y),
    labels=c("33.0","35.0","36.5"),
    limits = c(min_y, max_y),
    guide = guide_colorbar(barwidth = 1.5, 
                           barheight = 5,
                           frame.colour = "black",
                           frame.linewidth = 1),
    name = "Latitude") +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))+
  
  ##overall
  annotate("segment",
           x = 22.5, xend = 22.8,
           y = 5400, yend = 4650,
           arrow = arrow(length = unit(0.25, "cm")),
           color = "black", size = 0.5) +
  annotate("text",
           x = 24.2,
           y = 5500,
           label = paste0("Overall = ", round(slope.overall, 0)," kg/ha/°C"),
           size = 4)+
  
  ##warm/dry
  annotate("segment",
           x = 29, xend = 29,
           y = 1600, yend = 3600,
           arrow = arrow(length = unit(0.25, "cm")),
           color = "black", size = 0.5) +
  annotate("text",
           x = 27.4,
           y = 1500,
           label = paste0("Warm & Dry = ", round(slope.warm.dry, 0)," kg/ha/°C"),
           size = 4,
           colour = "#008837")+ 
  guides(linetype = "none")

ggsave("plots/p2 - temperature impacts.tiff",width=15,height=12,units ="cm",dpi=600,compression="lzw",bg="white")


##p3 -  Climate change with adaptation, summary====

#relative yields
baseline <- simulated0 %>%
  filter(scenario %in% c("baseline")) %>% 
  rename(baseline=Yield_kgha) %>% 
  select(x,y,date,baseline)

p3 <- simulated0 %>%
  filter(!scenario %in% c("baseline")) %>% 
  select(x,y,scenario,co2,date,Yield_kgha) %>% 
  pivot_wider(values_from = Yield_kgha, names_from = scenario) %>%
  left_join(baseline, by=c("x", "y", "date")) %>% 
  rename(time=date) %>%
  mutate(climate_change = (climate_change-baseline)/baseline*100,
         early_sowing  = (early_sowing-baseline)/baseline*100,
         longer_mat = (longer_mat-baseline)/baseline*100,
         early_sowing_longer_mat = (early_sowing_longer_mat-baseline)/baseline*100,
         ) %>% 
  select(-baseline) %>% 
  pivot_longer(values_to = "Yield_kgha",names_to = "scenario",!c(x,y,time,co2,time)) %>%
  mutate(scenario=as.factor(scenario),
         scenario=factor(scenario,levels=c('climate_change','longer_mat','early_sowing','early_sowing_longer_mat'),
                         labels=c("2°C-increase", "Late-Maturing","Early Sowing","Late-Maturing & Early Sowing")),
         scenario_num = as.numeric(factor(scenario, levels = c("2°C-increase", "Late-Maturing", "Early Sowing", "Late-Maturing & Early Sowing"))))

##discussion 3.1
p3 %>%
  filter(scenario %in% c("2°C-increase")) %>%
  group_by(co2) %>%
  summarise(
    Yield_kgha_min      = boxplot.stats(Yield_kgha)$stats[1],
    Yield_kgha_Q1       = boxplot.stats(Yield_kgha)$stats[2],
    Yield_kgha_median   = boxplot.stats(Yield_kgha)$stats[3],
    Yield_kgha_Q3       = boxplot.stats(Yield_kgha)$stats[4],
    Yield_kgha_max      = boxplot.stats(Yield_kgha)$stats[5],
    n                   = boxplot.stats(Yield_kgha)$n,
    outliers            = list(boxplot.stats(Yield_kgha)$out))

##Discussion 3.2
p3 %>%
  group_by(co2,scenario) %>%
  summarise(
    Yield_kgha_min      = boxplot.stats(Yield_kgha)$stats[1],
    Yield_kgha_Q1       = boxplot.stats(Yield_kgha)$stats[2],
    Yield_kgha_median   = boxplot.stats(Yield_kgha)$stats[3],
    Yield_kgha_Q3       = boxplot.stats(Yield_kgha)$stats[4],
    Yield_kgha_max      = boxplot.stats(Yield_kgha)$stats[5],
    n                   = boxplot.stats(Yield_kgha)$n,
    outliers            = list(boxplot.stats(Yield_kgha)$out))


plot3.1 <-
ggplot(p3, aes(x = factor(scenario), y = Yield_kgha, fill = as.factor(co2),colour = as.factor(co2))) +
  geom_abline(intercept=0,slope=0,linetype="dashed")+
  
  geom_half_violin(
    side = "l", 
    trim = FALSE,
    nudge = 0.03,
    alpha = 0.4,
    width = 1,
    colour=NA,
    size = 0.5,
    position = position_dodge(width = 0.6)) +
  
  geom_half_boxplot(
    side = "r", 
    position = position_dodge(width = 0.6),
    outlier.shape = NA, 
    width = 0.6, 
    color = "black", 
    size = 0.5
  )+
  temp+
  scale_colour_manual(values=c("#4dac26","#d01c8b"),
                      label=c("Without elevated CO2","With elevated CO2"))+
  scale_fill_manual(values=c("#4dac26","#d01c8b"),
                    label=c("Without elevated CO2","With elevated CO2"))+
  labs(x=element_blank(),y="Yield change (%)")+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1))+
  scale_y_continuous(breaks = c(-20,-10,0,10,20,30,40),limits = c(-20,40))

ggsave("plots/p3 - climate change with adaptation - summary.tiff",width=15,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

##p4 - Climate change with adaptation, map====

p3.2 <- p3 %>%
  mutate(scenario=factor(scenario,labels=c("2°C-increase", "Late-Maturing","Early Sowing","LM & ES"))) %>% 
  group_by(x,y,scenario,co2) %>%
  summarise(Yield_kgha=round(mean(Yield_kgha,na.rm=T),1)) %>% 
  mutate(yield_bin = cut(
           Yield_kgha,
           breaks = c(-15, 0, 15, 30, 45),
           labels = c("-15-0", "0-15", "15-30", "30-45"),
           include.lowest = TRUE),
         co2=as.factor(co2),
         co2=factor(co2,
                    levels = c("350","540"),
                    labels = c("Without elevated CO2","With elevated CO2")))
##for discussion
p3.2 %>% 
  mutate(positive_yields=ifelse(Yield_kgha>0,1,0)) %>% 
  group_by(co2,scenario) %>% 
  summarise(positive_yields=sum(positive_yields),
            total_rows=n(),
            rel = positive_yields/total_rows)

# Convert to stars object
df <- st_as_stars(p3.2, dims = c('x', 'y', 'scenario','co2'), xy = c('x', 'y'), proxy = TRUE)
st_crs(df) <- 'epsg:4326'
df <- st_transform(df, 5070)

# Load and transform Arkansas shapefile
ark <- st_read('raw-data/cropland/cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
ark <- subset(ark, STUSPS == 'AR')
ark <- st_transform(ark, 5070)

# Create raster grid for biomass
df2 <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)
df <- st_warp(df, df2, no_data_value = NA)

# Define limits for color scale
min_yield <- min(p3.2$Yield_kgha, na.rm = TRUE)
max_yield <- max(p3.2$Yield_kgha, na.rm = TRUE)

#plot
plot4 <-
ggplot() +
  geom_sf(data = ark, fill = "grey50", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = yield_bin)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  facet_grid(co2~scenario)+
  temp+
  scale_fill_manual(
    values = c("-15-0"="#b10026","0-15"="#e5f5f9","15-30"="#99d8c9","30-45"="#2ca25f","NA"= "grey50"),
    na.value = "grey50",
    na.translate = FALSE,
    name="Yield Change (%)")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 12))

ggsave("plots/p4 - climate change with adaptation - map.tiff",width=15,height=15,units ="cm",dpi=600,compression="lzw",bg="white")


plot3.1 <- plot3.1 + theme(plot.margin = margin(5, 5, 5, 5))
plot4   <- plot4   + theme(plot.margin = margin(5, 5, 5, 5))

plot5 <- plot_grid(plot3.1,plot4,
                   ncol = 2,
                   align = "v",
                   axis = "tb",
                   labels = "AUTO",
                   rel_widths = c(0.8,1))

ggsave("plots/p5 - climate change with adaptation - merged.tiff",width=30,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

#stop here, next = fill up with values and send to some co-authors

#p5 - Weather characterization====
p5a0 <- simulated0 %>% 
  filter(scenario %in% c("baseline"),
         co2 %in% c("350")) %>%
  mutate(year=year(date)) %>% 
  group_by(x,y,scenario,co2,year) %>% 
  ungroup() %>% 
  mutate(Rain.avg=mean(SeasonRain),
         Meat.avg=mean(SeasonMeanT),
         Rain.sd=sd(SeasonRain),
         Meat.sd=sd(SeasonMeanT),
         Normal=ifelse(SeasonRain<Rain.avg+Rain.sd*0.5,
                       ifelse(SeasonRain>Rain.avg-Rain.sd*0.5,
                              ifelse(SeasonMeanT<Meat.avg+Meat.sd*0.5,
                                     ifelse(SeasonMeanT>Meat.avg-Meat.sd*0.5,"Yes","No"),"No"),"No"),"No"),
         Rain.class=ifelse(SeasonRain>Rain.avg,"Wet","Dry"),
         Meat_class=ifelse(SeasonMeanT>Meat.avg,"Warm","Cool"),
         weather.class=ifelse(Normal=="Yes","Normal",paste0(Meat_class,"/",Rain.class)),
         weather.class=as.factor(weather.class),
         weather.class=factor(weather.class,levels = c("Cool/Wet","Warm/Wet","Normal","Cool/Dry","Warm/Dry"))) %>% 
  select(x,y,year,weather.class,Meat.avg,Rain.avg,Meat.sd,Rain.sd)

p5a <- simulated0 %>% 
  filter(scenario %in% c("baseline"),
         co2 %in% c("350")) %>% 
  mutate(year=year(date)) %>% 
  select(x,y,year,SeasonMeanT,SeasonRain) %>% 
  left_join(p5a0) %>% 
  filter(SeasonMeanT>20)# only 1 point not representatite

##APSIm phenology curve
# Example: triangular response curve like in your image
temp_response <- data.frame(
  temp = seq(10, 40, 0.1)
) %>%
  mutate(
    response = case_when(
      temp <= 10 ~ 0,
      temp > 10 & temp <= 30 ~ (temp - 10) / (30 - 10),  # 0 to 1
      temp > 30 & temp <= 40 ~ 1 - (temp - 30) / (40 - 30),  # 1 to 0
      temp > 40 ~ 0
    ),
    # Scale to your rainfall range (300-1400)
    response_scaled = response * (1400 - 300) + 300
  )

temp_response %>% ggplot(aes(x=temp,y=response))+geom_line()

#plotting map
plot5a <- 
p5a %>%
  #slice_sample(prop = 0.2) %>%
  ggplot(aes(x=SeasonMeanT,y=SeasonRain,fill=weather.class))+
  geom_point(size=3,alpha=0.2,shape=21)+
  temp+
  theme(legend.position = "top")+
  labs(x="Season temperature (°C)",y="Season Rainfall (mm)")+
  geom_vline(aes(xintercept=Meat.avg),size=0.5)+
  geom_hline(aes(yintercept=Rain.avg),size=0.5)+
  geom_rect(aes(xmin = Meat.avg-(Meat.sd*0.5), xmax = Meat.avg+(Meat.sd*0.5), 
                ymin = Rain.avg-(Rain.sd*0.5), ymax = Rain.avg+(Rain.sd*0.5)), 
            color = "black",fill=NA,linetype="dashed") +
  #geom_text(x=22.5,y=1250,label="Cool/Wet",hjust=0,vjust=1,colour="black")+
  #geom_text(x=22.5,y=400,label="Cool/Dry",hjust=0,vjust=0,colour="black")+
  #geom_text(x=28,y=1250,label="Warm/Wet",hjust=1,vjust=1,colour="black")+
  #geom_text(x=28,y=400,label="Warm/Dry",hjust=1,vjust=0,colour="black")+
  #geom_text(x=24.5,y=450,label="Normal",hjust=0,vjust=1,colour="black")+
  # Add temperature response curve
  geom_line(data=temp_response, 
            aes(x=temp, y=response_scaled), 
            inherit.aes=FALSE,
            color="black", linewidth=1.5)+
  
  # Add arrow pointing to the curve from top left
  annotate("segment", 
           x = 35, xend = 32.5,           
           y = 1330, yend = 1150,         
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           linewidth = 0.2, color = "black")+
  
  # Add text label in top left
  annotate("text", 
           x = 35.2, y = 1350, 
           label = "APSIM phenology\nresponse curve\n(0-1)",
           hjust = 0, vjust = 1, size = 3.5)+
  
  scale_y_continuous(limits = c(300,1400),
                     # Secondary axis (0-1) on the right side
                     sec.axis = sec_axis(~ (. - 300) / 1100, 
                                         name = "Phenology Temperature Response (0-1)",
                                         breaks = seq(0, 1, 0.2)))+
  scale_x_continuous(limits = c(10,40))+
  scale_fill_manual(values = c("Warm/Dry"="#bd0026",
                                 "Warm/Wet"="#fc8d59",
                                 "Cool/Dry"="#91cf60",
                                 "Cool/Wet"="#1a9850",
                                 "Normal"="#d95f0e"))+
  guides(fill = guide_legend(override.aes = list(alpha = 1, size = 4, shape = 21)))

##plot2

p5b <- simulated0 %>%
  filter(scenario %in% c("baseline","climate_change"),
         co2 %in% c("350")) %>% 
  mutate(year=year(date)) %>% 
  select(x,y,scenario,co2,year,EmergenceDAS,FloweringDAS,MaturityDAS) %>% 
  mutate(veg.per = FloweringDAS-EmergenceDAS,
         rep.per = MaturityDAS-FloweringDAS,
         whole.cycle=MaturityDAS-EmergenceDAS) %>%
  ungroup() %>% 
  left_join(p5a0) %>% 
  filter(!is.na(weather.class)) %>% 
  mutate(scenario=as.factor(scenario),
         scenario=factor(scenario,labels = c("Baseline", "2°C-increase")))

plot5b <-
p5b %>% 
  ggplot(aes(x=veg.per,y=scenario)) +
  geom_boxplot(aes(fill=scenario),alpha=0.5,width = 0.4)+
  temp+
  labs(x = "Calendar days to flowering", y = element_blank())+
  facet_wrap(~weather.class,nrow=1)+
  scale_fill_manual(values = c("#1a9850","#bd0026"))+
  theme(legend.position = "top",
        legend.direction = "horizontal",
        axis.text.y = element_blank())+
  theme(panel.spacing = unit(0.5, "cm"))

plot5 <- plot_grid(plot5a,plot5b,
                   ncol = 1,
                   align = "v",
                   axis = "r",
                   labels = "AUTO",
                   rel_heights = c(1,0.6))

ggsave("plots/p5 - environmental characterization.tiff",width=20,height=20,units ="cm",dpi=600,compression="lzw",bg="white")

##WUE====
##p6 - climate change without adaptation====
summary(p6)

p6 <- simulated0 %>%
  filter(scenario %in% c("baseline","early_sowing"),
         co2 %in% c(350)) %>% 
  group_by(x,y,scenario,co2) %>% 
  summarise(Yield_kgha=mean(Yield_kgha,na.rm=T),
            SeasonMeanT=mean(SeasonMeanT,na.rm=T),
            Crop_ET=mean(Crop_ET,na.rm=T),
            WDrainage=mean(WDrainage,na.rm=T),
            WRunoff=mean(WRunoff,na.rm=T),
            sWUE=mean(sWUE,na.rm=T),
            SeasonRain=mean(SeasonRain,na.rm=T),
            swhc_60cm=mean(swhc_24in,na.rm=T)*2.54) %>% 
  ungroup() %>% 
  select(-co2)

# Convert to stars object
df <- st_as_stars(p6, dims = c('x', 'y','scenario'), xy = c('x', 'y'), proxy = TRUE)
st_crs(df) <- 'epsg:4326'
df <- st_transform(df, 5070)

# Create raster grid for biomass
df2 <- st_as_stars(st_bbox(ark), dx = 2500, dy = 2500)
df <- st_warp(df, df2, no_data_value = NA)

#plot 2B

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = sWUE)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))+
  facet_wrap(~scenario)
ggsave("plots/wue_sWUE.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")





ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = Yield_kgha)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))+
  facet_wrap(~scenario)
ggsave("plots/wue_yield.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")


##Check here....temperature is weird
ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = SeasonMeanT)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))+
  facet_wrap(~scenario)
ggsave("plots/wue_SeasonMeanT.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = Crop_ET)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))+
  facet_wrap(~scenario)
ggsave("plots/wue_Crop_ET.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = WDrainage)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))
ggsave("plots/wue_WDrainage.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = WRunoff)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))
ggsave("plots/wue_WRunoff.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = sWUE)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))
ggsave("plots/wue_sWUE.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = swhc_24in)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))
ggsave("plots/wue_swhc_24in.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = swhc_60cm)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))
ggsave("plots/wue_swhc_60cm.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

ggplot() +
  geom_sf(data = ark, fill = "grey30", color = "black") +  # State boundaries
  geom_stars(data = df, aes(fill = SeasonRain)) +  # Raster data
  geom_sf(data = usa_counties, aes(geometry = geometry),fill=NA,colour="black",linewidth=0.5)+
  coord_sf(xlim = c(360000,570000)) +  # Maintain spatial coordinates
  temp +
  scale_fill_viridis_c(                                   # Continuous color scale
    option = "viridis",
    na.value = "transparent")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 12))
ggsave("plots/wue_SeasonRain.tiff",width=12,height=15,units ="cm",dpi=600,compression="lzw",bg="white")
