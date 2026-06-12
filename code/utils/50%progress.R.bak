rm(list=ls())

##Purpose: determine 50% date of USDA-NASS progress for corn and soybean 
#  corn: planting, emerge, silking, dough, dent, mature, harvest
#  soybean: planting, emerg, blooming, pod_set, full_pod, mature, harvest

## Author: Mitch Baum

## Date modified: 2/4/2020

library(tidyverse)
library(lubridate)
library(broom)
library(stringr)
library(readxl)
library(rstudioapi)
library(metan)
#####################################################################################################################
#####################################################################################################################

## read in data sets for both corn and soybean

setwd(dirname(getActiveDocumentContext()$path))
source("plot_theme.R")



nass0 <- read_excel("progress.xlsx") %>%
  tidy_colnames() %>% 
  tidy_strings(DATA_ITEM) %>% 
  filter(grepl("PLANTED",DATA_ITEM),
         YEAR<2024,
         YEAR>=1990) %>% 
  separate(DATA_ITEM, into = c("Crop","b","c","d","e","f","g"),sep="_") %>% 
  select(-c(b:g))

######################################################################################################################
######################################################################################################################

## clean data sheets and label them corn and soy 
# corn
nass <- nass0 %>%
  mutate(year = YEAR, 
         week = WEEK_ENDING,
         state = STATE,
         value = VALUE,
         crop = Crop) %>% 
  dplyr::select(year, state, crop, week, value) %>% 
  mutate(week = as.Date(week),
         DOY = strftime(week, format = "%j"),
         DOY = as.integer(as.numeric(DOY)))

#######################################################################################################################
#######################################################################################################################
## 50% sowing
corn_pl_planting <- nass %>% 
  mutate(value = value/100) %>% 
  group_by(state, year, crop) %>% 
  nest() %>% 
  group_by(state, year, crop) %>% 
  mutate(model = purrr::map(data, ~try(nls(value ~ 1/(1 + exp(-b * (DOY - c))), 
                                           start=list(b=0.05, c=130),##wheat is not converging
                                           data = .)))) %>%
  filter(class(model[[1]]) != "try-error") %>% 
  mutate(pred = purrr::map(model, tidy)) %>% 
  dplyr::select(pred) %>% 
  unnest() %>% 
  dplyr::select(term,estimate) %>% 
  spread(term,estimate) %>% 
  mutate(DOY = list(90:180)) %>% 
  unnest() %>% 
  mutate(pct_50 = round(c, digits = 0)) %>% 
  summarise(pct_50 = mean(pct_50)) %>% 
  filter(year != is.na(year))

##slopes

slopes.df <- corn_pl_planting %>% 
  group_by(crop) %>% 
  do(tidy(lm(pct_50~year,data=.))) %>%
  dplyr::select(1:3) %>% 
  spread(term,estimate) %>% 
  rename(interc=`(Intercept)`,
         slope=year)



##plot
corn_pl_planting %>% 
  filter(crop %in% c("COTTON","RICE","SOYBEANS")) %>% 
  ggplot(aes(x=year,y=pct_50,colour=crop,shape=crop,fill=crop)) +
  geom_point(size=3,colour="black")+
  geom_smooth(se=F,method = "lm")+
  scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(values = c("black","#A3A500","#00BF7D"))+
  scale_colour_manual(values = c("black","#A3A500","#00BF7D"))+
  temp+
  theme(legend.position = "none")+
  geom_text(aes(x=2005,y=155,label="Soybeans (-0.91 days/year)"),colour="#00BF7D")+
  geom_text(aes(x=2000,y=105,label="Rice (-0.26 days/year)"),colour="#A3A500")+
  geom_text(aes(x=1998,y=135,label="Cotton"),colour="black")+
  labs(x="Year",y="Day of year") +
  scale_y_continuous(labels = c("100 (4/10)","120 (4/30)","140 (5/20)","160 (6/9)"),breaks = c(100,120,140,160))

ggsave("sowing progress.tiff",width=17,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

##USDA proposal
corn_pl_planting %>% 
  filter(crop %in% c("COTTON","SOYBEANS")) %>% 
  ggplot(aes(x=year,y=pct_50,colour=crop,shape=crop,fill=crop)) +
  geom_point(size=3,colour="black")+
  geom_smooth(se=F,method = "lm")+
  scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(values = c("#A3A500","#00BF7D"))+
  scale_colour_manual(values = c("#A3A500","#00BF7D"))+
  temp+
  theme(legend.position = "none")+
  geom_text(aes(x=2005,y=155,label="Soybeans (-0.91 days/year)"),colour="#00BF7D")+
  geom_text(aes(x=1998,y=135,label="Cotton"),colour="#A3A500")+
  labs(x="Year",y="Day of year") +
  scale_y_continuous(labels = c("100 (4/10)","120 (4/30)","140 (5/20)","160 (6/9)"),breaks = c(100,120,140,160))

ggsave("sowing progress - USDA proposal.tiff",width=15,height=15,units ="cm",dpi=600,compression="lzw",bg="white")




##flowering

flowering0 <- read_excel("progress.xlsx") %>%
  tidy_colnames() %>% 
  tidy_strings(DATA_ITEM) %>% 
  filter(grepl("BLOOMING",DATA_ITEM),
         YEAR<2024,
         YEAR>=1990) %>% 
  separate(DATA_ITEM, into = c("Crop","b","c","d","e","f","g"),sep="_") %>% 
  select(-c(b:g))

######################################################################################################################
######################################################################################################################

## clean data sheets and label them corn and soy 
# corn
flowering <- nass0 %>%
  mutate(year = YEAR, 
         week = WEEK_ENDING,
         state = STATE,
         value = VALUE,
         crop = Crop) %>% 
  dplyr::select(year, state, crop, week, value) %>% 
  mutate(week = as.Date(week),
         DOY = strftime(week, format = "%j"),
         DOY = as.integer(as.numeric(DOY)))

#######################################################################################################################
#######################################################################################################################
## 50% flowering
corn_pl_flowering <- nass %>% 
  mutate(value = value/100) %>%
  filter(crop=="SOYBEANS") %>% 
  group_by(state, year, crop) %>% 
  nest() %>% 
  group_by(state, year, crop) %>% 
  mutate(model = purrr::map(data, ~try(nls(value ~ 1/(1 + exp(-b * (DOY - c))), 
                                           start=list(b=0.05, c=120),##wheat is not converging
                                           data = .)))) %>%
  filter(class(model[[1]]) != "try-error") %>% 
  mutate(pred = purrr::map(model, tidy)) %>% 
  dplyr::select(pred) %>% 
  unnest() %>% 
  dplyr::select(term,estimate) %>% 
  spread(term,estimate) %>% 
  mutate(DOY = list(90:180)) %>% 
  unnest() %>% 
  mutate(pct_50 = round(c, digits = 0)) %>% 
  summarise(pct_50 = mean(pct_50)) %>% 
  filter(year != is.na(year))

mean(corn_pl_flowering$pct_50)

as.Date("2023-01-01") + mean(corn_pl_flowering$pct_50)

as.Date("2023-10-01") - as.Date("2023-01-01")

##slopes

slopes.df <- corn_pl_planting %>% 
  group_by(crop) %>% 
  do(tidy(lm(pct_50~year,data=.))) %>%
  dplyr::select(1:3) %>% 
  spread(term,estimate) %>% 
  rename(interc=`(Intercept)`,
         slope=year)



##plot
corn_pl_planting %>% 
  filter(crop %in% c("COTTON","RICE","SOYBEANS")) %>% 
  ggplot(aes(x=year,y=pct_50,colour=crop,shape=crop,fill=crop)) +
  geom_point(size=3,colour="black")+
  geom_smooth(se=F,method = "lm")+
  scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(values = c("black","#A3A500","#00BF7D"))+
  scale_colour_manual(values = c("black","#A3A500","#00BF7D"))+
  temp+
  theme(legend.position = "none")+
  geom_text(aes(x=2005,y=155,label="Soybeans (-0.91 days/year)"),colour="#00BF7D")+
  geom_text(aes(x=2000,y=105,label="Rice (-0.26 days/year)"),colour="#A3A500")+
  geom_text(aes(x=1998,y=135,label="Cotton"),colour="black")+
  labs(x="Year",y="Day of year") +
  scale_y_continuous(labels = c("100 (4/10)","120 (4/30)","140 (5/20)","160 (6/9)"),breaks = c(100,120,140,160))

ggsave("sowing progress.tiff",width=17,height=15,units ="cm",dpi=600,compression="lzw",bg="white")

##plot
corn_pl_planting %>% 
  filter(crop %in% c("SOYBEANS")) %>% 
  ggplot(aes(x=year,y=pct_50,colour=crop,shape=crop,fill=crop)) +
  geom_point(size=3,colour="black")+
  geom_smooth(se=F,method = "lm")+
  scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(values = c("#00BF7D"))+
  scale_colour_manual(values = c("#00BF7D"))+
  temp+
  theme(legend.position = "none")+
  geom_text(aes(x=2005,y=155,label="Slope = -0.91 days/year"),colour="#00BF7D")+
  labs(x="Year",y="Day of year") +
  scale_y_continuous(labels = c("100 (4/10)","120 (4/30)","140 (5/20)","160 (6/9)","180 (6/29)"),breaks = c(100,120,140,160,180),limits = c(100,180))

ggsave("sowing progress - only soybeans.tiff",width=17,height=15,units ="cm",dpi=600,compression="lzw",bg="white")


##Flowering

flow <- read_excel("progress.xlsx") %>%
  tidy_colnames() %>% 
  tidy_strings(DATA_ITEM) %>% 
  filter(grepl("BLOOMING",DATA_ITEM),
         YEAR<2024,
         YEAR>=1990) %>% 
  separate(DATA_ITEM, into = c("Crop","b","c","d","e","f","g"),sep="_") %>% 
  select(-c(b:g))

######################################################################################################################
######################################################################################################################

## clean data sheets and label them corn and soy 
# corn
flow2 <- flow %>%
  mutate(year = YEAR, 
         week = WEEK_ENDING,
         state = STATE,
         value = VALUE,
         crop = Crop) %>% 
  dplyr::select(year, state, crop, week, value) %>% 
  mutate(week = as.Date(week),
         DOY = strftime(week, format = "%j"),
         DOY = as.integer(as.numeric(DOY)))

#######################################################################################################################
#######################################################################################################################
## 50% sowing
soy_flow <- flow2 %>% 
  mutate(value = value/100) %>% 
  group_by(state, year, crop) %>% 
  nest() %>% 
  group_by(state, year, crop) %>% 
  mutate(model = purrr::map(data, ~try(nls(value ~ 1/(1 + exp(-b * (DOY - c))), 
                                           start=list(b=0.05, c=130),##wheat is not converging
                                           data = .)))) %>%
  filter(class(model[[1]]) != "try-error") %>% 
  mutate(pred = purrr::map(model, tidy)) %>% 
  dplyr::select(pred) %>% 
  unnest() %>% 
  dplyr::select(term,estimate) %>% 
  spread(term,estimate) %>% 
  mutate(DOY = list(90:180)) %>% 
  unnest() %>% 
  mutate(pct_50 = round(c, digits = 0)) %>% 
  summarise(pct_50 = mean(pct_50)) %>% 
  filter(year != is.na(year))

##slopes

slopes.df.flow <- soy_flow %>% 
  group_by(crop) %>% 
  do(tidy(lm(pct_50~year,data=.))) %>%
  dplyr::select(1:3) %>% 
  spread(term,estimate) %>% 
  rename(interc=`(Intercept)`,
         slope=year)


##plot
soy_flow %>% 
  filter(crop %in% c("COTTON","RICE","SOYBEANS")) %>% 
  ggplot(aes(x=year,y=pct_50,colour=crop,shape=crop,fill=crop)) +
  geom_point(size=3,colour="black")+
  geom_smooth(se=F,method = "lm")+
  scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(values = c("black","#A3A500","#00BF7D"))+
  scale_colour_manual(values = c("black","#A3A500","#00BF7D"))+
  temp+
  theme(legend.position = "none")+
  labs(x="Year",y="Day of year") 

ggsave("soy flowering progress.tiff",width=17,height=15,units ="cm",dpi=600,compression="lzw",bg="white")







