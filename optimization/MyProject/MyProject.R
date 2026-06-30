## Cleaning up environment ====
rm(list=ls())

## Libraries ====
library(rstudioapi)
library(tidyverse)
library(readxl)
library(apsimx)


extd.dir <- system.file("extdata", package = "apsimx")

## Set working directory ====
setwd(dirname(getActiveDocumentContext()$path))

apsimx_options(exe.path = 'C:/Program Files/APSIM2025.10.7893.0/bin/Models.exe')

apsim_version(which = c("inuse"))


obs.soy.data <- read_excel("C:/Teste_Apsim/PO_Battist_Sentelhas_SSD.xlsx") %>% as.data.frame()
obs.soy.data$Planting_date <- as.Date(obs.soy.data$Planting_date)

head(obs.soy.data)

str(obs.soy.data)

dim(obs.soy.data)

obs.soy.data$Planting_date <- as.POSIXct(obs.soy.data$Planting_date, tz = "UTC")

ggplot(obs.soy.data, aes(Planting_date, DaysToR1)) + 
  geom_line()

sim0 <- apsimx("Soybean.apsimx", src.dir = extd.dir, value = "report")
