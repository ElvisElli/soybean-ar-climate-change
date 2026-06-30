## Cleaning up environment ====
rm(list=ls())

## Libraries ====
library(rstudioapi)
library(tidyverse)
library(readxl)
library(apsimx)
apsimx_options(exe.path = 'C:/APSIM2025.10.7895.0/bin/Models.exe')

extd.dir <- system.file("extdata", package = "apsimx")

## Set working directory ====
setwd(dirname(getActiveDocumentContext()$path))

apsimx_options(exe.path = 'C:/APSIM2025.10.7895.0/bin/Models.exe')



apsim_version(which = c("inuse"))


obs.soy.data <- read_excel("C:/Teste_Apsim/PO_Battist_Sentelhas_SSD.xlsx") %>% as.data.frame()
obs.soy.data$Planting_date <- as.Date(obs.soy.data$Planting_date)

head(obs.soy.data)

str(obs.soy.data)

dim(obs.soy.data)

obs.soy.data$Planting_date <- as.POSIXct(obs.soy.data$Planting_date, tz = "UTC")

ggplot(obs.soy.data, aes(Planting_date, DaysToR1)) + 
  geom_line()

sim0 <- apsimx("Phenology_Sentelhas_2013_14_SSD.apsimx", value = "SoybeanReportHarvest")
str(sim0)



## Parte Nova - CORRIGIDA
inspect_apsimx_replacement("Phenology_Sentelhas_2013_14_SSD.apsimx",
                           node = "Replacement",
                           node.child = "Soybean",
                           node.subchild = "Elli",
                           node.subsubchild = "Elli_late6",
                           verbose = FALSE)



# Valores iniciais fornecidos
par_initial <- c(
  396,
  140,
  0.072,
  664,
  12.58, 15.8,
  13.1, 16.49
)

# Limites inferiores (lower bounds) baseados na Tabela 1
# 'Vegetative' (310-470), 'EarlyFlower' (100-200), 'Fraction' (estimado), 'EntireGrainFill' (590-748)
# 'Photoperiod' Pcrit2 (11.8-14.6), Pcrit1 (14.7-22.4)
par_lower <- c(
  310,    # Vegetative
  100,    # EarlyFlowering
  0.05,   # EarlyGrainFilling.Fraction - *Intervalo estimado, pois não está na tabela*
  590,    # EntireGrainfillPeriod
  11.8,   # VegetativePhotoperiod... X[1] (associado a Pcrit2)
  14.7,   # VegetativePhotoperiod... X[2] (associado a Pcrit1)
  11.8,   # ReproductivePhotoperiod... X[1] (associado a Pcrit2)
  14.7    # ReproductivePhotoperiod... X[2] (associado a Pcrit1)
)

# Limites superiores (upper bounds) baseados na Tabela 1
par_upper <- c(
  470,    # Vegetative
  200,    # EarlyFlowering
  0.2,    # EarlyGrainFilling.Fraction - *Intervalo estimado*
  748,    # EntireGrainfillPeriod
  14.6,   # VegetativePhotoperiod... X[1]
  22.4,   # VegetativePhotoperiod... X[2]
  14.6,   # ReproductivePhotoperiod... X[1]
  22.4    # ReproductivePhotoperiod... X[2]
)



pp1 <- "Soybean.Elli.Elli_late6.Vegetative"

wop <- optim_apsimx("Phenology_Sentelhas_2013_14_SSD.apsimx", 
                    parm.paths = c(pp1),
                    data = obs.soy.data,
                    weights = "mean",
                    index = c("DaysToR1","SimulationName"),
                    replacement = c(TRUE),
                    initial.values = c(396))