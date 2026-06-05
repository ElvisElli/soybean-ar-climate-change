## This script was written to carry out the grid simulations
## for the cropland in the state of Arkansas

rm(list=ls())

#When switching desktop vs laptop, changes in lines 25 and 124

## libraries
##devtools::install_github('femiguez/apsimx')

library(apsimx)
library(ggplot2)
library(stars)
library(sf)
library(readr)
library(dplyr)
library(readxl)
library(lubridate)

## Set up the number of cores to use in the simulation
cores <- 10

#?apsimx_options
#for laptop, no need, for desktop, yes
#apsimx_options(exe.path = 'C:\\Users\\efelli\\AppData\\Local\\Programs\\APSIM2025.3.7681.0\\bin\\Models.exe')
#apsim_version(which = c("inuse"))

## reading the simulation grid & scenarios
sim.grid <- readRDS('./intermediate-data/sim-grid.rds')

sim.grid1 <- sim.grid %>% filter(!is.na(cultivated))

scenarios <- read_excel("intermediate-data/scenarios/soy-scenarios-10-24.xlsx",sheet = "Sheet1") %>% as.data.frame()

## Use a local temporary folder to avoid Box sync conflicts
local_proc_dir <- "C:/temp/apsim-proc"

##creating an apsim copy
file.copy(paste0("processed-data/_soybean-10-24-25.apsimx"),
          paste0(local_proc_dir,"/grid-simulation-file.apsimx"),
          overwrite = TRUE)

## grid and soil file index
sim.grid$cellid <- 1:nrow(sim.grid)

## Changing the clock
edit_apsimx(file = 'grid-simulation-file.apsimx',
            src.dir = local_proc_dir,
            wrt.dir = local_proc_dir,
            node = 'Clock',
            parm = 'Start',
            value = '1985-01-01',#
            overwrite = TRUE)


edit_apsimx(file = 'grid-simulation-file.apsimx',
            src.dir = local_proc_dir,
            wrt.dir = local_proc_dir,
            node = 'Clock',
            parm = 'End',
            value = '2024-12-31',
            overwrite = TRUE)

final.df <- vector('list', nrow(scenarios))

for (i in 1:nrow(scenarios)
     ){
  #i <- 1
  #i <- 1nrow(scenarios)
  #this_scenario <- scenarios$scenario[i]
  # this_co2 <- scenarios$co2[i]
  
  edit_apsimx(file = 'grid-simulation-file.apsimx',
              src.dir = local_proc_dir,
              wrt.dir = local_proc_dir,
              node = 'Other',
              parm.path = '.Simulations.Simulation.Field.SowSoybean.CultivarName',
              value = scenarios[i, 'cultivar'],
              verbose = FALSE,
              overwrite = TRUE)
  
  edit_apsimx(file = 'grid-simulation-file.apsimx',
              src.dir = local_proc_dir,
              wrt.dir = local_proc_dir,
              node = 'Other',
              parm.path = '.Simulations.Simulation.Field.SowSoybean.SowDate',
              value = scenarios[i, 'sowing'],
              verbose = FALSE,
              overwrite = TRUE)
  
  edit_apsimx(file = 'grid-simulation-file.apsimx',
              src.dir = local_proc_dir,
              wrt.dir = local_proc_dir,
              node = 'Other',
              parm.path = '.Simulations.Simulation.Field.ClimateController.EnableDate',
              value = scenarios[i, 'climate.control'],
              verbose = FALSE,
              overwrite = TRUE)
  
  edit_apsimx(file = 'grid-simulation-file.apsimx',
              src.dir = local_proc_dir,
              wrt.dir = local_proc_dir,
              node = 'Other',
              parm.path = '.Simulations.Simulation.Field.SowSoybean.RowSpacing',
              value = scenarios[i, 'RowSpacing'],
              verbose = FALSE,
              overwrite = TRUE)
  
  edit_apsimx(file = 'grid-simulation-file.apsimx',
              src.dir = local_proc_dir,
              wrt.dir = local_proc_dir,
              node = 'Other',
              parm.path = '.Simulations.Simulation.Field.CO2.CO2',
              value = scenarios[i, 'co2'],
              verbose = FALSE,
              overwrite = TRUE)
  
  max.cores <- parallel::detectCores(logical = FALSE)
  ncores <- ifelse(cores > max.cores, max.cores, cores)
  cl <- parallel::makeCluster(ncores, outfile = "log.txt")
  #for laptop, no need, for desktop, yes
  #parallel::clusterEvalQ(cl, {library('apsimx');apsimx::apsimx_options(exe.path = 'C:\\Users\\efelli\\AppData\\Local\\Programs\\APSIM2025.3.7681.0\\bin\\Models.exe')
  parallel::clusterEvalQ(cl, {library('apsimx')})
  
  parallel::clusterExport(cl, 
                          c('sim.grid', 'scenarios', 'i'))
  
  #print(scenarios[i, ])
  cat('Running scenario: ', scenarios$scenario[i],'\n')
  
  scenario.df <- parallel::parLapply(cl,
                                    1:nrow(sim.grid),
                                     function(j){
                                       
                                       #print(scenarios[i, ])
                                                                             # j <- 143
                                       if (is.na(sim.grid[j, 'cultivated']))
                                         return(NULL)
                                       #paths (the ones from the R project were not working)
                                       #weather_path <- paste0(getwd(), "/intermediate-data/weather/")
                                       #soil_path <- paste0(getwd(), "/intermediate-data/soil/")
                                       #crop_path <- paste0(getwd(), "/intermediate-data/crop-9-29-25/")
                                       
                                       if (j %in% seq(1, nrow(sim.grid), 500)){
                                         cat(paste("Sleeping after simulation #", j))
                                         Sys.sleep(5 * 60)
                                       }
                                       
                                       weather_path <- normalizePath(file.path(getwd(), "intermediate-data", "weather","\\"))
                                       soil_path <- normalizePath(file.path(getwd(), "intermediate-data", "soil","\\"))
                                       crop_path <- normalizePath(file.path(getwd(), "intermediate-data", "crop-9-29-25","\\"))

                                      cat('Processing cell # ', j, ' out of ', nrow(sim.grid),"| Scenario = ",scenarios$scenario[i]," | CO2 = ",scenarios$co2[i],"(# ",i," out of ",nrow(scenarios),")", '\n')
                                      
                                      #Fixing soil issues
                                      soil.result <- try(readRDS(paste0(soil_path, j, '.rds')), silent = TRUE)
                                       
                                       if (inherits(soil.result, "try-error") || 
                                           !is.list(soil.result) ||
                                           length(soil.result) < 1 || 
                                           !is.list(soil.result[[1]]) || 
                                           length(soil.result[[1]]) < 1 || 
                                           !inherits(soil.result[[1]][[1]], "soil_profile")) {
                                         
                                         return(NULL)
                                       }
                                       
                                       soils <- soil.result[[1]][[1]]
                                       
                                       ##Fixing KSAT so that the bottom profile does not drain a lot, decreasing exponential function
                                       soils$soil$KS
                                       KS_max <- max(soils$soil$KS, na.rm = TRUE)
                                       target_fraction <- 0.0001
                                       fractions <- exp(seq(from = 0, to = log(target_fraction), length.out = length(soils$soil$KS)))
                                       soils$soil$KS <- KS_max * fractions
                                       #plot(soils$soil$KS)
                                       
                                       kl <- c(0.08,0.08,0.08,0.08,0.07,0.07,0.07,0.07,0.06,0.06,0.06,0.06,0.05,0.05,0.04,0.04,0.03,0.03,0.02,0.02)
                                       xf <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)
                                       
                                       soils <- apsimx:::fix_apsimx_soil_profile(soils, verbose = FALSE)
                                       soils$initialwater <- initialwater_parms(Depth = soils$soil$Depth, Thickness = soils$soil$Thickness, InitialValues = soils$soil$DUL)
                                       soils$crops <- c("Soybean","Wheat","Maize")
                                       
                                       ## Use a local temporary folder to avoid Box sync conflicts
                                       local_proc_dir <- "C:/temp/apsim-proc"
                                       
                                       #Creating a temporary APSIM file
                                       par.sim.file <- paste0('par-sim-',j,'.apsimx')
                                       
                                       file.copy(file.path(local_proc_dir, 'grid-simulation-file.apsimx'),
                                                 file.path(local_proc_dir, par.sim.file),
                                                 overwrite = TRUE)
                                       

                                       edit_apsimx_replace_soil_profile(file = par.sim.file,
                                                                        src.dir = local_proc_dir,
                                                                        wrt.dir = local_proc_dir,
                                                                        soil.profile = soils,
                                                                        verbose = FALSE,
                                                                        overwrite = TRUE)
                                       
                                       edit_apsimx(file = par.sim.file,
                                                   src.dir = local_proc_dir,
                                                   wrt.dir = local_proc_dir,
                                                   node = "Soil",
                                                   soil.child = "Physical", 
                                                   parm = "KL", value = kl,
                                                   verbose = FALSE,
                                                   overwrite = TRUE)
                                       
                                       edit_apsimx(file = par.sim.file,
                                                   src.dir = local_proc_dir,
                                                   wrt.dir = local_proc_dir,
                                                   node = "Soil",
                                                   soil.child = "Physical", 
                                                   parm = "XF", value = xf,
                                                   verbose = FALSE,
                                                   overwrite = TRUE)
                                       
                                       #Replacing weather
                                       edit_apsimx(file = par.sim.file,
                                                   src.dir = local_proc_dir,
                                                   wrt.dir = local_proc_dir,
                                                   node = 'Weather',
                                                   value = paste0(weather_path, j, '.met'),
                                                   overwrite = TRUE,
                                                   verbose = FALSE)
                                       
                                       sim <- try(apsimx(file = par.sim.file,
                                                         src.dir =local_proc_dir,
                                                         cleanup = TRUE), silent = TRUE)
                                       
                                       if (inherits(sim, "try-error")) return(NULL)
                                       

                                       ans <- data.frame(cultivar = scenarios$cultivar[i],
                                                         sowing = scenarios$sowing[i],
                                                         scenario = scenarios$scenario[i],
                                                         climate.control = scenarios$climate.control[i],
                                                         co2 = scenarios$co2[i],
                                                         rowSpacing = scenarios$RowSpacing[i],
                                                         date = sim$Date,
                                                         EmergenceDAS = sim$EmergenceDAS,
                                                         FloweringDAS = sim$FloweringDAS,
                                                         SeedFillingDAS = sim$SeedFillingDAS,
                                                         MaturityDAS = sim$MaturityDAS,
                                                         CumRadiationInterceptionOnGreen = sim$CumRadiationInterceptionOnGreen,
                                                         Yield_kgha = sim$Yield_kgha,
                                                         biomass_kgha = sim$biomass_kgha,
                                                         SeasonRain = sim$SeasonRain,
                                                         SeasonRadn = sim$SeasonRadn,
                                                         SeasonMaxt = sim$SeasonMaxt,
                                                         SeasonMint = sim$SeasonMint,
                                                         SeasonMeanT = sim$SeasonMeanT,
                                                         BloomingSeasonMaxt = sim$BloomingSeasonMaxt,
                                                         Silking_RUE_Temp = sim$Silking_RUE_Temp,
                                                         Silking_Supply_Demand_Ratio = sim$Silking_Supply_Demand_Ratio,
                                                         swhc_6in = sim$swhc_6in,
                                                         swhc_12in = sim$swhc_12in,
                                                         swhc_24in = sim$swhc_24in,
                                                         Crop_ET = sim$Crop_ET,
                                                         WDrainage = sim$WDrainage,
                                                         WRunoff = sim$WRunoff,
                                                         sWUE = sim$sWUE)
                                       
                                       ans <- merge(sim.grid[j,],
                                                    ans)
                                       
                                       readr::write_csv(ans,
                                                        paste0(crop_path, j,"_",scenarios$scenario[i],".csv"))

                                       file.remove(file.path(local_proc_dir, par.sim.file))
                                      return(ans)
                                       
                                       
                                     })
  
  parallel::stopCluster(cl)
  
  scenario.df <- scenario.df[!sapply(scenario.df, is.null)]
  
  scenario.df <- do.call(rbind, scenario.df)
  
  final.df[[i]] <- scenario.df
  
}

final.df <- do.call(rbind, final.df)

saveRDS(final.df, './intermediate-data/simulated-scenarios-df.rds')
write_csv(final.df, './intermediate-data/simulated-scenarios-df.csv')


