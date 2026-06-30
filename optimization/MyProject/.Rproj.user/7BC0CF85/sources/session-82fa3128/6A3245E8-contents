library(apsimx)
library(ggplot2)

apsimx_options(exe.path = 'C:/APSIM2025.10.7895.0/bin/Models.exe')

apsimx_options(exe.path = 'C:/PROGRA~1/APSIM2025.10.7895.0/bin/Models.exe')
apsimx:::auto_detect_apsimx()


## reading the data in
dat <- read.csv(file = './raw-data/phenology-all-sites.csv')
site.info <- read.csv(file = './raw-data/site-info.csv')


## merging phenology and location information
dat <- merge(dat, site.info)
dat <- dat[order(dat$mg), ]


## cleaning some of the data
dat$date <- as.Date(dat$date, '%m/%d/%Y')
dat <- subset(dat, !(as.numeric(strftime(date, '%m')) == 12 & lat > 0))
#dat <- subset(dat, !(as.numeric(strftime(date, '%m')) < 7 & 
#                       phenology.num >= 10 &
#                       author == 'Salmeron'))


## translating between APSIM phenology and Fehr and Caviness
phen.dict <- data.frame(apsim.phen = c("Sowing", "Germination", "Emergence", "StartFlowering", 
                                       "StartPodDevelopment", "StartGrainFilling", "EndCanopyDevelopment", 
                                       "EndPodDevelopment", "EndGrainFill", "Maturity", "HarvestRipe"),
                        fc.phen = c(NA, NA, 'VE', 'R1', 'R3', 'R5', NA, NA, 'R6', 'R7', 'R8'))
phen.dict <- na.omit(phen.dict)

## Downloading soil and weather information
if (FALSE){
  
  site.info <- unique(dat[c('site', 'lat', 'lon')])
  
  for (i in 1:nrow(site.info)){
    
    pwr <- get_power_apsim_met(unlist(site.info[i, c('lon', 'lat')]),
                               c('1990-01-01', '2024-12-31'))
    
    soil <- try(get_ssurgo_soil_profile(unlist(site.info[i, c('lon', 'lat')])),
                silent = TRUE)
    if (inherits(soil, 'try-error')){
      soil <- try(get_isric_soil_profile(unlist(site.info[i, c('lon', 'lat')])),
                  silent = TRUE)
    }
    
    saveRDS(soil, paste0('./simulations/soil/', 
                         site.info$site[i],
                         '.rds'))
    write_apsim_met(pwr,
                    wrt.dir = './simulations/weather/',
                    paste0(site.info$site[i],'.met'))
    
    
    !any(sapply(list(pwr, soil), function(x) inherits(x, 'try-error')))
      cat('Sucess for  ', site.info[i, 1], '\n')
    
    Sys.sleep(15)
    
    
    
  }
  
  
}




## initial apsim values

apsim.mg.values <- data.frame(mg = 0:8,
                              vegetative = c(320, 340, 348, 380, 388, 396, 404, 416, 430),
                              early.flowering = c(100, 120, 120, 120, 140, 160, 180, 200, 200),
                              early.grain = c(0.467, 0.411, 0.386, 0.361, 0.324, 0.072, 0.056, 0.055, 0.054),
                              late.grain = c(600, 632, 648, 664, 664, 696, 712, 728, 744),
                              photoperiod.modifier1 = c(14.35, 13.84, 13.59, 13.4, 13.1, 12.83, 12.58, 12.33, 12.07),
                              photoperiod.modifer2 = c(21.11, 18.77, 17.61, 16.91, 16.49, 16.13, 15.8, 15.46, 15.1))



for (mg.tgt in unique(dat$mg2)[7:17]){
  mdat <- subset(dat, mg2 == mg.tgt)
  mdat$year <- as.numeric(strftime(mdat$date, '%Y'))
  mdat$pd <- as.Date(mdat$pd, tryFormats = c('%m/%d/%Y', "%m-%d-%Y"))
  mdat$pd.number <- mdat$pd |> as.Date('%M/%d/%Y') |> as.numeric()
  mdat$id<- paste(mdat$site, mdat$year, mdat$pd.number, sep = '-')
  mdat <- subset(mdat, !is.na(pd))
  cultivar.name <- paste0('Elli_', mg.tgt)
  
  
  
  run_mg <- function(mdat){
    mg.res <- vector('list', length(unique(mdat$id)))
    names(mg.res) <- unique(mdat$id)
    
    for (i in unique(mdat$id)){
      idat <- subset(mdat, id == i )
      
      site.name <- idat$site[1]
      soil <- readRDS(paste0('./simulations/soil/', site.name, '.rds'))
      if(inherits(soil, 'try-error'))
        next
      
      if(inherits(soil, 'list'))
        soil <- soil[[1]]
      soil$initialwater <- initialwater_parms(
        Thickness = soil$soil$Thickness,
        InitialValues = soil$soil$DUL)
      
      met <- paste0('./weather/', site.name, '.met')
      
      ## here we will update the apsim file to represent
      ## our simulation
      
      pp1 <- '.Simulations.Simulation.Field.Sowing'
      sim.pd <- strftime(unique(idat$pd), '%b-%d')
      start.date <- as.character(idat$pd[1] - 90)
      end.date <- as.character(max(idat$date) + 30)
      
      
      edit_apsimx(file = 'base-simulation.apsimx',
                  src.dir = './simulations/',
                  wrt.dir = './simulations/',
                  node = 'Clock',
                  parm = 'Start',
                  verbose = FALSE,
                  value = start.date,
                  overwrite = TRUE)
      
      edit_apsimx(file = 'base-simulation.apsimx',
                  src.dir = './simulations/',
                  wrt.dir = './simulations/',
                  node = 'Clock',
                  parm = 'End',
                  verbose = FALSE,
                  value = end.date,
                  overwrite = TRUE)
      
      
      
      edit_apsimx(file = 'base-simulation.apsimx',
                  src.dir = './simulations/',
                  wrt.dir = './simulations/',
                  parm.path = pp1,
                  node = 'Other',
                  parm = 'CultivarName',
                  verbose = FALSE,
                  value = cultivar.name,
                  overwrite = TRUE)
      
      edit_apsimx(file = 'base-simulation.apsimx',
                  src.dir = './simulations/',
                  wrt.dir = './simulations/',
                  parm.path = pp1,
                  node = 'Other',
                  parm = 'SowDate',
                  verbose = FALSE,
                  value = sim.pd,
                  overwrite = TRUE)
      
      edit_apsimx(file = 'base-simulation.apsimx',
                  src.dir = './simulations/',
                  wrt.dir = './simulations/',
                  node = 'Weather',
                  value = met,
                  verbose = FALSE,
                  overwrite = TRUE)
      
      edit_apsimx_replace_soil_profile(file = 'base-simulation.apsimx',
                                       src.dir = './simulations/',
                                       wrt.dir = './simulations/',
                                       verbose = FALSE,
                                       soil.profile = soil,
                                       overwrite = TRUE)
      
      sim0 <- try(apsimx(file = 'base-simulation.apsimx',
                     src.dir = './simulations/',
                     cleanup = TRUE),
                  silent = TRUE)
      if (inherits(sim0, 'try-error'))
        next
      
      sim0$id <- i
      row.names(sim0) <- NULL
      mg.res[[i]] <- sim0
    }
    
    mg.res <- do.call('rbind', mg.res)
    return(mg.res)
    
  }
  
  
  objfun <- function(parms,
                     starting.values){
    phen.pamrs <- parms * starting.values
    ns <- paste0('.Simulations.Replacements.Soybean.Elli.', cultivar.name)
    
    if (phen.pamrs[6] <= phen.pamrs[5])
      return(NA)
    
    edit_apsimx_replacement(file = 'base-simulation.apsimx',
                            src.dir = './simulations/',
                            wrt.dir = './simulations/',
                            root = list("Models.Core.Folder", 1),
                            verbose = FALSE,
                            node.string  = ns,
                            parm = 'Vegetative',
                            value = phen.pamrs[1],
                            overwrite = TRUE)
    
    edit_apsimx_replacement(file = 'base-simulation.apsimx',
                            src.dir = './simulations/',
                            wrt.dir = './simulations/',
                            root = list("Models.Core.Folder", 1),
                            verbose = FALSE,
                            parm = 'EarlyFlowering',
                            node.string = ns, 
                            value = phen.pamrs[2],
                            overwrite = TRUE)
    
    edit_apsimx_replacement(file = 'base-simulation.apsimx',
                            src.dir = './simulations/',
                            wrt.dir = './simulations/',
                            parm = 'EarlyGrainFilling',
                            root = list("Models.Core.Folder", 1),
                            verbose = FALSE,
                            node.string = ns, 
                            value = phen.pamrs[3],
                            overwrite = TRUE)
    
    edit_apsimx_replacement(file = 'base-simulation.apsimx',
                            src.dir = './simulations/',
                            wrt.dir = './simulations/',
                            parm = 'LateGrainFilling',
                            root = list("Models.Core.Folder", 1),
                            verbose = FALSE,
                            node.string = ns, 
                            value = phen.pamrs[4],
                            overwrite = TRUE)
    
    edit_apsimx_replacement(file = 'base-simulation.apsimx',
                            src.dir = './simulations/',
                            wrt.dir = './simulations/',
                            root = list("Models.Core.Folder", 1),
                            parm = 'ReproductivePhotoperiodModifier',
                            verbose = FALSE,
                            node.string = ns, 
                            value = paste(phen.pamrs[5], phen.pamrs[6], sep = ', '),
                            overwrite = TRUE)
    
    
    mg.res <- run_mg(mdat)
    mg.res <- merge(phen.dict, 
                     mg.res,
                     by.x = 'apsim.phen',
                     by.y = 'Soybean.Phenology.CurrentStageName')
    
    
    
    comb <- merge(mdat,
                  mg.res,
                  by.x = c('id', 'phenology'),
                  by.y = c('id', 'fc.phen'))
    
    comb$doy.x <- as.numeric(strftime(comb$date, '%j'))
    comb$doy.y <- as.numeric(strftime(comb$Date, '%j'))
    
    p <- ggplot(comb)+
      geom_point(aes(x = doy.x, y = doy.y, fill = phenology), pch = 21)+
      geom_abline()+
      labs( x = 'Predicted DOY', y = 'Observed DOY')+
      theme_bw()+
      facet_wrap(~site)
    print(p)
    
    ## rss
    rss1 <- sum((comb$doy.x - comb$doy.y)^2, na.rm = TRUE)
    rss1 <- log(rss1)
    
    cat( strftime(Sys.time()), '- ', cultivar.name, ' - ', paste(round(phen.pamrs, 2), collapse = ', '), '- RSS = ', round(rss1, 2), '\n')
    
    return(rss1)
  }
  
  
  
  ivi <- which(apsim.mg.values$mg ==   round(mdat$mg[1], 0))
  initial.values <- apsim.mg.values[ivi, -1]
  
  op1 <- try(optim(par = rep(1, 6),
                   fn = objfun,
                   method = 'Nelder-Mead',
                   starting.values = unlist(initial.values),
                   control = list(maxit = 100)
  ))
  
  
}



