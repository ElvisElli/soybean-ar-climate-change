## weighted mean for Yield_kgha
Yield_kgha <- lapply(soil.sims, function(df) df[c('Date', 'Yield_kgha')])
Yield_kgha <- Reduce(function(x, y) merge(x, y, by = 'Date'), Yield_kgha)
dates <-  Yield_kgha$Date
Yield_kgha <- matrix(unlist(Yield_kgha[-1]), ncol = length(soil.sims))
Yield_kgha <- apply(Yield_kgha, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for biomass_kgha
biomass_kgha <- lapply(soil.sims, function(df) df[c('Date', 'biomass_kgha')])
biomass_kgha <- Reduce(function(x, y) merge(x, y, by = 'Date'), biomass_kgha)
dates <-  biomass_kgha$Date
biomass_kgha <- matrix(unlist(biomass_kgha[-1]), ncol = length(soil.sims))
biomass_kgha <- apply(biomass_kgha, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for SeasonRain
SeasonRain <- lapply(soil.sims, function(df) df[c('Date', 'SeasonRain')])
SeasonRain <- Reduce(function(x, y) merge(x, y, by = 'Date'), SeasonRain)
dates <-  SeasonRain$Date
SeasonRain <- matrix(unlist(SeasonRain[-1]), ncol = length(soil.sims))
SeasonRain <- apply(SeasonRain, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for SeasonRadn
SeasonRadn <- lapply(soil.sims, function(df) df[c('Date', 'SeasonRadn')])
SeasonRadn <- Reduce(function(x, y) merge(x, y, by = 'Date'), SeasonRadn)
dates <-  SeasonRadn$Date
SeasonRadn <- matrix(unlist(SeasonRadn[-1]), ncol = length(soil.sims))
SeasonRadn <- apply(SeasonRadn, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for SeasonMaxt
SeasonMaxt <- lapply(soil.sims, function(df) df[c('Date', 'SeasonMaxt')])
SeasonMaxt <- Reduce(function(x, y) merge(x, y, by = 'Date'), SeasonMaxt)
dates <-  SeasonMaxt$Date
SeasonMaxt <- matrix(unlist(SeasonMaxt[-1]), ncol = length(soil.sims))
SeasonMaxt <- apply(SeasonMaxt, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for SeasonMint
SeasonMint <- lapply(soil.sims, function(df) df[c('Date', 'SeasonMint')])
SeasonMint <- Reduce(function(x, y) merge(x, y, by = 'Date'), SeasonMint)
dates <-  SeasonMint$Date
SeasonMint <- matrix(unlist(SeasonMint[-1]), ncol = length(soil.sims))
SeasonMint <- apply(SeasonMint, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for SeasonMeanT
SeasonMeanT <- lapply(soil.sims, function(df) df[c('Date', 'SeasonMeanT')])
SeasonMeanT <- Reduce(function(x, y) merge(x, y, by = 'Date'), SeasonMeanT)
dates <-  SeasonMeanT$Date
SeasonMeanT <- matrix(unlist(SeasonMeanT[-1]), ncol = length(soil.sims))
SeasonMeanT <- apply(SeasonMeanT, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for BloomingSeasonMaxt
BloomingSeasonMaxt <- lapply(soil.sims, function(df) df[c('Date', 'BloomingSeasonMaxt')])
BloomingSeasonMaxt <- Reduce(function(x, y) merge(x, y, by = 'Date'), BloomingSeasonMaxt)
dates <-  BloomingSeasonMaxt$Date
BloomingSeasonMaxt <- matrix(unlist(BloomingSeasonMaxt[-1]), ncol = length(soil.sims))
BloomingSeasonMaxt <- apply(BloomingSeasonMaxt, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for Silking_Supply_Demand_Ratio
Silking_Supply_Demand_Ratio <- lapply(soil.sims, function(df) df[c('Date', 'Silking_Supply_Demand_Ratio')])
Silking_Supply_Demand_Ratio <- Reduce(function(x, y) merge(x, y, by = 'Date'), Silking_Supply_Demand_Ratio)
dates <-  Silking_Supply_Demand_Ratio$Date
Silking_Supply_Demand_Ratio <- matrix(unlist(Silking_Supply_Demand_Ratio[-1]), ncol = length(soil.sims))
Silking_Supply_Demand_Ratio <- apply(Silking_Supply_Demand_Ratio, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for temperature impacts on RUE
Silking_RUE_Temp <- lapply(soil.sims, function(df) df[c('Date', 'Silking_RUE_Temp')])
Silking_RUE_Temp <- Reduce(function(x, y) merge(x, y, by = 'Date'), Silking_RUE_Temp)
dates <-  Silking_RUE_Temp$Date
Silking_RUE_Temp <- matrix(unlist(Silking_RUE_Temp[-1]), ncol = length(soil.sims))
Silking_RUE_Temp <- apply(Silking_RUE_Temp, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for MaturityDAS
MaturityDAS <- lapply(soil.sims, function(df) df[c('Date', 'MaturityDAS')])
MaturityDAS <- Reduce(function(x, y) merge(x, y, by = 'Date'), MaturityDAS)
dates <-  MaturityDAS$Date
MaturityDAS <- matrix(unlist(MaturityDAS[-1]), ncol = length(soil.sims))
MaturityDAS <- apply(MaturityDAS, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for EmergenceDAS
EmergenceDAS <- lapply(soil.sims, function(df) df[c('Date', 'EmergenceDAS')])
EmergenceDAS <- Reduce(function(x, y) merge(x, y, by = 'Date'), EmergenceDAS)
dates <-  EmergenceDAS$Date
EmergenceDAS <- matrix(unlist(EmergenceDAS[-1]), ncol = length(soil.sims))
EmergenceDAS <- apply(EmergenceDAS, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for FloweringDAS
FloweringDAS <- lapply(soil.sims, function(df) df[c('Date', 'FloweringDAS')])
FloweringDAS <- Reduce(function(x, y) merge(x, y, by = 'Date'), FloweringDAS)
dates <-  FloweringDAS$Date
FloweringDAS <- matrix(unlist(FloweringDAS[-1]), ncol = length(soil.sims))
FloweringDAS <- apply(FloweringDAS, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for SeedFillingDAS
SeedFillingDAS <- lapply(soil.sims, function(df) df[c('Date', 'SeedFillingDAS')])
SeedFillingDAS <- Reduce(function(x, y) merge(x, y, by = 'Date'), SeedFillingDAS)
dates <-  SeedFillingDAS$Date
SeedFillingDAS <- matrix(unlist(SeedFillingDAS[-1]), ncol = length(soil.sims))
SeedFillingDAS <- apply(SeedFillingDAS, 1, function(br) weighted.mean(br, soil.fractions))

## weighted mean for CumRadiationInterceptionOnGreen
CumRadiationInterceptionOnGreen <- lapply(soil.sims, function(df) df[c('Date', 'CumRadiationInterceptionOnGreen')])
CumRadiationInterceptionOnGreen <- Reduce(function(x, y) merge(x, y, by = 'Date'), CumRadiationInterceptionOnGreen)
dates <-  CumRadiationInterceptionOnGreen$Date
CumRadiationInterceptionOnGreen <- matrix(unlist(CumRadiationInterceptionOnGreen[-1]), ncol = length(soil.sims))
CumRadiationInterceptionOnGreen <- apply(CumRadiationInterceptionOnGreen, 1, function(br) weighted.mean(br, soil.fractions))
