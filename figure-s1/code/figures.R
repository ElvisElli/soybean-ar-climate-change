library(apsimx)
library(ggplot2)
library(patchwork)
library(usmap)
library(sf)

## map of locations and mgs ----
us <- us_map('states')
us <- st_transform(us, 4326)

locs <- readRDS(file = './processed-data/locs.rds')
locs <- aggregate(locs['mg'],
          locs[c('site', 'lon', 'lat')],
          function(x) paste(range(formatC(x, digits = 1, format = 'f')), collapse = ' - '))
locs <- st_as_sf(locs, 
                 coords = c('lon', 'lat'),
                 crs = 'epsg:4326')


states <- st_intersects(us, locs)
states <- sapply(states, function(x) length(x) > 0)
states <- us[states, ]


cols <- hcl.colors(3, 'RdYlBu')

map_with_states <- ggplot()+
  geom_sf(data = us)+
  geom_sf(data = states, fill = cols[2], col = 'black')+
  theme_void()+
  theme(panel.background = element_rect(color = 'black', 
                                        fill = 'transparent'))

map_with_points <- ggplot()+
  geom_sf(data = states, col = 'black', fill = cols[2])+
  geom_sf(data = locs, cex = 3, fill = '#A51122', pch = 21)+
  #labs(fill = "Maturity group range", shape = "Maturity group range")+
  theme_void(base_size = 16)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = cols[c(1, 3)])+
  theme(legend.position = 'bottom',
        legend.title.position  = 'top')


final_map <- map_with_points + 
  labs(tag = 'A')+
  inset_element(map_with_states,
                left = 0,
                right = 0.5,
                bottom = 0.73,
                top = 1) 
png('./figures/01-map.png',
    width = 6*300, height = 5*300, res = 300)  
print(final_map)
dev.off()

## one to ones

cal.phen <- readRDS('./processed-data/calibrated-phen.rds')
uncal.phen <- readRDS('./processed-data/uncalibrated-phen.rds')

cal.yield <- readRDS('./processed-data/calibrated-yield.rds')
cal.yield <- cal.yield[order(cal.yield$SoybeanYieldkgha), ]
cal.yield <- cal.yield[-1*(1:4),]

uncal.yield <- readRDS('./processed-data/uncalibrated-yield.rds')



bias.cal <- sum(cal.phen$doy.x - cal.phen$doy.y)/nrow(cal.phen)
rmse.cal <- sqrt(sum((cal.phen$doy.x - cal.phen$doy.y)^2 )/nrow(cal.phen))
rmse.pre <- sqrt(sum((uncal.phen$doy.x - uncal.phen$doy.y)^2 )/nrow(uncal.phen))
bias.pre <- sum(uncal.phen$doy.x - uncal.phen$doy.y)/nrow(uncal.phen)
cal.label <- paste0('Optimized\nRMSE: ', round(rmse.cal, 1), ' d\nBias: ', round(bias.cal, 1),' d')
pre.label <- paste0('Uncalibrated\nRMSE: ', round(rmse.pre, 1), ' d\nBias: ', round(bias.pre, 1), ' d')


ybias.cal <- sum(cal.yield$yield - cal.yield$SoybeanYieldkgha)/nrow(cal.yield)
yrmse.cal <- sqrt(sum((cal.yield$yield - cal.yield$SoybeanYieldkgha)^2 )/nrow(cal.yield))
yrmse.pre <- sqrt(sum((uncal.yield$yield - uncal.yield$SoybeanYieldkgha)^2 )/nrow(uncal.yield))
ybias.pre <- sum(uncal.yield$yield - uncal.yield$SoybeanYieldkgha)/nrow(uncal.yield)

ycal.label <- paste0('Optimized\nRMSE: ', round(yrmse.cal, 1), ' kg/ha\nBias: ', round(ybias.cal, 1),' kg/ha')
ypre.label <- paste0('Uncalibrated\nRMSE: ', round(yrmse.pre, 1), ' kg/ha\nBias: ', round(ybias.pre, 1), ' kg/ha')



## doy 
p <- ggplot()+
  
  geom_point(data = uncal.phen, aes(x = doy.x, y = doy.y, fill = 'Uncalibrated', , pch = 'Uncalibrated', colour = 'Uncalibrated'))+
  geom_point(data = cal.phen, aes(x = doy.x, y = doy.y, fill = 'Optimized', pch = 'Optimized', colour = 'Optimized'))+
  geom_text(aes(x = Inf, y = -Inf, label = cal.label),
            vjust = -0.1, hjust = 1.1)+
  geom_text(aes(x = -Inf, y = Inf, label = pre.label),
            vjust = 1, hjust = 0)+
  geom_abline()+
  labs( x = 'Predicted DOY', y = 'Observed DOY', 
        fill = '',
        col = '',
        shape = '',
        tag = 'B')+
  scale_color_manual(values = c('Optimized' =alpha('#1f77b4', 1), 
                               'Uncalibrated' = alpha('#ff7f0e', 1)))+
  scale_fill_manual(values = c('Optimized' =alpha('#1f77b4', 0.3), 
                               'Uncalibrated' = alpha('#ff7f0e', 0.3)))+
  scale_shape_manual(values = c('Optimized' = 21, 
                                'Uncalibrated'= 24))+
  theme_bw(base_size = 14)+
  theme(legend.position  = 'bottom',
        legend.title.position = 'top',
        axis.text = element_text(colour = 'black'))

## yield

p2 <- ggplot()+
  geom_point(data =uncal.yield, aes(x = yield, y = SoybeanYieldkgha, fill = 'Uncalibrated', colour = 'Uncalibrated', shape = 'Uncalibrated'),  cex = 2)+
  geom_point(data = cal.yield, aes(x = yield, y = SoybeanYieldkgha, fill = 'Optimized', colour = 'Optimized', shape = 'Optimized'),  cex = 2)+
  geom_text(aes(x = Inf, y = -Inf, label = ycal.label),
            vjust = -0.1, hjust = 1.1)+
  geom_text(aes(x = -Inf, y = Inf, label = ypre.label),
            vjust = 1, hjust = 0)+
  geom_abline()+
  labs( x = 'Predicted Yield (kg/ha)', y = 'Observed Yield (kg/ha)', 
        col = '',
        shape = '',
        fill = '',
        tag = 'C')+
  guides(fill = 'none', colour = 'none', shape = 'none')+
  coord_cartesian(xlim = c(0, 8000),
                  ylim = c(0, 8000))+
  scale_color_manual(values = c('Optimized' =alpha('#1f77b4', 1), 
                               'Uncalibrated' = alpha('#ff7f0e', 1)))+
  scale_fill_manual(values = c('Optimized' =alpha('#1f77b4', 0.3), 
                               'Uncalibrated' = alpha('#ff7f0e', 0.3)))+
  scale_shape_manual(values = c('Optimized' = 21, 
                                'Uncalibrated'= 24))+
 theme_bw(base_size = 14)+
  theme(legend.position  = 'bottom',
        legend.title.position = 'top',
        axis.text = element_text(colour = 'black'))
 p + p2 

## joining both
 
 png('./figures/supp-figure.png',
     width = 8*300, height = 8*300, res = 300)  
 final_map + p + p2  +
   plot_layout(design = '
  12
              13'
   ) &
   theme(legend.position = 'top')
 dev.off()


  ## days after planting
 
 cal.phen$dap.obs <- as.numeric(cal.phen$date - cal.phen$pd)
 uncal.phen$dap.obs <- as.numeric(uncal.phen$date - uncal.phen$pd)
 
 cal.phen$dap.sim <- as.numeric(cal.phen$Date - cal.phen$pd)
 uncal.phen$dap.sim <- as.numeric(uncal.phen$Date - uncal.phen$pd)

 ## --- Outlier filtering for DAP plot ---
 ## Three clusters visible in the annotated figure are excluded:
 ##   (1) Bottom cluster  : dap.obs < 30 — near-zero observed DAP, indicating
 ##       erroneous or missing planting-date records in the source data.
 ##   (2) Top-left cluster: obs ~150 d but sim ~70 d  \  both captured by
 ##   (3) Mid-right cluster: sim ~130 d but obs ~70 d /  |error| > 50 d
 ## The 50 d threshold retains biologically plausible scatter while removing
 ## cases that almost certainly reflect data-quality issues.
 cal.phen.filt   <- cal.phen[   cal.phen$dap.obs   >= 30 & abs(cal.phen$dap.obs   - cal.phen$dap.sim)   <= 50, ]
 uncal.phen.filt <- uncal.phen[ uncal.phen$dap.obs >= 30 & abs(uncal.phen$dap.obs - uncal.phen$dap.sim) <= 50, ]

 ## Recompute RMSE/Bias on the filtered DAP data for the p3 panel label
 dap.bias.cal <- sum(cal.phen.filt$dap.obs   - cal.phen.filt$dap.sim)   / nrow(cal.phen.filt)
 dap.rmse.cal <- sqrt(sum((cal.phen.filt$dap.obs   - cal.phen.filt$dap.sim)^2)   / nrow(cal.phen.filt))
 dap.rmse.pre <- sqrt(sum((uncal.phen.filt$dap.obs - uncal.phen.filt$dap.sim)^2) / nrow(uncal.phen.filt))
 dap.bias.pre <- sum(uncal.phen.filt$dap.obs - uncal.phen.filt$dap.sim) / nrow(uncal.phen.filt)
 dap.cal.label <- paste0('Optimized\nRMSE: ',    round(dap.rmse.cal, 1), ' d\nBias: ', round(dap.bias.cal, 1), ' d')
 dap.pre.label <- paste0('Uncalibrated\nRMSE: ', round(dap.rmse.pre, 1), ' d\nBias: ', round(dap.bias.pre, 1), ' d')
 ## --- end outlier filtering ---

p3 <- ggplot()+
  geom_point(data = uncal.phen.filt, aes(x = dap.sim, y = dap.obs, fill = 'Uncalibrated', , pch = 'Uncalibrated', colour = 'Uncalibrated'))+
  geom_point(data = cal.phen.filt, aes(x = dap.sim, y = dap.obs, fill = 'Optimized', pch = 'Optimized', colour = 'Optimized'))+
  geom_text(aes(x = Inf, y = -Inf, label = dap.cal.label),
            vjust = -0.1, hjust = 1.1)+
  geom_text(aes(x = -Inf, y = Inf, label = dap.pre.label),
            vjust = 1, hjust = 0)+
  geom_abline()+
  scale_y_continuous(limits = c(0,200))+
  scale_x_continuous(limits = c(0,200))+
  labs( x = 'Predicted days after planting', y = 'Observed days after planting', 
        fill = '',
        col = '',
        shape = '',
        tag = 'B')+
  scale_color_manual(values = c('Optimized' =alpha('#1f77b4', 1), 
                               'Uncalibrated' = alpha('#ff7f0e', 1)))+
  scale_fill_manual(values = c('Optimized' =alpha('#1f77b4', 0.3), 
                               'Uncalibrated' = alpha('#ff7f0e', 0.3)))+
  scale_shape_manual(values = c('Optimized' = 21, 
                                'Uncalibrated'= 24))+
  theme_bw(base_size = 14)+
  theme(legend.position  = 'bottom',
        legend.title.position = 'top',
        axis.text = element_text(colour = 'black'))

 png('./figures/supp-figure-v2.png',
     width = 8*300, height = 8*300, res = 300)  
 final_map + p3 + p2  +
   plot_layout(design = '
  12
              13'
   ) &
   theme(legend.position = 'top')
 dev.off()