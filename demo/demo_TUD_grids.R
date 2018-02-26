rm(list=ls())
library(rein)
library(sp)
load(file = '~/Documents/simtool/simtool/SimTOOL/data/TUD_grids.RData')
grid <- TUD_grids[[6]]
rm(TUD_grids)
load("/home/asiono/Documents/shiny_rein/AndresGrid.RData")
grid$power$type[grid$power$type == 'BB'] <- 'PQ'
grid$Vref <- 20000

for (i in 1:nrow(grid$lines)) {
  if (grepl('trafo', grid$lines$element[i])) grid$lines[i, 'type'] <- 'trafo'
  if (grepl('line', grid$lines$element[i])) grid$lines[i, 'type'] <- 'line'
}

grid$S_cal[1] <- 7000*2
grid$S_cal[2] <- 7000*1.54
grid$S_cal[3] <- 7000*2
grid$S_cal[14] <- 7000*3
grid$S_cal[15] <- 7000*0.8
grid$S_cal[16] <- 7000*2
grid$S_cal[17] <- 7000*1
grid$S_cal[18] <- 7000*1
grid$S_cal[9] <- 7000*4
grid$S_cal[10] <- 7000*0.8
grid$S_cal[11] <- 7000*0.6
grid$S_cal[12] <- 7000*1.54
grid$S_cal[13] <- 7000*0.5

rein::reinforcement(grid, reinforcement_method = 'rpc', avail_asset_types = list(line = c("NAYY4x150", "2xNAYY4x150", "2xNAYY4x240", "4xNAYY4x240"), 
                                                                                  trafo = c("DOTEL_2000","DOTEL_1600")), verbose = 0)
