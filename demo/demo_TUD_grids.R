rm(list=ls())
load(file = '~/Documents/simtool/simtool/SimTOOL/data/TUD_grids.RData')
source('~/Documents/rein_lp/rein/R/reinforcement.R')
grid <- TUD_grids[[6]]
rm(TUD_grids)
grid$power$type[grid$power$type == 'BB'] <- 'PQ'
grid$Vref <- 20000

grid$S_cal[1] <- 7000
grid$S_cal[2] <- 7000*1.54
grid$S_cal[3] <- 7000*2
grid$S_cal[4] <- 7000*3
grid$S_cal[5] <- 7000*0.8
grid$S_cal[6] <- 7000*1.4
grid$S_cal[7] <- 7000*1
grid$S_cal[8] <- 7000*1
grid$S_cal[9] <- 7000*2
grid$S_cal[10] <- 7000*0.8
grid$S_cal[11] <- 7000*0.6
grid$S_cal[12] <- 7000*1.54
grid$S_cal[13] <- 7000*0.5

reinforcement(grid, reinforcement_method = 'conventional', verbose = 0)