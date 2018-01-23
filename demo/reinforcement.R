######################################################################################################################
#' @title Reinforcement
#' @description This is the main function of ReIn program for grid reinforcement simulation.
#' @param grid                    List, SimTOOL container of initial grid data.
#' @param reinforcement_method    Character, reinforcement method to be applied.
#' @param desired_line_types      Character, list of available line types for replacement.
#' @param desired_trafo_types     Character, list of available transformer types for replacement.
#' @param iteration               Double, counter of reinforcement iteration.
#' @param iteration_limit         Double, limit of reinforcement iteration.
#' @param cost                    Double, total cost of reinforcement.
#' @param grid_solved             List, SimTOOL container of reinforced grid data.
#' @param oltc.trigger            Logical, indicator if OLTC is used in the grid (TRUE when OLTC is present).
#' @param U_set                   Double, secondary voltage, it is stored because OLTC will change tranformer's ratio.
#' @return {reinforcement cost, grid graph before reinforcement, grid graph after reinforcement}
#' @seealso \code{\link{reinforcement_iteration}}
#' @author Andre Siono      andre.siono(at)ise.fraunhofer.de
######################################################################################################################

rm(list = ls()) 
library(SimTOOL)
library(reIn)
library(Matrix)
library(data.table)
options(stringsAsFactors = FALSE)
setwd('~/Documents/rein_lp/rein/')
starttime <- Sys.time()

# 0. Loading necessary database
# 0.1 loading necessary functions
  source('demo/reinforcement_iteration.R')
  source('R/plot_grid_rein.R')
  source('R/reactive_power_control.R')
  source('R/wrapper.prepare.grid.R')

# 0.2 load line types and trafo types database and put them as lazy load index (.rdb/.rdx)
 types = local({load(file = 'data/types.RData'); environment()})
 tools:::makeLazyLoadDB(types, "types")

# 1. Input data
 
# 1.1 Load grid that was built in SimTOOL (see build_grid.R)
 grid <- get(load(file = '/home/asiono/Documents/test/AndresGrid.RData'))
 load(file = '~/Documents/simtool/simtool/SimTOOL/data/TUD_grids.RData')
 grid <- TUD_grids[[6]]
 
# 1.2 Load household profile if available
 
# 1.3 Insert Distributed Generator's apparent power (in Watt single-phase)
 grid$S_cal[3] <- 20000
 grid$S_cal[4] <- 12000
 grid$S_cal[6] <- 20000
 grid$S_cal[12] <- 10000
 grid$S_cal[8] <- 7000
 grid$S_cal[9] <- 5000
 grid$S_cal[14] <- 7000
 grid$S_cal[16] <- 20000
 grid$S_cal[17] <- 2000
 grid$S_cal[11] <- 5000
 grid$S_cal[7] <- 7000
 grid$S_cal[5] <- 10000

# 1.3 Choose reinfrocement method from either: conventional, oltc, rpc, or oltc&rpc
  reinforcement_method <- 'conventional'
  
# 1.4 Define desired line and trafo types
  avail_asset_types <- list(line = c("NAYY4x150", "2xNAYY4x240", "2xNAYY4x150", "4xNAYY4x240", "8xNAYY4x240"),
  trafo = c("DOTEL_630", "DOTEL_1000", "DOTEL_1250", "DOTEL_800", "DOTEL_1600"))
  #add original line and trafo into database if not yet available
  if (!all(grid$lines$model[which(grid$lines$type == 'line')] %in% avail_asset_types$line)) {
    avail_asset_types$line <- c(avail_asset_types$line, unique(grid$lines$model[which(grid$lines$type == 'line')]))
  }
  if (all(grid$lines$model[which(grid$lines$type == 'trafo')] %in% avail_asset_types$trafo)) {
    avail_asset_types$trafo <- c(avail_asset_types$trafo, unique(grid$lines$model[which(grid$lines$type == 'trafo')]))
  }
  
# 2. Optimization Process
# 2.1 Adjusting feed-in power for reactive power control (if rpc mode is called)
  if (reinforcement_method == 'rpc' | reinforcement_method == 'oltc&rpc') {
    grid$S_cal <- reactive_power_control(grid$lines, grid$S_cal)
  }

# 2.2 Calculate initial power flow
  grid <- wrapper.prepare.grid(grid, check = T, verbose = 0)

# 2.3 Reinforcement iteration
  iteration_count <- 1          #number of iteration is reset to 1
  cost <- 0                #cost is reset to 0
  grid_solved <- grid     #reinforced grid is called grid_solved and original grid is grid
  oltc.trigger <- F       
  iteration_limit <- 6    
  #because OLTC will change tranformer's ratio, initial ratio is stored
  U_set <- grid$lines[which(grid$lines$type == 'trafo'), "trafo_U2"]
  repeat {
    message(sprintf("\nCurrent iteration no: %s", iteration_count))
    grid_solved <- reinforcement_iteration(grid_solved, method = reinforcement_method, avail_asset_types, U_set, iteration_count)
    cost <- cost + grid_solved[[length(grid_solved)]]
    grid_solved <- grid_solved[-(length(grid_solved))]

    if (any(grepl('OLTC', grid_solved$lines$model))) {
      #minimum iteration for OLTC mode is 2 to ensure that new secondary voltage are adopted in grid calculation
      if (all(abs(Mod(grid_solved$U_res[-1])/(grid$lines[which(grid$lines$type == 'trafo'), "trafo_U2"]) - 1) < 0.08) 
          & iteration_count > 1 | iteration_count > iteration_limit & iteration_count > 1) {
          break
          }
    } else if (all(abs(Mod(grid_solved$U_res[-1])/(grid$lines[which(grid$lines$type == 'trafo'), "trafo_U2"]) - 1) < 0.03) 
               | iteration_count > iteration_limit) {
          break
    }
    iteration_count <- iteration_count + 1
  }

# 2.4 Plot grid before and after reinforcement
  endtime <- Sys.time()
  totaltime <- endtime - starttime
  print(totaltime)
#plot_grid_rein(grid, U_set = U_set)
plot_grid_rein(grid_solved, U_set = U_set)

