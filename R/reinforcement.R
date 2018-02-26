######################################################################################################################
#' @title Reinforcement
#' @description Simulate grid reinforcement and choose the optimum measures given the method, load and available assets data
#' @param grid   List containing initial grid data.
#' @param reinforcement_method   Reinforcement method to be applied.
#' @param avail_asset_types    List of available line and trafo types for replacement, default value line = "NAYY4x150", "2xNAYY4x240", "2xNAYY4x150", "4xNAYY4x240" 
#' trafo = "DOTEL_630", "DOTEL_1000".
#' @param verbose   Verbosity level. value greater than zero to display step by step of reinforcement
#' @return grid_solved
#' @export
#' @import plyr Matrix tools igraph slam
#' @importFrom SimTOOL create.admittance create.power solve.LF
#' @importFrom data.table as.data.table setnames
#' @importFrom reshape2 melt
#######################################################################################################################

reinforcement <- function(grid, reinforcement_method, 
                          avail_asset_types = list(line = c("NAYY4x150", "2xNAYY4x240", "2xNAYY4x150", "4xNAYY4x240"), 
                                                   trafo = c("DOTEL_630", "DOTEL_1000")), verbose = 0) {
starttime <- Sys.time()

# 1. Loading neccesarry datas
    # Assign reinforcement method
      reinforcement_method <- reinforcement_method
      
    # add original line and trafo into database if not yet available
      if (!all(grid$lines$model[which(grid$lines$type == 'line')] %in% avail_asset_types$line)) {
        avail_asset_types$line <- c(avail_asset_types$line, unique(grid$lines$model[which(grid$lines$type == 'line')]))
      }
      if (all(grid$lines$model[which(grid$lines$type == 'trafo')] %in% avail_asset_types$trafo)) {
        avail_asset_types$trafo <- c(avail_asset_types$trafo, unique(grid$lines$model[which(grid$lines$type == 'trafo')]))
      }
  
# 2. Optimization Process
    # Adjusting feed-in power for reactive power control (if rpc mode is called)
      if (reinforcement_method == 'rpc' | reinforcement_method == 'oltc&rpc') {
        grid$S_cal <- reactive_power_control(grid$lines, grid$S_cal)
      }
      
    # Calculate initial load flow
      grid <- wrapper.prepare.grid(grid, check = T, verbose = 0)
      
    # Reinforcement iteration
      iteration_count <- 1          #number of iteration is reset to 1
      cost <- 0                #cost is reset to 0
      grid_solved <- grid     #reinforced grid is called grid_solved and original grid is grid
      oltc.trigger <- F       #trigger when OLTC transformer is apllied
      iteration_limit <- 3    
      #because OLTC will change tranformer's ratio, initial ratio is stored
      U_set <- grid$lines[which(grid$lines$type == 'trafo'), "trafo_U2"]
      repeat {
        message(sprintf("\nCurrent iteration no: %s", iteration_count))
        grid_solved <- reinforcement_iteration(grid_solved, method = reinforcement_method, 
                                                     avail_asset_types, U_set, iteration_count, oltc.trigger)
        cost <- cost + grid_solved[[length(grid_solved)]]
        grid_solved <- grid_solved[-(length(grid_solved))]
        
        if (any(grepl('OLTC', grid_solved$lines$model))) {
          oltc.trigger <- T    
          if (all(abs(Mod(grid_solved$U_res[-1])/(grid$lines[which(grid$lines$type == 'trafo'), "trafo_U2"]) - 1) < 0.08) 
              | iteration_count > iteration_limit) {
              break
              }
        } else if (all(abs(Mod(grid_solved$U_res[-1])/(grid$lines[which(grid$lines$type == 'trafo'), 
                                                                  "trafo_U2"]) - 1) < 0.03) 
                   | iteration_count > iteration_limit) {
              break
        }
        iteration_count <- iteration_count + 1
      }

# 3. Displaying the optimization result
    # Plot grid before and after reinforcement
      #plot_grid_rein(grid, U_set = U_set)
      #plot_grid_rein(grid_solved, U_set = U_set)
    # Calculate optimization time
      endtime <- Sys.time()
      totaltime <- round(difftime(endtime, starttime, units = "secs"), digits = 2)
      message("Grid reinforcement optimization were done within ", totaltime, " seconds")
      return(grid_solved)
}

