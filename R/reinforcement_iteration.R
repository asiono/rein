################################################################################
#' @title Reinforcement Iteration
#' @description This function build a reinforced grid after one time iteration.       
#' @param grid                 List, data frame containing information of grid to be optimized.
#' @param method               Character, optimization method, either conventional or OLTC or RPC or both OLTC and RPC.
#' @param desired_line_types   Character, list of available line types for replacement.
#' @param desired_trafo_types  Character, list of available transformer types for replacement.
#' @param solution_space       List, matrices of grid reinforcement options.
#' @param matrices             List, optimization matrices for linear solver.
#' @param grid_solved          List, SimTOOL container of reinforced grid data.
#' @return grid_solved
#' @keywords reinforcement, iteration
#' @author Andre Siono         andre.siono(at)ise.fraunhofer.de
################################################################################

reinforcement_iteration <- function(grid, method, avail_asset_types, U_set, iteration_count, oltc.trigger) {
  source('R/build_solution_space_update.R')
  source('R/create_resulting_grid.R')
  source('R/wrapper.prepare.grid.R')
  source('R/combine_solution_space.R')
  source('R/build_optimization_matrices_oltc.R')
  
  #load optimization function based on method selection
  if (method == 'conventional') {
    print("### GRID REINFORCEMENT WITH CONVENTIONAL METHOD ###")
  } else if (method == 'oltc') {
    print("### GRID REINFORCEMENT WITH INNOVATIVE TECHNOLOGY METHOD: OLTC ###")
    avail_asset_types$trafo <- c(avail_asset_types$trafo, "DOTEL_400_OLTC", "DOTEL_630_OLTC")
    avail_asset_types$trafo <- unique(avail_asset_types$trafo)
  } else if (method == 'rpc') {
    print("### GRID REINFORCEMENT WITH INNOVATIVE TECHNOLOGY METHOD: REACTIVE POWER CONTROL ###")
  } else if (method == 'oltc&rpc') {
    print("### GRID REINFORCEMENT WITH INNOVATIVE TECHNOLOGY METHOD: OLTC & REACTIVE POWER CONTROL ###")
    avail_asset_types$trafo <- c(avail_asset_types$trafo, "DOTEL_400_OLTC", "DOTEL_630_OLTC")
    avail_asset_types$trafo <- unique(avail_asset_types$trafo)
  } else
    stop("wrong method, should be either conventional, oltc, rpc, or oltc&rpc")
  
  #build possible solution for reinforcement
  solution_space <- build_solution_space_update(grid, avail_asset_types)
  
  #build matrices for linear optimization
  matrices <- build_optimization_matrices(solution_space, iteration_count, lines, oltc.trigger, verbose = 0)
  
  kappa(matrices$A)

  #run optimization with linear and integer programming
  result_lp <-  lp("min", objective.in = matrices$cost,
                   const.mat = matrices$A, const.dir =  matrices$const.dir,
                   const.rhs = matrices$b, all.bin = T)
  if (!any(as.logical(result_lp$solution))) stop("There is no possible solution found. 
                                                 Try to provide more available_asset_type with higher specification")
  
  #write chosen solution into solution matrice
  solution_space_combined <- combine_solution_space(solution_space = solution_space,
                                                    chosen_expansions = result_lp$solution)
  
  #clean envir data before recalculating grid
  ifrm <- function(x, env = globalenv()) {
    if (exists(x, envir = env)) {
      rm(list = x, envir = env)
    }
  }
  ifrm("voltage")
  ifrm("app_power")
  ifrm("current_pos")
  ifrm("current_neg")
  ifrm("lf_pos")
  ifrm("lf_neg")
  ifrm("trafo_types")
  
  #rebuild and recalculate grid with optimization result
  grid_solved <- create_resulting_grid(solution_space_combined = solution_space_combined, grid, verbose = 0)
  
  #rearrange grid data by sorting gird$lines and removing unnecessary variables
  grid_solved$lines <- grid_solved$lines[order(match(grid_solved$lines[,2], grid$lines[,2])), 
                                         !colnames(grid_solved$lines) %in% c("dU", "cost")]
  
  grid_solved <- wrapper.prepare.grid(grid_solved[c('grid_name', 'Nref', 'Vref', 'frequency', 
                                                    'power', 'coordinates', 'cal_node', 'lines', 'S_cal')],  
                                      check = F, solution_space_combined, U_set, oltc.trigger, verbose = 0)
  
  #define total cost
  grid_solved$total_cost <- result_lp$objval
  return(grid_solved)
  
}