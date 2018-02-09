################################################################################
#' @title Reinforcement Iteration
#' @description build a reinforced grid based on available assets       
#' @param grid                 List containing information of grid to be optimized.
#' @param method               optimization method, either conventional or OLTC or RPC or both OLTC and RPC.
#' @param avail_asset_types    List of available line and trafo types for replacement
#' @param U_set                Grid's lower voltage level 
#' @param iteration_count      counter for reinforcement iteration.
#' @param oltc.trigger         indication for OLTC transformator usage.
#' @return grid_solved
#' @importFrom lpSolve lp
################################################################################

reinforcement_iteration <- function(grid, method, avail_asset_types, U_set, iteration_count, oltc.trigger) {

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
  solution_space <- build_solution_space(grid, avail_asset_types)
  
  #build matrices for linear optimization
  matrices <- build_optimization_matrices(solution_space, iteration_count, lines, oltc.trigger, verbose = 0)
  kappa(matrices$A)

  #run optimization with linear and integer programming
  result_lp <- lp("min", objective.in = matrices$cost,
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
  
  #rebuild and recalculate grid with optimization result
  grid_solved <- create_resulting_grid(solution_space_combined = solution_space_combined, grid, verbose = 0)
  
  #rearrange grid data by sorting gird$lines and removing unnecessary variables
  grid_solved$lines <- grid_solved$lines[order(match(grid_solved$lines[,2], grid$lines[,2])), 
                                         !colnames(grid_solved$lines) %in% c("dU", "cost")]
  if (any(grepl('OLTC', grid_solved$lines$model))) oltc.trigger <- T
  
  grid_solved <- wrapper.prepare.grid(grid_solved[c('grid_name', 'Nref', 'Vref', 'frequency', 
                                                    'power', 'coordinates', 'cal_node', 'lines', 'S_cal')],  
                                      check = F, U_set, oltc.trigger, verbose = 0)
  
  #define total cost
  grid_solved$total_cost <- result_lp$objval
  return(grid_solved)
  
}