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

reinforcement_iteration <- function(grid, method, avail_asset_types, U_set, iteration_count) {
  
  source('R/build_solution_space_update.R')
  source('R/create_resulting_grid.R')
  source('R/build_optimization_matrices_oltc.R')
  source('~/Documents/simtool/simtool/SimTOOL/R/solve.LF.R')
  
  #load optimization function based on method selection
  if (method == 'conventional') {
    print("### GRID REINFORCEMENT WITH CONVENTIONAL METHOD ###")
  } else if (method == 'oltc') {
    print("### GRID REINFORCEMENT WITH INNOVATIVE TECHNOLOGY METHOD: OLTC ###")
    avail_asset_types$trafo <- c(avail_asset_types$trafo, "DOTEL_400_OLTC", 
                             "DOTEL_630_OLTC", "DOTEL_800_OLTC", "DOTEL_1250_OLTC", "DOTEL_1000_OLTC")
    avail_asset_types$trafo <- unique(avail_asset_types$trafo)
  } else if (method == 'rpc') {
    print("### GRID REINFORCEMENT WITH INNOVATIVE TECHNOLOGY METHOD: REACTIVE POWER CONTROL ###")
  } else if (method == 'oltc&rpc') {
    print("### GRID REINFORCEMENT WITH INNOVATIVE TECHNOLOGY METHOD: OLTC & REACTIVE POWER CONTROL ###")
    avail_asset_types$trafo <- c(avail_asset_types$trafo, "DOTEL_400_OLTC", 
                             "DOTEL_630_OLTC", "DOTEL_800_OLTC", "DOTEL_1250_OLTC", "DOTEL_1000_OLTC")
    avail_asset_types$trafo <- unique(avail_asset_types$trafo)
  } else
    stop("wrong method, should be either conventional, oltc, rpc, or oltc&rpc")
  
  #build possible solution for reinforcement
  solution_space <- build_solution_space_update(grid, avail_asset_types)
  
  #build matrices for linear optimization
  matrices <- build_optimization_matrices(solution_space = solution_space, iteration_count, lines = grid$lines, verbose = 5 )
  kappa(matrices$A)

  #run optimization with linear and integer programming
  result_lp <-  lp("min", objective.in = matrices$cost,
                   const.mat = matrices$A, const.dir =  matrices$const.dir,
                   const.rhs = matrices$b, all.bin = T)
  
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

  if (any(grepl('OLTC', grid_solved$lines$model))) {
    assign('oltc.trigger', T, envir = .GlobalEnv)
    #create controller list
    grid_solved$ctr <- list()
    
    #define controller entry
    grid_solved$ctr[[1]] <- list()
    grid_solved$ctr[[1]]$mode <- "OLTC"
    #connection nodes
    grid_solved$ctr[[1]]$hv_node <- solution_space_combined[1, 'begin']
    grid_solved$ctr[[1]]$lv_node <- solution_space_combined[1, 'end']
    if (oltc_control_method == 'OLTC1') {
      grid_solved$ctr[[1]]$ctr_node <- grid$lines$end[which(grid$lines$type == 'trafo')]
      } else if (oltc_control_method == 'OLTC2') {
        grid_solved$ctr[[1]]$ctr_node <- grid$lines_cal[which(!grid$lines$end %in% grid$lines$begin),]
        grid_solved$ctr[[1]]$ctr_node <- grid_solved$ctr[[1]]$ctr_node[which.min(grid_solved$ctr[[1]]$ctr_node$I), 'end']
      } else if (oltc_control_method == 'OLTC3') {
        grid_solved$ctr[[1]]$ctr_node <- grid$lines$end[which(!grid$lines$end %in% grid$lines$begin)]
      } else if (oltc_control_method == 'OLTC4') {
        grid_solved$ctr[[1]]$ctr_node <- names(grid$S_cal[which(!grid$S_cal == 0)])
      } else {
        stop('Please insert correct OLTC control method')
      }
    #tap settings
    grid_solved$ctr[[1]]$pos_taps <- 6 #voltage up regulation
    grid_solved$ctr[[1]]$neg_taps <- 6 #voltage down regulation
    # pos_taps+neg taps + 1(0-tap) < [5,7,9](http://www.reinhausen.com/de/desktopdefault.aspx/tabid-1605/1835_read-4652/)
    grid_solved$ctr[[1]]$curr_tap <- 0  
    grid_solved$ctr[[1]]$tap_size <- 1.5 #percentual of U_set  usual [1,5%, 2% or 2,5%]
    #[0.8 ... 2.5%]according to: On-Load Tap-Changers for Power Transformers A Technical Digest, MR Publication
    #lead voltage
    grid_solved$ctr[[1]]$U_set <- U_set
    grid_solved$ctr[[1]]$deadband <- 0.6 #percentual of U_set 0.6
    grid_solved$ctr[[1]]$U_min <- U_set*(1 - 0.08)
    grid_solved$ctr[[1]]$U_max <- U_set*(1 + 0.08)
    grid_solved$ctr[[1]]$verbose <- 2
    grid_solved <- solve.LF(grid = grid_solved, meth = "G", ctr = c("OLTC"), warm = F, verbose = 0)
  } else {
    grid_solved <- solve.LF(grid = grid_solved, warm = F, verbose = 0)
  }
  
  #define total cost
  grid_solved$total_cost <- result_lp$objval
  return(grid_solved)
  
}