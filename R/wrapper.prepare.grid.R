################################################################################
#  Description:
#'  function that runs all necessary functions for newbuildin or rebuilding a SimTOOL grid. 
# 
#' @title         wrapper.prepare.grid
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL grid structure
#' @param  \strong{line_types} ' dataframe containing specific line types
#' @param  \strong{trafo_types} ' dataframe containing specific trafo types
#' 
#' @return  grid that has included all previously done changes
# @seealso
#' @author        Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################


wrapper.prepare.grid <- function(grid, check = F, solution_space_combined = NULL, U_set = NULL, oltc.trigger = F, verbose = 0){
  source('~/Documents/simtool/simtool/SimTOOL/R/solve.LF.R')
  #setwd('~/Documents/rein_lp/rein/')
  source('R/convert.lines.R')
  source('R/replace_trafo_types.R')
  source('R/replace_line_types.R')
  source('R/check_reinforcement.R')

  # setting some probleme cases to NULL 
  grid$current <- NULL
  grid$transm_power <- NULL
  
  if (verbose > 0) print('################# processing function: check_reinforcement #################')
  if (check) check_reinforcement(lines = grid$lines)
  if (verbose > 0) print('################# processing function: replace_line_types #################')
  grid$lines <- replace_line_types(lines = grid$lines, verbose = verbose)
  if (verbose > 0) print('################# processing function: replace_trafo_types #################')
  grid$lines <- replace_trafo_types(lines = grid$lines, verbose = verbose)
  if (verbose > 0) print('################# processing function: convert.lines #################')
  grid <- convert.lines(grid = grid, verbose = verbose )
  if (verbose > 0) print('################# processing function: create.admittance #################')
  grid <- create.admittance(grid = grid, verbose = verbose )
  if (verbose > 0) print('################# processing function: create.power #################')

  if (is.null(grid$S_cal)) {
    names_actual <- rownames(grid$Y_red)
    actual <- rep(0, length(names_actual))
    names(actual) <- names_actual  
    warm = T
  } else {
    warm = F
    #change the power into kilo-Watt
    actual <- grid$S_cal*3/1000
  }
  
  grid <- create.power(grid, verbose = verbose,  actual = actual)
  if (verbose > 0) print('################# processing function: solve.LF #################')
  #add the parallel lines into U_cal matrice for calculation
  if (any(grepl('_p', grid$cal_node))) {
    add_U_cal <- matrix(0:0, length(c(grid$cal_node[grepl('_p', grid$cal_node)])), 1, 
                      dimnames = list(c(grid$cal_node[grepl('_p', grid$cal_node)])))
    grid$U_cal <- rbind(grid$U_cal, add_U_cal)
  }
  
  grid <- solve.LF(grid = grid, warm = F , save = F, fast = F, verbose = verbose)
  
  if (any(grepl('OLTC', grid$lines$model)) & oltc.trigger == T) {
    #need to add trafo in the element because solve.LF in SimTOOL requires checking it
    grid$lines$element[which(grid$lines$type == 'trafo')] <- as.character('trafo')
    #create controller list
    grid$ctr <- list()
    #define controller entry
    grid$ctr[[1]] <- list()
    grid$ctr[[1]]$mode <- "OLTC"
    #connection nodes
    grid$ctr[[1]]$hv_node <- grid$lines$begin[which(grid$lines$type == 'trafo')]
    grid$ctr[[1]]$lv_node <- grid$lines$end[which(grid$lines$type == 'trafo')]
    grid$ctr[[1]]$ctr_node <- grid$lines$end[which(grid$lines$type == 'trafo')]
    #tap settings
    grid$ctr[[1]]$pos_taps <- 6 #voltage up regulation
    grid$ctr[[1]]$neg_taps <- 6 #voltage down regulation
    # pos_taps+neg taps + 1(0-tap) < [5,7,9]
    grid$ctr[[1]]$curr_tap <- 0  
    grid$ctr[[1]]$tap_size <- 1.5 #percentual of U_set  usual [1,5%, 2% or 2,5%]
    #[0.8 ... 2.5%]according to: On-Load Tap-Changers for Power Transformers A Technical Digest, MR Publication
    #lead voltage
    grid$ctr[[1]]$U_set <- U_set
    grid$ctr[[1]]$deadband <- 0.6 #percentual of U_set 0.6
    grid$ctr[[1]]$U_min <- U_set*(1 - 0.08)
    grid$ctr[[1]]$U_max <- U_set*(1 + 0.08)
    grid$ctr[[1]]$verbose <- 2
    grid <- solve.LF(grid = grid, meth = "G", ctr = c("OLTC"), warm = F, verbose = 0)
  }
  
  return(grid)
}


