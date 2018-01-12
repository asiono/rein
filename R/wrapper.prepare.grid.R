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


wrapper.prepare.grid <- function(grid, check = F, verbose = 0){
  source('R/convert.lines.R')
  source('R/replace_trafo_types.R')
  source('R/replace_line_types.R')
  source('R/check_reinforcement.R')
  
  # setting some probleme cases to NULL 
  grid$current <- NULL
  grid$transm_power <- NULL
  
  if (verbose > 0) print('################# processing function: check grid #################')
  if (check) check_reinforcement(lines = grid$lines)
  if (verbose > 0) print('################# processing function: replace_line_types #################')
  grid <- replace_line_types(grid = grid, line_types = line_types, verbose = verbose )
  if (verbose > 0) print('################# processing function: replace_trafo_types #################')
  grid <- replace_trafo_types(grid = grid, trafo_types = trafo_types, verbose = verbose)
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
  }else{
    warm = F
    #change the power into kilo-Watt
    actual <- grid$S_cal/1000
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
  if (verbose > 0) print('################# setting a lines type #################')
  grid$lines$type <- get.element.type(grid$lines$element)
  
  return(grid)
}


