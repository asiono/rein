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

wrapper.prepare.grid.initial <- function(grid, check = F, line_types = NULL, trafo_types = NULL, verbose = 0){

  source('R/convert.lines.R')
  source('R/replace_trafo_types.R')
  source('R/replace_line_types.R')
  # setting some probleme cases to NULL 
  grid$current <- NULL
  grid$transm_power <- NULL
  if (verbose > 0) print('################# processing function: check grid #################')
  if (check) check.grid(grid = grid)  
  if (verbose > 0) print('################# processing function: replace_line_types #################')
  grid <- replace_line_types(grid = grid,line_types = line_types, verbose = verbose )
  if (verbose > 0) print('################# processing function: replace_trafo_types #################')
  grid <- replace_trafo_types(grid = grid, trafo_types = trafo_types, verbose = verbose)
  if (verbose > 0) print('################# processing function: convert.lines #################')
  grid <- convert.lines(grid = grid, verbose = verbose )
  if (verbose > 0) print('################# processing function: create.admittance #################')
  grid <- create.admittance(grid = grid,verbose = verbose )
  if (verbose > 0) print('################# processing function: create.power #################')
  
  # if(is.null(grid$S_cal)){
  if (F) {
    names_actual <- rownames(grid$Y_red)
    actual <- rep(0, length(names_actual))
    names(actual) <- names_actual  
    warm = T
  }else{
    warm = F
    actual <- grid$S_cal*3/1000
  }
  
  grid <- create.power(grid, verbose = verbose,  actual = actual)
  #grid <- solve.LF(grid = grid, warm = F , save = F, fast = F, verbose = verbose)
  
  return(grid)
}


