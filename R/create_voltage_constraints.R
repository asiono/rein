################################################################################
# Description:
#' this function creates the constraints given by the current       
#'
#' @title         create_voltage_constraints
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{lines}                'lines data of the grid 
#' @param  \strong{allowed_deviation}    'uncertainity of voltage calculation that is taken as security margin 
#' @param  \strong{allowed_voltage}      'allowed voltage rise 
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and c. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener/Gunther Gust         wolfgang.biener(at)ise.fraunhofer.de
################################################################################


create_voltage_constraints <- function(solution_space, big_M,allowed_voltage, verbose = 0){
  source('~/Documents/rein2/rein.git1/reIn/R/create_voltage_constraints_no_parallel.R')
  #source('~/Documents/rein2/rein.git1/reIn/R/create_voltage_constraints_parallel.R')
  # todo anpassen auf neue notation in problembeschreibung
  
  matrices_voltage_no_parallel <- create_voltage_constraints_no_parallel(solution_space, big_M,
                                                                         allowed_voltage)
#   View(cbind(matrices_voltage_no_parallel$A,matrices_voltage_no_parallel$b,
#              matrices_voltage_no_parallel$const.dir))

  matrices_voltage_parallel <- create_voltage_constraints_parallel(solution_space, big_M,
                                                                      allowed_voltage)
#   View(cbind(matrices_voltage_parallel$A,matrices_voltage_parallel$b,
#              matrices_voltage_parallel$const.dir))
#   
#source('~/Documents/rein2/rein.git1/reIn/R/create_voltage_constraints_parallel_branch.R')
    matrices_voltage_parallel_branch <- create_voltage_constraints_parallel_branch(solution_space, big_M,
                                                                                 allowed_voltage)

#   View(cbind(matrices_voltage_parallel_branch$A,matrices_voltage_parallel_branch$b,
#              matrices_voltage_parallel_branch$const.dir))
#   
  A <- rbind(matrices_voltage_no_parallel$A,matrices_voltage_parallel$A,matrices_voltage_parallel_branch$A)
  b = c(matrices_voltage_no_parallel$b,matrices_voltage_parallel$b,matrices_voltage_parallel_branch$b)
  const.dir <- c(matrices_voltage_no_parallel$const.dir,matrices_voltage_parallel$const.dir,matrices_voltage_parallel_branch$const.dir)

  if (verbose > 3) {
    print('A,const.dir, b')
    print(cbind(A,const.dir,b))

  }
  
  matrices <- list( A = A, b = b, const.dir = const.dir)
  
  return(matrices)
}

