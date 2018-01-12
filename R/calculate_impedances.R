################################################################################
# Description:
#' the function calculates impedances for the assets within the solution space.        
#'
#' @title         calculate_impedances
#' 
#                   name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' @param  \strong{solution_space}  'data frame containing information for possible expansion alternatives.  
#' @return 
#' \strong{solution_space} with impendances relative to the chosen refrence voltage.
#'@author   Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################

calculate_impedances <- function(grid, solution_space, type){
  
  if (type == 'line') {
    # multiplying per km data with their length
    solution_space[, c("R",  "L",	"G",	"C", 'cost')] <- solution_space[, c("R",  "L",	"G",	"C", 'cost')]*solution_space$length_km
    solution_space$X <- solution_space$L*2*pi*50/1e3
    
    solution_space[, c("R",  "L",	"G",	"C", 'X')] <- 
      solution_space[, c("R",  "L",	"G",	"C", 'X')]/solution_space$transmissio_ratio^2
    
    } else if (type == 'trafo') {
    source('~/Documents/rein2/rein.git1/reIn/R/calc_trafo_impedance.R')
    solution_space <- calc_trafo_impedance(solution_space = solution_space, Vref = grid$Vref)
  }
 
  return(solution_space)
   
}