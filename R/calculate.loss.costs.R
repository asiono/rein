################################################################################
#  Description:
# 'This function calculates the anual loss costs of a grid.
# 
#' @title        #### toDo ### calculate.loss.costs
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
# ' 
#' @param  \strong{energy_price} 'Numeric vector giving the energy price in € per kWh for the # DSO  
#' @param  \strong{load_types}   'Character vector for load types of all nodes
#' 
#' @details 
#'The methodology is referring to Dickert, J. and Hable, M. and Schegner, P. (2009): Energy loss estimation in distribution networks for planning purposes, in IEEE Bucharest Power Tech Conference, 2009.  
# 
#' For the determination of losses two load flow calculations are executed. One for maximum load case and one for no load case. The loss factor is approximated with an empiric function on basis of the load factor. The effects of decentraliced generators aren't taken into account. 
#' As \strong{load_types} the following options are possible: 

#'\itemize{
#'  \item "H0": general household
#'  \item "L0": general agriculture
#'  \item "L1": agriculture with milk and cattle
#'  \item "L2": other agriculture
#'  \item "G0": general business
#'  \item "G1": business 08:00-18:00
#'  \item "G2": business high evening load
#'  \item "G3": business continuous
#'  \item "G4": Shop/coiffeur
#'  \item "G5": baker
#'  \item "G6": business weekend. 
#'  }
#' If no load types are defined, all loads are assumed as housholds(H0). For each type an # average load value is stored in the function definition.
#'
#'\strong{energy_price} 0.042 is the energy price in Euro/ kWh. That is the average energy # price from 2010 to 2014
#
#'@seealso
#'  \code{\link{grid.reinforcement}}, \code{\link{calculate.material.cost}}, \code{\link{calc ulate.installation.cost}} 
#
#' @keywords cost, loss 
#' @author        Benjamin Krug              benjamin.krug(at)ise.fraunhofer.de
#################################################################################


calculate.loss.costs <- function(grid, energy_price=0.042, load_types = c()) {
  # determine peakload
  peakload <- Mod(sum(Re(grid$S_cal[Re(grid$S_cal) < 0])))
  # determine average load
  average_load <- determine.average_load(grid, load_types)
  
  # determine load factor
  load_factor <- average_load / peakload
  if (load_factor > 1) {
    print("calculate.loss.costs: average load is greater than peak load")
  }
  # determine loss factor - estimation functions should give nearly the same result
  loss_factor <- 0.083 * load_factor + 1.036 * load_factor^2 - 0.119 * load_factor^3 #estimation function 1
  #loss_factor2 <- load_factor^1.8  # #estimation function 2
  #loss_factor3 <- (load_factor^2 * (2+load_factor^2)) / (1 + 2 * load_factor) # #estimation function 3
  
  # determine load losses - maximum load case
  grid$S_cal[Re(grid$S_cal) > 0] <- 0 + 0i
    grid <- wrapper.prepare.grid(grid = grid, verbose = 0)
  load_loss <- Mod(grid$loss)
  
  # determine no-load losses
  grid$S_cal[] <- 0 + 0i
  grid <- wrapper.prepare.grid(grid = grid, verbose = 0)
    no_load_loss <- Mod(grid$loss)
  
  # determine energy losses per year
  annual_loss <- (loss_factor * load_loss + no_load_loss) * 8760

  # calculcate loss costs
  loss_costs <- annual_loss * energy_price
  names(loss_costs) <- "loss costs in Euro/a"
  return(loss_costs)
}