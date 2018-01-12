################################################################################
#  Description:
#' This function determines the average load at a node. It is for internal use of \code{calculate.loss.cost}
# 
#' @title         determine.average_load
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' 
#' @param  \strong{load_types}   'Character vector for load types of all nodes
#' 
#'#' 
#' @details 
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

#' If no load types are defined, all loads are assumed as housholds(H0). For each type an average load value is stored in the function definition.
#'
#
#'@seealso
#'  \code{\link{calculate.loss.cost}}, \code{\link{calculate.material.cost}}, \code{\link{calculate.installation.cost}} 
#'
#' @keywords cost, loss , loads
#' @author        Benjamin Krug              benjamin.krug(at)ise.fraunhofer.de
#' 
#################################################################################

determine.average_load <- function(grid, load_types = c()) {
  
  average_load_data <- c(0.53, 0.47, 0.37, 0.53, 0.48, 0.24, 0.46, 0.74, 0.5, 0.46, 0.38) # see standard load profile data (excel sheets)
  names(average_load_data) <- c("H0", "L0", "L1", "L2", "G0", "G1", "G2", "G3", "G4", "G5", "G6")
  
  nb_loads <- which(Re(grid$S_cal) < 0)
  nb_generators <- which(Re(grid$S_cal) > 0)
  # check input of load_types
  if (length(load_types) == 0) {
    print("calculate.loss.costs: no load types specified: the H0 standard load profile is assigned to all loads")
    load_types <- Mod(grid$S_cal)
    load_types[] <- NA
    load_types[nb_loads] <- "H0"
    load_types[nb_generators] <- "PV"
  } else if (length(load_types) != length(grid$S_cal)) {
    stop("calculate.loss.costs: length of load_types is unequal to grid$S_cal")
  } else {
    names(load_types) <- names(grid$S_cal)
    if (any(load_types[!is.na(load_types)] %in% c("PV", names(average_load_data)))) {
      print("calculate.loss.costs: there are unknown load_types. Please check the spelling of the input.")  
    } 
  }
  load_factors <- Mod(grid$S_cal)
  load_factors[] <- 0

  for (load_type_i in seq_along(average_load_data)) {
    load_factors[which(load_types == names(average_load_data)[[load_type_i]])] <- average_load_data[load_type_i]
  }
  average_load <- as.numeric(load_factors %*% (Mod(grid$S_cal)))
  return(average_load)
}
