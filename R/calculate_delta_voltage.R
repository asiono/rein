###############################################################################
#  Description:
#' This function calculates the nodal voltages taking into account the linear
#' calcualted voltage drops per line.  
#' \code{chosen_line_type}. 
#'  
#' @title         calculate_delta_voltage
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' @param  \strong{solution_space}    'solution space of the optimization problem
#' @param  \strong{result}       'result vector of the solver  

#'
#'
# @details 
#' the function calculates the delta U that is taken into account from the optimizer. 
#' This is done for every node in the grid. It is only viable for branched grids. 
#' 
#' @seealso
#' \code{\link{grid.to.igraph}} 
#' @author       Wolfgang Biener      wolfgang.biener(at)ise.fraunhofer.de
################################################################################

calculate_delta_voltage <- function(grid, solution_space, result){
  if (nrow(solution_space) != length(result)) {
    stop('There is a problem with the length of the input variables in 
         calculate_delta_voltages')
  }
  
  expansion_measures <-  cbind(solution_space, result)
  expansion_places <- which(expansion_measures$result == 1)
  expansion_measures_realized <- expansion_measures[expansion_places,
                                                    c("begin","end",'dU')]
  
  graph <- graph.data.frame(expansion_measures_realized[,c('begin','end')], 
                            directed = T)
  E(graph)$weight <- expansion_measures_realized$dU
  
  # setting the voltage drop as edgeweight
  
  # replacing the calculated voltage by the approximated voltage drop 
  U_delta <- rep(0, length(V(graph)$name))
  names_U_delta <- V(graph)$name
  names(U_delta) <- names_U_delta
  
  paths <- shortest_paths(graph,weights = NA,
                          from = 'GRID', to = names_U_delta, output = c("epath"))

  for (i in seq_along(U_delta)) {
    # replaced sum(sum(...)) by Mod(sum(...))
    U_delta[i] <- sum(E(graph)$weight[paths$epath[[i]]])  
  }  
  paths$epath[[i]]
  
  U_delta <-   1 + U_delta  
  U_delta <-   Mod(U_delta)
  
  return(U_delta)
}