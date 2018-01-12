  ################################################################################
#  Description:
#' This function checks if reinforcement is possible
#' @title         check_reinforcement
#' 
#                 name         type                   description  
#' @param  \strong{grid}           'SimTOOL container of grid data
#' 
#' @seealso
#' \code{\link{grid.reinforcement}}
#' @author        Wolfgang Biener      wolfgang.biener(at)ise.fraunhofer.de
################################################################################

check_reinforcement <- function(grid){

  # check if all line and trafos types are known
  if (!exists('trafo_types')) data(types)
  known_types <- c(trafo_types$type,line_types$type)
  if (!all(grid$lines$model %in% known_types)) {
    stop('Not all types in grid$lines$model are known. This is necessary for the
         calculation of voltage drops.')}
  
  # preparing igraph
  graph <- graph.data.frame(grid$lines, directed = F)
  #check islands 
  if (!is.connected(graph)) stop('There are islands')
  # check if grid still has a double line 
  if (any(is.loop(graph))) stop('grid still has a double line')    
  # check if grid is radial  
  if (ecount(mst(graph)) != ecount(graph)) stop('graph is not radial ')


}
  
  
