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

check_reinforcement <- function(lines){

  # check if all line and trafos types are known
  if (!exists('trafo_types') | !exists('line_types')) lazyLoad('types')
  if (!all(grid$lines$model %in% line_types$type | grid$lines$model %in% trafo_types$type)) 
    {
    stop('Not all cable and transformer types in the grid are known')
    }
  
  # preparing igraph
  graph <- graph.data.frame(grid$lines, directed = F)
  #check islands 
  if (!is.connected(graph)) stop('There is islanding in the grid')
  # check if grid still has a double line 
  if (any(is.loop(graph))) stop('The grid has double line')    
  # check if grid is radial  
  if (ecount(mst(graph)) != ecount(graph)) stop('The tested grid is not radial')


}
  
  
