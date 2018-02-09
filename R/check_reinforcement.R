  ################################################################################
#' @title         check_reinforcement
#' @description   This function checks if reinforcement is possible
#' @param lines   data frame from grid data containing grid assets and its parameters
#' @param line_types   database containing all line types and its specification
#' @param trafo_types   database containing all line types and its specification
################################################################################

check_reinforcement <- function(lines, line_types = NA, trafo_types = NA){

  # check if all line and trafos types are known
  if (is.na(trafo_types) | is.na(line_types)) lazyLoad('types')
  if (!all(lines$model %in% line_types$type | lines$model %in% trafo_types$type)) 
    {
    stop('Not all cable and transformer types in the grid are known')
    }
  
  # preparing igraph
  graph <- igraph::graph.data.frame(grid$lines, directed = F)
  #check islands 
  if (!igraph::is.connected(graph)) stop('There is islanding in the grid')
  # check if grid still has a double line 
  if (any(igraph::is.loop(graph))) stop('The grid has double line')    
  # check if grid is radial  
  if (igraph::ecount(igraph::mst(graph)) != igraph::ecount(graph)) stop('The tested grid is not radial')


}
  
  
