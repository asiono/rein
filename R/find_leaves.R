################################################################################
# Description:
#'Fucntion to find the leave nodes of a grid. It searches the nodes having only
#' one connection.
#'
#' @title         find_leaves
#' 
#                   name                           description  
#' @param  \strong{lines}            'lines_data frame of package SimTOOL
#' @param  \strong{slack = 'GRID'}   'Slack node of the grid.
#' 
#' 
#' @return 
#' Names of leave nodes of the grid. 
#' @details 
#' the slack node is erased from the leaves
#'@keywords paths grid_paths
#'@author        Gunther Gust             gunther.gust(at)is.uni-freiburg.de
################################################################################

find_leaves <- function(lines, slack = 'GRID'){

  all_nodes <- c(lines$begin, lines$end)
  duplicated_nodes <- duplicated(all_nodes)
  unique_nodes <- all_nodes[!all_nodes %in% duplicated_nodes]
  
  if (length(which(unique_nodes == slack)) == 1) {
    leaves <- unique_nodes[unique_nodes != slack]
  }else{
    warning('Slack is defined as:', slack, 'is it right?')  
    }
  
  return(leaves)
} 