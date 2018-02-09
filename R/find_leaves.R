################################################################################
#' @title find_leaves
#' @description find the leaf node(s) and remove slack node of a grid. It searches the nodes having only one connection.
#' @param lines lines_data frame of package SimTOOL
#' @param slack Slack node of the grid.
#' @return Names of leave nodes of the grid.
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