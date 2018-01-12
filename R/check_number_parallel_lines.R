################################################################################
# Description:
#'Checks if the number of obtained parallel lines is correct 
#'
#' @title         check_number_parallel_lines
#' 
#                   name         type                   description  
#' @param  \strong{lines}              'lines of the grid (part of SimTOOL container of grid data)
#' @param  \strong{parallel_lines}     'Dataframe containing all parallel lines
#' @param  \strong{rootnode}           'node from which parallel lines start. normally lv side of transformer
#' @return 
#' throws a warning if number of parallel lines is wrong 
#'@keywords parallel_lines
#'@author        Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################


check_number_parallel_lines <- function(parallel_lines, lines , root_node){
  
  # nb of found plines 
  nb_plines <- length(which(grepl('Parallel',colnames(parallel_lines))))
  
  # number of nodes without lv and hv side of transformer
  nb_nodes <- length(unique(c(lines$begin, lines$end))) - 3/2*length(which(is.na(match(lines$end, lines$begin))))
  
  graph <- grid.to.igraph(lines, directed = T)
  nb_main_branches <- degree(graph)[root_node, drop = T] 
  nb_pline_target <- nb_nodes - nb_main_branches

  if (nb_plines != nb_pline_target[[1]]) {
    warning('the number of parallel lines does not fit to the needed number of parallel lines.\n',
            'Created number of parallel lines: ', nb_plines, '. Correct number: ', nb_pline_target)
  }
  
}