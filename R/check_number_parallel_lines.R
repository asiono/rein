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


check_number_parallel_lines <- function(parallel_lines, lines){
  # nb of found plines 
  nb_plines <- length(which(grepl('Parallel',colnames(parallel_lines))))
  
  # number of nodes deducted by: 
  # number of nodes connected to LV side of transformer,
  # number of leaf nodes,
  # hv and lv side of transformer
  nb_pline_target <- length(unique(c(grid$lines$begin, grid$lines$end))) - 
    sum(!is.na(match(lines$begin, lines$end[lines$type == 'trafo']))) - sum(is.na(match(lines$end, lines$begin))) -
    2 * nrow(lines[lines$type == 'trafo',])
  
  if (nb_plines != nb_pline_target[[1]]) {
    warning('the number of parallel lines does not fit to the needed number of parallel lines.\n',
            'Created number of parallel lines: ', nb_plines, '. Correct number: ', nb_pline_target)
  }
  
}