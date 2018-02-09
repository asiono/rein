################################################################################
#' @title    check_number_parallel_lines
#' @description Checks if the number of obtained parallel lines is correct 
#' @param parallel_lines   number of parallel lines created
#' @param lines   data frame from grid data containing grid assets and its parameters
#' @return  warnings if the number of created parallel lines is not correct
################################################################################

check_number_parallel_lines <- function(parallel_lines, lines){
  # nb of found plines 
  nb_plines <- length(which(grepl('Parallel',colnames(parallel_lines))))
  
  # number of nodes deducted by: 
  # number of nodes connected to LV side of transformer,
  # number of leaf nodes,
  # hv and lv side of transformer
  nb_pline_target <- length(unique(c(grid$lines$begin, grid$lines$end))) - sum(is.na(match(lines$end, lines$begin))) -
    2 * nrow(lines[lines$type == 'trafo',])
  
  if (nb_plines != nb_pline_target[[1]]) {
    warning('the number of parallel lines does not fit to the needed number of parallel lines.\n',
            'Created number of parallel lines: ', nb_plines, '. Correct number: ', nb_pline_target)
  }
  
}