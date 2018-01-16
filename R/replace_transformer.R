###############################################################################
#  Description:
#' This function replaces a trafo defined by \code{start_node} and  \code{end_node} by  
#' \code{chosen_line_type}. 
#'  
#' @title         replace_transformer
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' 
#' @param  \strong{start_node}    'begin node of the segment to be replaced
#'
#' @param  \strong{end_node}      'end node of the segment to be replaced
#'
#' @param  \strong{chosen_trafo_type}     'trafo type to replace the old transformer
# @details 
#' 
#' @seealso
#' \code{\link{parallel_lines}}, \code{\link{sort.begin.end.nodes}}
#'  \code{\link{increase_line_size}}, \code{\link{replace_line_by_bigger}}

#' @author       Wolfgang Biener      wolfgang.biener(at)ise.fraunhofer.de
################################################################################

replace_transformer <- function(grid,start_node,end_node,
                                chosen_trafo_type, verbose = 0, trafo_types = NA){

  if (verbose > 0) print('replace_transformer started')
  
  #checking if data(types) has already been executed 
  if (is.na(trafo_types)) lazyLoad('types')
  assign('trafo_types', trafo_types, envir = .GlobalEnv)
  
  lines <- grid$lines
  
  if (length(start_node) != length(end_node) | 
     length(start_node) != length(chosen_trafo_type)) {
    
    stop('there is a problem with input data in ')
  }
  for (i in 1:length(start_node)) {
    line_nr <- which(lines$begin == start_node[i] & lines$end == end_node[i])
    
    if (lines$type[line_nr] != 'trafo') next

    trafo_voltages <- get_trafo_voltages(trafo = lines[line_nr, 'element'])
    
    lines$element[line_nr] <- paste0('trafo(',chosen_trafo_type[i],
                                    ',',trafo_voltages$V1,
                                    ',', trafo_voltages$V2,')')
    lines$model[line_nr] <- chosen_trafo_type[i]
    lines$cost_material[line_nr]  <- get_material_cost(line = lines[line_nr,],
                                                       verbose = verbose)
    lines$changed[line_nr] <- 'trafo_changed'

    lines$max_I[line_nr]  <- trafo_types$max_I[which(trafo_types$type == chosen_trafo_type)]

  }
  
  grid$lines <- lines
  
  return(grid)
}