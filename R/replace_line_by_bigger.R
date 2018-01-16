###############################################################################
#  Description:
#' This function replaces a line defined by \code{start_node} and  \code{end_node} by  
#' \code{chosen_line_type}. 
#'  
#' @title         replace_line_by_bigger
#' 
#                 name         type                   description  
#' @param  \strong{grid}              'SimTOOL container of grid data
#' 
#' @param  \strong{start_node}        'begin node of the segment to be replaced
#'
#' @param  \strong{end_node}          'end node of the segment to be replaced
#'
#' @param  \strong{chosen_line_type} 'line type to replace the old line
#' 
#' @seealso
#' \code{\link{parallel_lines}}, \code{\link{sort.begin.end.nodes}}
#' \code{\link{increase_line_size}}, \code{\link{replace_transformer}}
#' @author       Wolfgang Biener      wolfgang.biener(at)ise.fraunhofer.de
################################################################################

replace_line_by_bigger <- function(grid,start_node,end_node,
                                   chosen_line_type, verbose = 0, line_types = NA){
  
  if (verbose > 0) print('replace_line_by_bigger started')

    if (length(start_node) != length(end_node) | length(start_node) != length(chosen_line_type)) {
    stop('there is a problem with input data in replace_line_by_bigger')
  }
  
  #checking if data(types) has already been executed 
  if (is.na(line_types)) lazyLoad('types')
  assign('line_types', line_types, envir = .GlobalEnv)
  
  lines <- grid$lines
  rownames(lines) <- 1:nrow(lines)
  graph <- grid.to.igraph(lines)
  line_length_sum <- 0
  for (i in 1:length(start_node)) {

    edges_position <- get.shortest.paths(graph, from = start_node[i], to = end_node[i],
                                         output = c("epath"))$epath[[1]]

    lines$cost_material <- c(rep(0, nrow(lines)))
    lines$changed <- c(rep(0, nrow(lines)))
    lines$max_I <- c(rep(0, nrow(lines)))

    for (e in edges_position) {
      
      if (lines$type[e] != 'line') next
      
      line_length <- get_line_length(line_element = lines$element[e], verbose = verbose)
      line_length_sum <- line_length_sum + line_length
      
      lines$element[e] <- paste0('line(',chosen_line_type[i],
                                ',',line_length,')')
      lines$model[e] <- chosen_line_type[i]
      source('~/Documents/rein2/rein.git1/reIn/R/get_material_cost.R')
      lines$cost_material[e]      <- get_material_cost(line = lines[e,], verbose = verbose)

      lines$cost_installation[e]  <- get_installation_cost(line = lines[e,],
                                                           verbose = verbose)
      lines$changed[e] <- 'increased'
      lines$max_I[e]   <- line_types$max_I[which(line_types$type == chosen_line_type[i])]
    }
  }
  
 
  lines$begin <- as.character(lines$begin)
  lines$end <- as.character(lines$end)
  lines$element <- as.character(lines$element)
  grid$lines <- lines
  
  grid$changed_line_length <- line_length_sum
  
  return(grid)
}