###############################################################################
#  Description:
#' This function replaces the line segment from the \code{begin_node} to the \code{end_node}.
#' The new line goes parallel to the \code{crit_feeder_sorted} from the \code{start_node} to the \code{end_node}. 
# 
#' @title         replace_line_by_parallel
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' 
#' @param  \strong{start_node}     'point at which parallel lines start allways
#' 
#' @param  \strong{begin_node}     'begin node of the segment to be replaced
#'
#' @param  \strong{end_node}      'end node of the segment to be replaced
#'
#' @param  \strong{crit_feeder_sorted}   'feeder to be reinforced by parallel line
#' 
#' @param  \strong{chosen_line_type}     'line type out of which the parallel line shall be built
# @details 
#' 
#' @seealso
#' \code{\link{parallel_lines}}, \code{\link{sort.begin.end.nodes}}
#' @author       Wolfgang Biener      wolfgang.biener(at)ise.fraunhofer.de
################################################################################


replace_line_by_parallel <- function(grid,start_node,begin_node,end_node,
                                     crit_feeder_sorted,chosen_line_type , verbose = 0){

  lines <- grid$lines
  line_to_replace <- which(lines$begin == begin_node  &
                             lines$end == end_node)
  
  # initialising variables
  line_length <- 0
  x_coords <- c()
  y_coords <- c()
  material_cost <- 0
  installation_cost <- 0
  
    if (length(line_to_replace) == 1) {
    
    graph <- grid.to.igraph(grid$lines)
    
    if (start_node == begin_node) {
      
      if (lines$type[line_to_replace] == 'line') {
        line_length <- get_line_length(line_element = lines$element[line_to_replace], verbose = verbose)
        
        if (is.null(grid$lines$add_x)) {
          grid <- add.connection(grid, begin = begin_node, end = end_node ,type = 'line',
                                 par = list(type = chosen_line_type, length = line_length))
          
        }else{
          grid <- add.connection(grid, begin = begin_node, end = end_node ,type = 'line',
                                 x = grid$lines$add_x[line_to_replace],
                                 y = grid$lines$add_y[line_to_replace], 
                                 par = list(type = chosen_line_type, 
                                           length = line_length))
        }
        
        lines <- grid$lines 
        line_to_replace_old <- line_to_replace
        line_to_replace <- length(lines$begin)
        lines$model[line_to_replace] <- lines$model[line_to_replace_old]
        lines$type[line_to_replace] <- lines$type[line_to_replace_old]
        lines$install_factor[line_to_replace] <- lines$install_factor[line_to_replace_old]
        lines$settlement_class[line_to_replace] <- lines$settlement_class[line_to_replace_old]
        lines$installation_type[line_to_replace] <- lines$installation_type[line_to_replace_old]
        lines$changed[line_to_replace] <- 'double'
        installation_cost <- get_installation_cost(line = lines[line_to_replace,], verbose = verbose)
      }
      
      if (lines$type[line_to_replace] == 'switch' || lines$type[line_to_replace] == 'SWITCH') {
        lines <- grid$lines 
        lines$changed[line_to_replace] <- 'parallel'
        installation_cost <- 100
      }

    }else{
      edges_position <- get.shortest.paths(graph, from = start_node, to = end_node,
                                           output = c("epath"))$epath[[1]]
      
      for (e in edges_position) {
        if (lines$type[e] != 'line') next
        line_length <- line_length + get_line_length(line_element = lines$element[e], verbose = verbose)
        installation_cost <- installation_cost + get_installation_cost(line = lines[e,], verbose = verbose)
        
        x_coords <- c(x_coords, lines$add_x[line_to_replace])
        y_coords <- c(y_coords, lines$add_yline_to_replace[])
        
      }
      
      lines[edges_position,c('element','model','cost_material','cost_installation' )]
      
      if (!all(lines$type[edges_position] %in% c('switch','SWITCH') )) {
        #rewriting the line
        lines$begin[line_to_replace] <- start_node
        lines$end[line_to_replace] <- end_node
        
        lines$element[line_to_replace] <- paste('line(',chosen_line_type,
                                                ',',line_length,')', sep = '')
        lines$model[line_to_replace] <- chosen_line_type
        lines$max_I[line_to_replace] <- line_types$max_I[which(line_types$type == chosen_line_type)]
        if (!is.null(x_coords)) lines$add_x[line_to_replace] <- paste(x_coords, collapse = ',')
        if (!is.null(x_coords)) lines$add_y[line_to_replace] <- paste(y_coords, collapse = ',')
        lines$changed[line_to_replace] <- 'parallel'
        
      }else{
        lines <- grid$lines 
        lines$changed[line_to_replace] <- 'parallel'
        installation_cost <- 100
        
      }
      
    }
    
    # add cost data
    lines$cost_material[line_to_replace] <- get_material_cost(line = lines[line_to_replace,],
                                                              verbose = verbose)
    lines$cost_installation[line_to_replace]  <- installation_cost
    lines[line_to_replace,]
    grid$lines <- lines
    grid$lines
    grid$changed_line_length <- line_length
    
  }
  
  return(grid)
}