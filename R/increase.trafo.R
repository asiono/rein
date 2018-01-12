################################################################################
#  Description:
#'  The function searches lines that connect out of a set of pcc 
# 
#' @title         increase.trafo
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL grid object
#' 
#' @param  \strong{trafo_line_nb}     'number of trafo in grid_lines 
#' 
#' @param  \strong{trafo_types}   'trafo_types from data(types)
#'
#'  @param  \strong{allowed_trafo_types}   'a list of trafo trypes that may be chosen.
#' 
#'  
#' @return grid with a replaced trafo
#' @seealso
#' \code{\link{debranch.feeder_nodes}}, \code{\link{grid.to.igraph}}
#' @author        Benjamin Krug           wolfgang.biener(at)ise.fraunhofer.de
################################################################################


increase.trafo <- function(grid,trafo_line_nb,trafo_types, allowed_trafo_types, verbose=1) {
  
  if (missing(trafo_line_nb)) {
    trafo_line_nb <- grep("trafo", grid$lines$element)[[1]]
    print(sprintf("increase.trafo: No trafo_line_nb specified. 
                    %s selected for enlargement.",grid$lines$ID[trafo_line_nb]))
  }

  if (missing(trafo_types)) {
    data(types, envir = environment())
  }
  trafo_parameters <- get.element.parameters(grid$lines$element[trafo_line_nb]) 
  
  #find next transformer size
  for (trafo_i in 1:length(allowed_trafo_types)) {
    #get line of transformer
    trafo_line_nb_next <- grep(allowed_trafo_types[trafo_i],trafo_types$type)
    #extract maximal current and compare with current transformer
    if (trafo_types$max_I[trafo_line_nb_next] > grid$lines$max_I[trafo_line_nb]) {
      break()
    } else if (trafo_i == length(allowed_trafo_types) && verbose > 0) {
      print("replace.trafo: Transformer could not be replaced. There is no bigger transformer available.")
    }
  }
  grid <- replace.trafo(grid, trafo_line_nb, trafo_type = allowed_trafo_types[[trafo_i]], trafo_types)
  return(grid)
}
