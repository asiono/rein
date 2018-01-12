###############################################################################
#  Description:
#' Helper function for increase trafo_size and replace trafo by parallel
# 
#' @title         replace.trafo
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' 
#' @param  \strong{allowed_line_types}     'line types that may be chosen by reinforcement
#' 
#' 
# @details 
#' 
#' @seealso
#' \code{\link{increase.trafo}}, \code{\link{replace_line_by_parallel}}
#' @author       Benjamin Krug            wolfgang.biener(at)ise.fraunhofer.de
################################################################################

replace.trafo <- function(grid, trafo_line_nb, trafo_type, trafo_types) {
  
  if (missing(trafo_line_nb)) {
    trafo_line_nb <- grep("trafo", grid$lines$element)[[1]]
    print(sprintf("replace.trafo: No trafo_line_nb specified. 
                    %s selected for enlargement.",grid$lines$ID[trafo_line_nb]))
  }

  if (missing(trafo_types)) {
    data(types, envir = environment())
  }
  
  trafo_parameters <- get.element.parameters(grid$lines$element[trafo_line_nb])
  #build new trafo & replace old
  grid$lines$element[trafo_line_nb] <- paste("trafo(",trafo_type, ",", 
                                             trafo_parameters$V1, ",", trafo_parameters$V2,")", sep = "")

  grid$lines$type[trafo_line_nb] <- "trafo"
  grid$lines$max_I[trafo_line_nb] <- trafo_types$max_I[trafo_types$type == trafo_type]
  grid$lines$comment[trafo_line_nb] <- trafo_type
  grid$lines$model[trafo_line_nb] <- trafo_type
  #get specific parameters from database
  grid <- replace_trafo_types(grid)
  return(grid)
}
