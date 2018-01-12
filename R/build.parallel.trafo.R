################################################################################
#  Description:
#' function replaces a transformer by two identical parallel transformers
#' the size of the transformers is adjusted to the usage of the old transformer
# 
#' @title         #### toDo ####  build.parallel.trafo 
#' 
#                 name         type                   description  
#' @param  \strong{trafo_line_nb}       'row number of the trafo in grid$lines
#' 
#' @param  \strong{trafo_types}     ' global data frame with all transformer types 
#' 
#
#' @param  \strong{allowed_trafo_types}  vector with all allowed trafo types (usually from grid.reinforcement)
#' 
#'  

#' 
#' @details 
#' extensions
#' differnt methods for the type selection
#' usage of different transformer types --> compatibility check necassary 
#' (identical transmission ratio, nearly the same Uk, S2<max(3*S1), identical con, identical V1 and V2)

#
#
#' @seealso
#' \code{\link{grid.reinforcement}}, \code{\link{grid.to.igraph}}
#' @author        Benjamin Krug              benjamin.krug(at)ise.fraunhofer.de
################################################################################

build.parallel.trafo <- function(grid,trafo_line_nb,trafo_types, allowed_trafo_types, verbose=1) {
  
  if (missing(trafo_line_nb)) {
    trafo_line_nb <- grep("trafo", grid$lines$element)[[1]]
    print(sprintf("increase.trafo: No trafo_line_nb specified. 
                  %s selected for enlargement.",grid$lines$ID[trafo_line_nb]))
  }

  if (missing(trafo_types)) {
    data(types, envir = environment())
  }
  
  old_trafo_parameters <- get.element.parameters(grid$lines$element[trafo_line_nb])
  # select transformer size
  S_options <- trafo_types$S[trafo_types$type %in% allowed_trafo_types]
  

  if (!is.null(grid$usage)) { # select size adjusted to actual usage
    trafo_usage <- as.data.frame(grid$usage)[grid$lines$end[trafo_line_nb],grid$lines$begin[trafo_line_nb]]
    new_S <- 0.5 * (old_trafo_parameters$S * trafo_usage) # '* 0.5' for half of needed apparent power
    if (new_S < 0.5*S_options[[1]] && verbose > 0) {
      print("build.parallel.trafo: Usage of the transformer is very low. Smallest allowed trafo types are used. 
            Consider replacing the transformer instead of parallel transformers")
    }
    new_S <- S_options[S_options > new_S][[1]]
    trafo_type <- trafo_types$type[trafo_types$S == new_S]
    #replace old trafo
    grid <- replace.trafo(grid, trafo_line_nb, trafo_type, trafo_types)

        if (verbose > 1) {
      print(sprintf("build.parallel.trafo: Two transformers of type %s have been build",trafo_type))
    }
    
  } else{# use type of existing trafo 
    trafo_type <- trafo_types$type[which(trafo_types$S >= old_trafo_parameters$S &
                                           trafo_types$type %in% allowed_trafo_types)][[1]]
    if (verbose > 1) {
      print(sprintf("build.parallel.trafo: Transformer of type %s has been added",trafo_type))
    }
  }
  # build new trafo
  grid <- add.connection(grid,begin = grid$lines$begin[trafo_line_nb], end = grid$lines$end[trafo_line_nb],
                         type = "trafo", par = list(V1 = old_trafo_parameters$V1,V2 = old_trafo_parameters$V2,
                                                type = trafo_type), verbose = 0)
  grid <- replace_trafo_types(grid)
  # check compability
  #... only needed, when different trafo types are used
  return(grid)
  }
