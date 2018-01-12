################################################################################
#  Description:
#'  function to determine the power of a transformer
# 
#' @title         get_trafo_power
#' 
#                 name         type                   description  
#' @param  \strong{trafo}       'line$element entry of SimTOOL lines  
#' 
#'  
#' @return  Trafo power in kW 
#' @seealso
#' \code{\link{get_line_length}}
#' @author        Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################


get_trafo_power <- function(trafo){
  trafo <- as.character(trafo)
  
  line_element <- strsplit(trafo, ",")
  
  if (length(line_element[[1]]) == 3) {
    trafo_name <- sub(x = unlist(line_element)[1], pattern = 'trafo\\(', replacement = '')
    trafo_place <- which(trafo_types$type == trafo_name)
    trafo_power <- trafo_types$S[trafo_place]
  }else{
    trafo_power <- sub(x = unlist(line_element)[1], pattern = 'trafo\\(', replacement = '')
  }
  trafo_power <- as.numeric(trafo_power)

  if (!is.numeric(trafo_power)) stop('there is a problem with the calculation of Trafo power')
  
  return(trafo_power)
  
}
