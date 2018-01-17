################################################################################
#  Description:
#'  function to determine the power of a transformer
# 
#' @title         get_trafo_voltages
#' 
#                 name         type                   description  
#' @param  \strong{trafo}       'line$element entry of SimTOOL lines  
#' 
#'  
#' @return  list with V1 and V2
#' @seealso
#' \code{\link{get_line_length}}
#' @author        Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################


get_trafo_voltages <- function(trafo){
  # getting high and lowvoltage level
  trafo <- as.character(trafo)
  trafo <- sub(")", "", trafo)
  trafo <- strsplit(trafo, ",")

  if (length(trafo[[1]]) == 3) {
    V1 <- as.numeric(unlist(trafo)[2])
    V2 <- as.numeric(unlist(trafo)[3])
  }else{
    V1 <- as.numeric(unlist(trafo)[7])
    V2 <- as.numeric(unlist(trafo)[8])
  }

  return(list(V1 = V1, V2 = V2))
}
