################################################################################
#  Description:
#'  function to calculate the installation cost for a line 
#'  for trafos the installation is included within the material cost
# 
#' @title         get_material_cost
#' 
#                 name         type                   description  
#' @param  \strong{line}       'one line of a SimTOOL cable 
#' 
#'  
#' @return  Material cost for a line segment
#' @details Material costs are in ???/km in the costs data. 
#'          Where installation costs are given in ???/m. 
#'  
#' @seealso
#' \code{\link{replace_line_by_bigger}}, \code{\link{replace_line_by_parallel}}
#' @author        Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################

# function to determine material cost
get_material_cost <- function(line, verbose = 0){
  if (verbose > 10) print('get_material_cost called')
  if (!exists('line_types') | !exists('trafo_types')) load(file = '~/Documents/test/types.RData')
  model <- as.character(line$model)
  material_cost <- NA

  if (line$type == 'line') {
    line_length <- get_line_length(line$element, verbose = verbose)
    #todo find a better solution to pass specific material costs
    line_types$cost[line_types$type == 'NAYY4x150'] <- 90000
    per_unit_cost <- as.numeric(line_types$cost[line_types$type == model])
  

    if (!is.numeric(per_unit_cost) | length(per_unit_cost) == 0 ) {
      stop('the cost of the line is unknown')
    } 
    material_cost <- per_unit_cost*line_length
  }else if (line$type == 'trafo') {
    
    trafo_power <- get_trafo_power(line$element)
    model <- trafo_types$type[which(trafo_types$S == trafo_power )]
    
    material_cost <- as.numeric(trafo_types$cost[trafo_types$type == model])
    
    if (!is.numeric(material_cost) | length(material_cost) == 0 ) {
      stop('the cost of the trafo is unknown')
    } 
  }else if (line$type == 'SWITCH' | line$type == 'switch') {
    material_cost <- 100
  }else if (length(material_cost) == 0) {
    stop('material cost cannot be determined since there its neither a trafo nor a line in lines$type')
  }
    if (is.na(material_cost)) {
      print(material_cost)
      stop(paste('there is no cost data available for', model ))
    } 
  return(material_cost)
}
