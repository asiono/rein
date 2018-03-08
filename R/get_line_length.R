################################################################################
#' @title         get_line_length
#' @description   this function returns the line length from a lines$element
#' @param line_element   element value of line data frame 
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @return  line length in km
################################################################################

get_line_length <- function(line_element, verbose = 0){
  if (verbose > 0) print('get_line_length_called')
  
  length_line_element <- length(line_element)
  line_length_vec <- rep(NA, length_line_element)
  
  for (i in 1:length_line_element) {
    
    line_element_char <- strsplit(line_element[i], ",")
    if (grepl('line', line_element_char)) {
      if (length(line_element_char[[1]]) == 2) {
        line_length <- sub(x = unlist(line_element_char)[2], pattern = ')', replacement = '')
        
      } else{
        line_length <- sub(x = unlist(line_element_char)[5], pattern = ')', replacement = '')
      }
    }else{
      
      line_length <- 0.01
    }
    
    
    line_length_vec[i] <- as.numeric(line_length)

    if (!is.numeric(line_length_vec[i])) {
      stop(paste('there a problem with the calculation of line length of the element', line_element[i]))
    }
  }
  
  if (length(line_element) != length(line_length_vec)) {
    stop('the line lengths are not calculated correctly')
  }
  
  return(line_length_vec)
  
}
