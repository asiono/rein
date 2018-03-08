################################################################################
#' @title sort_begin_end_by_grid_order
#' @description  reorders the beginning and the end just as it is in the original lines object
#' @param lines  data frame from grid data containing grid assets and its parameters
#' @param data_frame  dataframe having begin and end as cols like grid$lines
#' @param cols character vector giving the corresponding names of 'begin' and 'end'. c('begin', 'and) is the custom values
#' @return this function orders the begin and end points just as it is in grid$lines
################################################################################

sort_begin_end_by_grid_order <- function(lines, data_frame, cols = c('begin','end')){

  lines_begin_end <- lines[,c('begin','end')]

  # resorting parallel_to to fit with lines
  data_frame_begin_end <- data_frame[,cols]
  data_frame_begin_end[,cols[1]] <- as.character(data_frame_begin_end[,cols[1]])
  data_frame_begin_end[,cols[2]] <- as.character(data_frame_begin_end[,cols[2]])
  
  for (i in 1:nrow(data_frame_begin_end)) {
    data_frame_i <- data_frame_begin_end[i,]
    for (j in 1:nrow(lines_begin_end)) {
      all(data_frame_i %in% lines_begin_end[j,])
      if (all(data_frame_i %in% lines_begin_end[j,])) {
        data_frame_begin_end[i,] <- lines_begin_end[j,]
        break
      }
      if (j == nrow(lines_begin_end)) {
        stop('It was not possible to finde the right direction. 
             Probably a factor problem.')
      }
      
    }
  }  
  
  data_frame[, cols] <- data_frame_begin_end
  
  return(data_frame)
  
}
  
  
