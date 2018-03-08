################################################################################
#' @title  reactive_power_control
#' @description change feed-in power based on specific power factor 
#' @param lines lines data of the grid
#' @param S_cal omplex vector giving all powers in internal notation for cal_nodes
#' @param verbose verbosity level
#' @return This function change the value of active and reactive power of the feed-in. 
#' The ouput is new complex number of feed-in power. 
################################################################################

reactive_power_control <- function(lines, S_cal, verbose = 0) {
  
  #calculate node distance from LV side of transformer
  grid_begin <- lines[which(lines$type == 'trafo'), 'begin']
  PF_matrix <- cbind(lines[,c("begin","end")], line_length = get_line_length(lines$element))

  for (i in 1:nrow(PF_matrix)) {
    if (PF_matrix$begin[i] != grid_begin) {
      repeat {
      PF_matrix$line_length[i] <- PF_matrix$line_length[i] + PF_matrix$line_length[which(PF_matrix$end == PF_matrix$begin[i])]
      PF_matrix$begin[i] <- PF_matrix$begin[which(PF_matrix$end == PF_matrix$begin[i])]
      if (PF_matrix$begin[i] == grid_begin) {
        break
        }
      }
    } else {
      PF_matrix$line_length[i] <- PF_matrix[which(PF_matrix$begin == grid_begin), "line_length"] }
  }
  
  PF_matrix <- merge(PF_matrix, as.data.frame(S_cal), by.x = "end", by.y = 0)
  
  #setting power factor
  #power factor are set based on distance from transformer and load
  #+------------------+-------------------------+
  #|                  |       Load balance      |
  #+                  +-------------------------+
  #|     Distance     |  low  |  medium |  high |
  #+                  +-------+---------+-------+
  #|                  | <3.6kW | 3.6-13.8kW | >13.8kW |
  #+------------------+-------+---------+-------+
  #| short  | <50m    |  1.0  |   1.0   |  1.0  |
  #+--------+---------+-------+---------+-------+
  #| medium | 50-100m |  1.0  |   1.0   |  0.95 |
  #+--------+---------+-------+---------+-------+
  #|  far   | >100m   |  1.0  |   0.95  |  0.9  |
  #+--------+---------+-------+---------+-------+
  PF_matrix$PF <- 0

  for (i in 1:nrow(PF_matrix)) {
    if (Re(PF_matrix$S_cal[i]) < 3680) {
      PF_matrix$PF[i] = 1
    } else if (Re(PF_matrix$S_cal[i]) <= 13800) {
      if (PF_matrix$line_length[i] > 0.1) {
        PF_matrix$PF[i] = 0.95
      } else PF_matrix$PF[i] = 1
    } else if (PF_matrix$line_length[i] < 0.05) {
      PF_matrix$PF[i] = 1
    } else if (PF_matrix$line_length[i] < 0.1) {
      PF_matrix$PF[i] = 0.95
    }else PF_matrix$PF[i] = 0.9
  }

  #apply power factor setting, change the S_cal value
  for (i in 1:nrow(PF_matrix)) {
    if (Mod(PF_matrix$S_cal[i]) != 0) {
    PF_matrix$S_cal[i] <- complex(real = Mod(PF_matrix$S_cal[i])*PF_matrix$PF[i], 
                                  imaginary = (sqrt(Mod(PF_matrix$S_cal[i])*PF_matrix$PF[i])^2*((1/PF_matrix$PF[i])^2 - 1)))
    }
  }
  
  S_cal <- PF_matrix$S_cal
  names(S_cal) <- PF_matrix$end
  return(S_cal)
}
  
  