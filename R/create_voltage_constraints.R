################################################################################
# Description:
#' this function creates the constraints given by the current       
#'
#' @title         create_voltage_constraints
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{lines}                'lines data of the grid 
#' @param  \strong{allowed_deviation}    'uncertainity of voltage calculation that is taken as security margin 
#' @param  \strong{allowed_voltage}      'allowed voltage rise 
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and c. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener/Gunther Gust         wolfgang.biener(at)ise.fraunhofer.de
################################################################################


create_voltage_constraints <- function(solution_space, big_M, allowed_voltage, verbose = 0){
  source('R/create_voltage_constraints_no_parallel.R')
  source('R/create_voltage_constraints_parallel.R')
  source('R/create_voltage_constraints_parallel_branch.R')
  # todo anpassen auf neue notation in problembeschreibung

  matrices_voltage_no_parallel <- create_voltage_constraints_no_parallel(solution_space, big_M, allowed_voltage)
  
#   View(cbind(matrices_voltage_no_parallel$A,matrices_voltage_no_parallel$b,
#              matrices_voltage_no_parallel$const.dir))

  matrices_voltage_parallel <- create_voltage_constraints_parallel(solution_space, big_M, allowed_voltage)
#   View(cbind(matrices_voltage_parallel$A,matrices_voltage_parallel$b,
#              matrices_voltage_parallel$const.dir))
#   
  matrices_voltage_parallel_branch <- create_voltage_constraints_parallel_branch(solution_space, big_M, allowed_voltage)
#   View(cbind(matrices_voltage_parallel_branch$A,matrices_voltage_parallel_branch$b,
#              matrices_voltage_parallel_branch$const.dir))
#   
  
  A <- rbind(matrices_voltage_no_parallel$A,matrices_voltage_parallel$A,matrices_voltage_parallel_branch$A)
  AT <- matrix(rep(solution_space$T$dU, nrow(A)), ncol = length(solution_space$T$dU), byrow = T)
  big_M_OLTC <- 0.6
  const.dir <- c(matrices_voltage_no_parallel$const.dir, matrices_voltage_parallel$const.dir,
                 matrices_voltage_parallel_branch$const.dir)

  
  #### check if there is OLTC
  if (any(grepl('OLTC', solution_space$T$model))) {
    print('###building matrix for OLTC###')
    ##find which column is OLTC trafo and fill it with minus BIg M
    AT_OLTC <- matrix(0, nrow = 4*nrow(AT), ncol = nrow(solution_space$T))
    colnames(AT_OLTC) <- solution_space$T$model
    for (i in 1:nrow(solution_space$T)) {
      if (grepl('OLTC', solution_space$T$model[i])) {
        AT_OLTC[1:(2*nrow(AT)), i] <- -big_M_OLTC
      }else {
        AT_OLTC[((2*nrow(AT)) + 1):nrow(AT_OLTC), i] <- -big_M_OLTC
      }
    }
    ##built them into big M matrix
    AT <- matrix(rep(t(AT), 4), ncol = ncol(AT) , byrow = TRUE ) + AT_OLTC
    A <- cbind(AT, matrix(rep(t(A), 4), ncol = ncol(A) , byrow = TRUE ))
    
    ##repeat the b matrix, it is complicated because need to check with upper (+3%) and lower voltage limit swing (-3%)
    #OLTC voltage swing allowance
    b_OLTC_swing <- 0.08
    b_diff <- b_OLTC_swing - allowed_voltage
    b_OLTC1 <- c(matrices_voltage_no_parallel$b1, matrices_voltage_no_parallel$b2, 
                 matrices_voltage_parallel$b1, matrices_voltage_parallel$b2,
                 matrices_voltage_parallel_branch$b1, matrices_voltage_parallel_branch$b2)
    b_OLTC2 <- c(matrices_voltage_no_parallel$b1 + b_diff, matrices_voltage_no_parallel$b2 - b_diff,
                 matrices_voltage_parallel$b1 + b_diff, matrices_voltage_parallel$b2 - b_diff,
                 matrices_voltage_parallel_branch$b1 + b_diff, matrices_voltage_parallel_branch$b2 - b_diff)
    b_OLTC3 <- c(matrices_voltage_no_parallel$b1 - big_M_OLTC, matrices_voltage_no_parallel$b2 - big_M_OLTC,
                 matrices_voltage_parallel$b1 - big_M_OLTC, matrices_voltage_parallel$b2 - big_M_OLTC,
                 matrices_voltage_parallel_branch$b1 - big_M_OLTC, matrices_voltage_parallel_branch$b2 - big_M_OLTC)
    b_OLTC4 <- c(matrices_voltage_no_parallel$b1 + b_diff - big_M_OLTC, matrices_voltage_no_parallel$b2 - b_diff - big_M_OLTC,
                 matrices_voltage_parallel$b1 + b_diff - big_M_OLTC, matrices_voltage_parallel$b2 - b_diff - big_M_OLTC,
                 matrices_voltage_parallel_branch$b1 + b_diff - big_M_OLTC, matrices_voltage_parallel_branch$b2 - b_diff - big_M_OLTC)
    b <- c(b_OLTC1, b_OLTC2, b_OLTC3, b_OLTC4) 
    
    #repeat const.dir
    dat <- ifelse(const.dir == '>=', 1, -1)
    const.dir <- c(dat, dat, dat*-1, dat*-1)
    const.dir <- ifelse(const.dir == 1, '>=', '<=')

    }else{
    A <- cbind(AT, A)
    b <- c(matrices_voltage_no_parallel$b1, matrices_voltage_no_parallel$b2, 
                 matrices_voltage_parallel$b1, matrices_voltage_parallel$b2,
                 matrices_voltage_parallel_branch$b1, matrices_voltage_parallel_branch$b2)
  }
  
  matrices <- list( A = A, b = b, const.dir = const.dir)
  
  return(matrices)
}

