################################################################################
#' @title replace_trafo_types
#' @description get transformer parameter set based on its type and database Data/types.RData
#' @param lines data frame from grid data containing grid assets and its parameters
#' @param trafo_types database containing all transformer types and its specification
#' @param verbose  Verbosity level. value greater than zero to display step by step of reinforcement
#' @return lines data frame with all parameter set
################################################################################

replace_trafo_types <- function(lines, trafo_types = NA, verbose = 0) {

  #local copies
  lines$element <- as.character(lines$element)
  
  #out
  if (verbose >= 1) message("replace_trafo_types  data.frame lines")
  if (verbose >= 3) print(lines$element)
  
  #checking if data(types) has already been executed 
  if (is.na(trafo_types)) lazyLoad("types")

  #loop trafo type by rows 
  for (i in 1:nrow(trafo_types)) {
       trafo_match <- which(lines$model == trafo_types$type[i])

    #if hits
    if (length(trafo_match) > 0) {
      
      #get trafo upper and lower voltage level  from lines$lement if it's not yet exist
      if (is.null(lines$trafo_U1) | is.null(lines$trafo_U2)) {
      type_list <- strsplit(x = lines$element[which(lines$type == 'trafo')],split = ",")
      type_vector0 <- unlist(type_list)
      type_vector0 <- sub(pattern = '[)]', x = type_vector0, replacement = '')
      lines[, 'trafo_U1'] <- as.numeric(type_vector0[length(type_vector0) - 1])
      lines[, 'trafo_U2'] <- as.numeric(type_vector0[length(type_vector0)])
      }
      
      #out
      if (verbose >= 2) message(sprintf("%s: %s", trafo_types$type[i], paste(trafo_match, collapse = ", ")))
      
      #replace trafo auxiliary data
      lines[trafo_match, 'trafo_Sn'] <- trafo_types$S[i]
      lines[trafo_match, 'trafo_group'] <- trafo_types$con[i]
      lines[trafo_match, 'trafo_uk'] <- trafo_types$uk[i]
      lines[trafo_match, 'trafo_Pcu'] <- trafo_types$PCu[i]
      lines[trafo_match, 'trafo_i0'] <- trafo_types$i0[i]
      lines[trafo_match, 'trafo_Pfe'] <- trafo_types$PFe[i]
      lines[trafo_match, "max_I"] <- trafo_types$max_I[i]
      lines[trafo_match, "element"] <- as.character("trafo,x,x,x,x,x,x,x")
    }
  }

  return(lines)
}

