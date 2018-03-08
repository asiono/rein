################################################################################
#' @title replace_line_types
#' @description get line parameter set based on its type and database Data/types.RData
#' @param lines data frame from grid data containing grid assets and its parameters
#' @param line_types database containing all line types and its specification
#' @param verbose  Verbosity level. value greater than zero to display step by step of reinforcement
#' @return lines data frame with all parameter set
################################################################################

replace_line_types <- function(lines, line_types = NA, verbose = 1) {
  lines$element <- as.character(lines$element)

  if (verbose >= 1) message("replace_line_types  data.frame lines")
  if (verbose >= 3) print(lines$element) 
  
  #checking if data(types) has already been executed 
  if (is.na(line_types)) lazyLoad('types')
  
  # get lines length from lines$element if it's not yet present
  if (is.null(lines$line_l)) {
    lines$line_l <- c()
    for (j in 1:nrow(lines)) {
      type_list <- strsplit(x = lines$element[j],split = ",")
      type_vector0 <- unlist(type_list)
      type_vector0 <- sub(pattern = '[)]', x = type_vector0, replacement = '')
      lines$line_l[j] <- as.numeric(type_vector0[length(type_vector0)])
    }
  }
  
  #loop line type rows
  for (i in nrow(line_types):1) {
    #searching matching elements in table
    line_match <- which(lines$model == line_types$type[i])
    
    if (length(line_match) > 0) {
      
      if (verbose >= 2) message(sprintf("%s: %s", line_types$type[i], paste(line_match, collapse = ", ")))
      
      #replacing the RLGCl data
      lines[line_match, 'line_R'] <- line_types$R[i]
      lines[line_match, 'line_L'] <- line_types$L[i]
      lines[line_match, 'line_G'] <- line_types$G[i]
      lines[line_match, 'line_C'] <- line_types$C[i]
      lines[line_match, "max_I"] <- line_types$max_I[i]
    }
  }
  
  if (any(is.na(lines$element))) {
    lines$element[which(is.na(lines$element) & lines$type != "trafo")] <- "line"
  }

  return(lines)
}

