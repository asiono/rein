
replace_trafo_types <- function(grid, trafo_types = NULL , verbose=1) {


  #local copies
  lines <- grid$lines
  lines$element <- as.character(lines$element)
  
  #out
  if (verbose >= 1) message("replace_trafo_types  data.frame lines")
  if (verbose >= 3) print(lines$element)
  
  #checking if data(types) has already been executed 
  if (is.null(trafo_types)) {
    load(file = '~/Documents/test/types.RData')
  }
  
  #loop trafo type by rows 
  for (i in 1:nrow(trafo_types)) {
    type_list <- strsplit(x = lines$element[which(lines$type == 'trafo')],split = ",")
    type_vector0 <- unlist(type_list)
    type_vector <- type_vector0[grep("[(]",type_vector0)]
    type_vector <- sub(pattern = 'line[(]', x = type_vector, replacement = '')
    type_vector <- sub(pattern = 'trafo[(]', x = type_vector, replacement = '')
    type_vector0 <- sub(pattern = '[)]', x = type_vector0, replacement = '')
    
    #match types with element
    trafo_match <- which(type_vector == trafo_types$type[i])
    
    if (length(trafo_match) == 0) {
      trafo_match <- which(lines$model == trafo_types$type[i])
    }

    #if hits
    if (length(trafo_match) > 0) {
      lines$element[trafo_match] <- sprintf("%s%s,%s,%s%s", "trafo(", lines$model[trafo_match], 
                                            type_vector0[(length(type_vector0) - 1)], type_vector0[length(type_vector0)], ")")
      #out
      if (verbose >= 2) message(sprintf("%s: %s", trafo_types$type[i], paste(trafo_match, collapse = ", ")))
      
      #build string of specific parameter set
      sub_text <- sprintf("%s,%s,%s,%s,%s,%s", trafo_types$type[i], trafo_types$con[i],trafo_types$uk[i], trafo_types$PCu[i], trafo_types$i0[i], trafo_types$PFe[i])
      #replace unspecific element by its parameterset 
      lines$element[trafo_match] <- sub(as.character(trafo_types$type[i]), sub_text, lines$element[trafo_match])
      #replace trafo auxiliary data
      lines[trafo_match, 'trafo_Sn'] <- trafo_types$S[i]
      lines[trafo_match, 'trafo_group'] <- trafo_types$con[i]
      lines[trafo_match, 'trafo_uk'] <- trafo_types$uk[i]
      lines[trafo_match, 'trafo_Pcu'] <- trafo_types$PCu[i]
      lines[trafo_match, 'trafo_i0'] <- trafo_types$i0[i]
      lines[trafo_match, 'trafo_Pfe'] <- trafo_types$PFe[i]
      lines[, 'trafo_U1'] <- as.numeric(type_vector0[length(type_vector0) - 1])
      lines[, 'trafo_U2'] <- as.numeric(type_vector0[length(type_vector0)])
      
      #add maxI column in lines if non-existant
      if (!("max_I" %in% names(lines))) lines$max_I <- NA
      
      #find NA entries in lines for maxI
      sel <- trafo_match[which(trafo_match  %in% which(is.na(lines$max_I)))]
      lines[sel, "max_I"] <- trafo_types$max_I[i]
      
      #add empty comment line if non-existant
      if (!("comment" %in% names(lines))) lines$comment <- ""
      
      #paste comments from trafo_types
      lines[sel, "comment"] <- trafo_types$type[i]
      
      #select trafos without model description and paste
      #add model line if non-existant

      if (!("model" %in% names(lines))) lines$model <- NA
      
      sel <- trafo_match[which(trafo_match  %in% which(is.na(lines$model)))]
      lines[sel, "model"] <- trafo_types$type[i]
      
    }
  }

  grid$lines <- lines
  return(grid)
}

