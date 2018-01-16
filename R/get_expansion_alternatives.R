#todo doku

get_expansion_alternatives <- function(type, avail_asset_types, line_types = NA, trafo_types = NA) {

  if (is.na(line_types) | is.na(trafo_types)) lazyLoad('types')
  browser()
  #todo adopt it to the model
  
  if (type == 'line') {
    
    expansion_alternatives <- as.data.table(line_types[line_types$U == "0.4",  ])
    expansion_alternatives <- expansion_alternatives[type %in% avail_asset_types$line]  
    
  }
  if (type == 'trafo') {
    expansion_alternatives <- as.data.table(trafo_types[trafo_types$type %in% avail_asset_types$trafo, ])
  }
  # cleaning data frame 
  setnames(expansion_alternatives, old = "type", new = "model")
  expansion_alternatives[cost > 0, -c('comment','creation','code')]
  # it exists already in assets 
  # todo see if it ise necessary and if it can be replaced by model now
  expansion_alternatives$line_type <- expansion_alternatives$type
  
  # deleting lines and transformers not having cost data
  if (any(is.na(expansion_alternatives$cost))) {
    expansion_alternatives <- expansion_alternatives[-is.na(expansion_alternatives$cost),]
  }
  
  return(expansion_alternatives)
}


