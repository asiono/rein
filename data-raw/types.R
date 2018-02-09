types = local({load(file = '/home/asiono/Documents/rein_lp/rein/data-raw/types.RData'); environment()})
tools:::makeLazyLoadDB(types, "types") 
devtools::use_data(types)