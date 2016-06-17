rounded_list <- function(list, dec = 3){
  return(lapply(list, FUN = function(x){print(round(x, dec))}))
}