extract <- function(x, what){
  if(what %in% names(x)){
    result <- x[[what]]
  }else{
    result <- list()
    for(i in 1:length(x)){
      result[[i]] <- x[[i]][[what]]
    }
    if(all(unlist(lapply(result, length)) == 1)){
      result <- unlist(result)
    }
  }
  return(result)
}