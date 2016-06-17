getStart <- function(d, K = 10, nrep = 100, safety = TRUE, ...){
  cat("Iteration counter (one dot equals one finished iteration):\n")
  n <- nrow(d)
  start <- matrix(ncol = K, nrow = n, 0)
  if(safety){
    ## Transform warnings to errors:
    options(warn = 2)
    for(i in 1:nrep){
      tt <- ""
      class(tt) <- "try-error"
      count <- 0
      while(is(tt, "try-error")){
        tt <- try(res <- kmeans(d, centers = K, iter.max = 100, nstart = 100, trace = FALSE, ...))
        count <- count + 1
        if(count >= 1.5){cat("Safety was needed: warning in kmeans occurred, respective results are discarded.\n")}
      }
      ho <- res$cluster; rm(res)
      for(j in 1:n){
        start[j, ho[j]] <- start[j, ho[j]] + 1
      }
      cat(ifelse(i %% 40 == 0, paste(". ", i, " done.\n", sep = ""), "."))
    }
    ## Reset 'warn' parameter in options on default:
    options(warn = 0)
  }else{
    for(i in 1:nrep){
      res <- kmeans(d, centers = K, iter.max = 100, nstart = 100, trace = FALSE, ...)
      ho <- res$cluster; rm(res)
      for(j in 1:n){
        start[j, ho[j]] <- start[j, ho[j]] + 1
      }
      cat(ifelse(i %% 40 == 0, paste(". ", i, " done.\n", sep = ""), "."))
    }
  }
  start <- start/nrep
  return(start)
}