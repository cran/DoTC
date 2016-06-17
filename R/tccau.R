tccau <- function(fkm){
  U <- extract(fkm, "U")
  co <- extract(fkm, "combinations")
  re <- extract(fkm, "remaining")
  if(all(!is.na(co))){
    nk <- unlist(lapply(co, length))
    Uparts <- list()
    for(i in 1:length(co)){
      Uparts[[i]] <- (max(nk)/nk[i]) * rowSums(U[, co[[i]]])
    }
    if(all(!is.na(re))){
      Uparts[[length(Uparts)+1]] <- max(nk)*U[, re]
    }
    U <- do.call(cbind, Uparts)
  }
  rs <- rowSums(U)
  tc <- ca <- rep(0, length(rs)) ## typicality index ## cz cluster-zugehoerigkeit
  if(ncol(U) > 1){
    for(i in 1:nrow(U)){
      U[i, ] <- U[i, ]/rs[i]
      tc[i] <- abs(diff(sort(U[i, ], decreasing = TRUE)[1:2]))
      ca[i] <- as.numeric(which.max(U[i, ]))
    }
  }else{
    cat("warning: typicality index makes no sense for one cluster solution.\n")
    ca <- ca + 1 
  }
  colnames(U) <- paste("cluster", 1:ncol(U), sep = "")
  results <- list(tc = tc, 
                  ca = ca, 
                  U = U)
  return(results)
}