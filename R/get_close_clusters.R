get_close_clusters <- function(H, threshold) {
  if(any(H < threshold)){
    H <- as.matrix(H)
    for (i in 1:nrow(H)) {
      H[i, i:ncol(H)] <- NA
    }
    hi <- which(H < threshold, arr.ind = T)
    for (i in 1:nrow(hi)) {
      hi[i,] <- sort(hi[i,])
    }
    combinations <- NULL
    count <- 1
    for (i in 1:ncol(H)) {
      combines_with <- NULL
      for (i2 in 1:nrow(hi)) {
        if (i %in% hi[i2,]) {
          combines_with <- sort(unique(c(combines_with, hi[i2,])))
        }
      }
      if (!is.null(combines_with)) {
        combinations[[count]] <- combines_with
        count <- count + 1
      }
    }
    combinations_copy <- combinations
    ho1 <- max(unlist(lapply(combinations_copy, FUN = length)))
    for(i in 1:length(combinations_copy)){
      ho2 <- length(combinations_copy[[i]])
      if(ho2 != ho1){
        combinations_copy[[i]] <- c(combinations_copy[[i]], rep(NA, ho1-ho2))
      }
    }
    combinations_copy <- unique(do.call(rbind, combinations_copy))
    ho <- list()
    for(i in 1:nrow(combinations_copy)){
      ho[[i]] <- combinations_copy[i, ]
      ho[[i]] <- ho[[i]][!is.na(ho[[i]])]
    }
    combinations <- ho
    rm(combinations_copy)
    colnames(hi) <- c("cluster 1", "cluster 2")
    rownames(hi) <- paste0("comb. ", 1:nrow(hi), ": ")
    hi2 <- unique(as.numeric(hi))
    remaining <- c(1:ncol(H))[-hi2]
    ## n_cluster:
    n_cluster <- length(remaining)
    if (!is.null(combinations)) {
      n_cluster <- n_cluster + length(combinations)
    }
    if(length(hi2) == ncol(H)){
      remaining <- NA
    }
    results <- list(combined = hi, 
                    remaining = remaining, 
                    combinations = combinations, 
                    n_cluster = n_cluster)
  }else{
    results <- list(combined = NA, 
                    remaining = 1:ncol(as.matrix(H)), 
                    combinations = NA, 
                    n_cluster = ncol(as.matrix(H)))
  }
  return(results)
}