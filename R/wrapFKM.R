wrapFKM <- function(d, m, start, maxit = 1e4, threshold = 0.1){
  if(length(c(m)) < 1.5){
    cat(paste("FKM calculation for m = ", m, ":\n")) 
    results <- FKM(X = d, k = ncol(start), m = m, startU = start, maxit = maxit)
    results$H <- get_H_from_fclust(results)
    pools <- get_close_clusters(results$H, threshold = threshold)
    results[["combined"]] <- pools$combined
    results[["remaining"]] <- pools$remaining
    results[["combinations"]] <- pools$combinations
    results[["n_cluster"]] <- pools$n_cluster
  }else{
    results <- list()
    for(i in 1:length(m)){
      ho1 <- proc.time()[1]
      cat(paste("FKM calculation for m = ", m[i], ":\n")) 
      results[[i]] <- FKM(X=d, k = ncol(start), m = m[i], startU = start, maxit = maxit)
      ho2 <- proc.time()[1]
      cat(paste("done in ", round(ho2 - ho1, 2), " sec.\n", sep = ""))
      results[[i]]$H <- get_H_from_fclust(results[[i]])
      pools <- get_close_clusters(results[[i]]$H, threshold = threshold)
      results[[i]][["combined"]] <- pools$combined
      results[[i]][["remaining"]] <- pools$remaining
      results[[i]][["combinations"]] <- pools$combinations
      results[[i]][["n_cluster"]] <- pools$n_cluster
    }
    att <- attributes(results)
    att$m <- m
    n_cluster <- extract(x = results, what = "n_cluster")
    att$n_cluster <- n_cluster
    if(any(abs(diff(n_cluster)) > 0.5)){
      hi <- which(abs(diff(n_cluster)) > 0.5)
      att$m_before_step <- m[c(hi, max(hi) + 1)]
      att$which_list_indexes_m_before_step <- which(m %in% att$m_before_step)
    }else{
      att$m_before_step <- att$which_list_indexes_m_before_step <- NULL
    }
    attributes(results) <- att
  }
  return(results)
}