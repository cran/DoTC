plotCS <- function(fkm, which_clusters = NULL, 
                   colors = NULL, 
                   main = ""){
  if(fkm$n_cluster < 1.5){
    stop("Pairwise comparisons of cluster segregation not possible for one-cluster solution.")
    }
  if(is.null(which_clusters)){
    which_clusters <- 1:fkm$n_cluster
    if(fkm$n_cluster > 6.5){
      which_clusters <- 1:6
      warning("To avoid over-loaded plotting, which_clusters is set to 1:6.\n  Please specify which_clusters manually if more/other pair-wise comparisons should be plottet.")
    }
  }
  if(is.null(colors)){
    min_rgb <- 0.3
    max_rgb <- 0.7
    k <- length(which_clusters)
    #if((k %/% 3) > 0){
    #  l <- k %/% 3
    #}else{
    #  l <- 1 + (k %/% 3)  
    #}
    if(k < 8.5){
      l <- 2
    }
    if((k > 8.5) & (k < 27.5)){
      l <- 3
    }
    if(k > 27.5){
      stop("Not supported for more than 27 clusters.")
    }
    p <- seq(min_rgb, max_rgb, length = l)
    for(i in 1:k){
      ho <- p[as.numeric(expand.grid(1:l, 1:l, 1:l)[i, ])]
      colors <- c(colors, rgb(ho[1], ho[2], ho[3], alpha = 0.5))
    }
  }
  ho <- tccau(fkm)
  U <- ho$U
  ca <- ho$ca
  combis <- combn(which_clusters, m = 2)
  combis <- as.data.frame(t(combis))
  names(combis) <- c("x", "y")
  combis <- arrange(combis, x)
  combis <- arrange(combis, desc(y))
  combis <- t(as.matrix(combis))
  mat <- matrix(nrow = length(which_clusters)-1, ncol = length(which_clusters)-1, 0, byrow = T)
  count1 <- 1
  count2 <- count3 <- ncol(mat)
  for(i in 1:nrow(mat)){
    mat[i, 1:count3] <- count1:count2
    count1 <- count2 + 1
    count3 <- count3 - 1
    count2 <- count2 + count3
  }
  ##mat <- mat[, ncol(mat):1]
  layout(mat = mat)
  for(i in 1:ncol(combis)){
    which_clusters <- combis[, i]
    i1 <- combis[1, i]
    i2 <- combis[2, i]
    ## i1 <- which_clusters[1]
    ## i2 <- which_clusters[2]
    x <- U[, i1]
    y <- U[, i2]
    #if(c(i2 > 7.5)){
    #  print(paste(i1, ",", i2, "\n", sep = ""))
    #  print(x)
    #}
    hi <- which(ca %in% which_clusters)
    x <- x[hi]
    y <- y[hi]
    index_for_colors <- 1 + as.numeric(ca[hi] == i2)
    plot(x, y, axes = F, xlim=c(0, 1), ylim = c(0, 1), pch = 16, 
         col = colors[c(i1, i2)[index_for_colors]], 
         xlab = i1, ylab = i2)
    axis(1, pos = 0)
    axis(2, pos = 0) 
    lines(c(0, 1), c(1, 0))
    lines(c(0, 0.5), c(0, 0.5), lty = 2)
  }
}