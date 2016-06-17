plotTC <- function(fkm, main = NULL){
  counts <- Cluster <- pos <- fontsize <- NULL
  co <- extract(fkm, "combinations")
  re <- extract(fkm, "remaining")
  if(all(!is.na(co))){
    nk <- length(co) + length(re)    
  }else{
    nk <- length(re)
  }
  ho <- tccau(fkm)
  tc <- ho$tc
  ca <- ho$ca
  ho <- list()
  for(i in 1:nk){
    ho[[i]] <- hist(tc[ca == i], breaks = seq(0, 1, by = 0.1), plot = F)$counts
  }
  ho <- do.call(rbind, ho)
  ho2 <- data.frame(tc = rep(seq(0.05, 0.95, by = 0.1), nk), 
                    Cluster = factor(rep(1:nk, each = 10)), 
                    counts = as.numeric(t(ho)))
  ho2 <- ddply(ho2, c("tc"), transform, pos = cumsum(counts) - (0.5 * counts))
  ho2$fontsize <- c(NA, 1)[1 + (ho2$counts > 0)]
  ho <- seq(0.3, 0.7, length = nk)
  col <- apply(cbind(0.0+0*ho,ho,0.8*rev(ho)), MARGIN = 1, FUN = function(x){rgb(x[1], x[2], x[3])})
  if(is.null(main)){
    main <- paste("m = ", as.numeric(fkm$m), sep = "")
  }
  result <- ggplot(ho2, aes(x = tc, y = counts, fill = Cluster)) + 
    geom_bar(stat = "identity") + ggtitle(main) +
    geom_bar(stat = "identity", colour = rgb(0.3, 0.3, 0.3), show.legend = FALSE) +##show_guide=FALSE) +
    scale_fill_manual(values = col) + 
    geom_text(aes(label = Cluster, y = pos, size = fontsize), fontface="bold", show.legend = FALSE) +##show_guide = F) + 
    scale_x_continuous("Typicality Coefficient") +
    scale_y_continuous("Frequency")
  return(result)
}