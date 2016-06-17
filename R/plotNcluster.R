plotNcluster <- function(fkm, ...){
  att <- attributes(fkm)
  m <- att$m
  if(length(m) > 1.5){
    plot(m, att$n_cluster, type = "n", ylab = "Number of Clusters", 
       xlab = "Fuzziness Parameter m", bty = "l", ...)
    if(sum(abs(diff(att$n_cluster))) > 0.5){
      for(i in 1:length(att$m_before_step)){
        lines(rep(att$m_before_step[i], 2), 
              c(att$n_cluster[att$which_list_indexes_m_before_step[i]], 0), 
              lty = 2, col = "grey")
      }
      lines(m, att$n_cluster, type = "S")
      points(att$m_before_step, att$n_cluster[att$which_list_indexes_m_before_step], pch = 16)
    }else{
      lines(m, att$n_cluster)
      warning("no differences in n_cluster to be illustrated.")
    }
  }else{
    plot(fkm$m, fkm$n_cluster, pch = 16, ylab = "Number of Clusters", 
         xlab = "Fuzziness Parameter m", bty = "l", ...)
    warning("m is only a single value, no development of n_cluster to be plotted.")
    }
}