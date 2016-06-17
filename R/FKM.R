FKM <- function (X, k, m, RS, startU, conv, maxit){
  if (missing(X)) 
    stop("The data set must be given.")
  if (is.null(X)) 
    stop("The data set X is empty.")
  n = nrow(X)
  p = ncol(X)
  if (is.null(rownames(X))) 
    rn = paste("Case", 1:n, sep = " ")
  else rn = rownames(X)
  if (is.null(colnames(X))) 
    cn = paste("Var", 1:p, sep = " ")
  else cn = colnames(X)
  X = as.matrix(X)
  if (any(is.na(X))) 
    stop("The data set X must not contain NA values.")
  if (!is.numeric(X)) 
    stop("The data set X is not a numeric data.frame or matrix.")
  if ((missing(startU)) || (is.null(startU))) {
    check = 1
    if (missing(k)) {
      k = 2
      cat("The default value k=2 has been set ", fill = TRUE)
    }
    if (!is.numeric(k)) {
      k = 2
      cat("The number of clusters k is not numeric: the default value k=2 will be used ", 
          fill = TRUE)
    }
    if ((k > ceiling(n/2)) || (k < 2)) {
      k = 2
      cat("The number of clusters k must be an integer in {2, 3, ..., ceiling(n/2)}: the default value k=2 will be used ", 
          fill = TRUE)
    }
    if (k%%ceiling(k) > 0) {
      k = ceiling(k)
      cat("The number of clusters k must be an integer in {2, 3, ..., ceiling(nrow(X)/2)}: the value ceiling(k) will be used ", 
          fill = TRUE)
    }
  }
  else {
    startU = as.matrix(startU)
    ns = nrow(startU)
    k = ncol(startU)
    check = 0
    if (any(is.na(startU))) {
      k = 2
      cat("The rational start must not contain NA values: the default value k=2 and a random start will be used ", 
          fill = TRUE)
      check = 1
    }
    if (!is.numeric(startU)) {
      k = 2
      cat("The rational start is not a numeric data.frame or matrix: the default value k=2 and a random start will be used ", 
          fill = TRUE)
      check = 1
    }
    if ((k > ceiling(n/2)) || (k < 2)) {
      k = 2
      cat("The number of clusters k must be an integer in {2, 3, ..., ceiling(n/2)}: the default value k=2 and a random start will be used ", 
          fill = TRUE)
      check = 1
    }
    if ((ns != n) && (check = 0)) {
      cat("The number of rows of startU is different from that of X: k=ncol(startU) and a random start will be used ", 
          fill = TRUE)
      check = 1
    }
    if (any(apply(startU, 1, sum) != 1)) {
      startU = startU/apply(startU, 1, sum)
      cat("The sums of the rows of startU must be equal to 1: the rows of startU will be normalized to unit row-wise sum ", 
          fill = TRUE)
    }
  }
  if (missing(m)) {
    m = 2
  }
  if (!is.numeric(m)) {
    m = 2
    cat("The parameter of fuzziness m is not numeric: the default value m=2 will be used ", 
        fill = TRUE)
  }
  if (m <= 1) {
    m = 2
    cat("The parameter of fuzziness m must be >1: the default value m=2 will be used ", 
        fill = TRUE)
  }
  if (missing(RS)) {
    RS = 1
  }
  if (!is.numeric(RS)) {
    cat("The number of starts RS is not numeric: the default value RS=1 will be used ", 
        fill = TRUE)
    RS = 1
  }
  if (RS < 1) {
    cat("The number of starts RS must be an integer >=1: the default value RS=1 will be used ", 
        fill = TRUE)
    RS = 1
  }
  if (RS%%ceiling(RS) > 0) {
    cat("The number of starts RS  must be an integer >=1: the value ceiling(RS) will be used ", 
        fill = TRUE)
    RS = ceiling(RS)
  }
  if (missing(conv)) 
    conv = 1e-09
  if (conv <= 0) {
    cat("The convergence criterion conv must be a (small) value >0: the default value conv=1e-9 will be used ", 
        fill = TRUE)
    conv = 1e-09
  }
  if (!is.numeric(conv)) {
    cat("The convergence criterion conv is not numeric: the default value conv=1e-9 will be used ", 
        fill = TRUE)
    conv = 1e-09
  }
  if (missing(maxit)) 
    maxit = 1e+06
  if (!is.numeric(maxit)) {
    cat("The maximum number of iterations maxit is not numeric: the default value maxit=1e+6 will be used ", 
        fill = TRUE)
    maxit = 1e+06
  }
  if (maxit <= 0) {
    cat("The maximum number of iterations maxit must be an integer >0: the default value maxit=1e+6 will be used ", 
        fill = TRUE)
    maxit = 1e+06
  }
  if (maxit%%ceiling(maxit) > 0) {
    cat("The maximum number of iterations maxit must be an integer >0: the value ceiling(maxit) will be used ", 
        fill = TRUE)
    maxit = 1e+06
  }
  value = vector(length(RS), mode = "numeric")
  ## cput = vector(length(RS), mode = "numeric")
  it = vector(length(RS), mode = "numeric")
  func.opt = 10^10 * sum(X^2)
  for (rs in 1:RS) {
    if ((rs == 1) & (check != 1)) 
      U = startU
    else {
      set.seed(rs)
      U = matrix(runif(n * k, 0, 1), nrow = n, ncol = k)
      U = U/apply(U, 1, sum)
    }
    D = matrix(0, nrow = n, ncol = k)
    H = matrix(0, nrow = k, ncol = p)
    U.old = U + 1
    iter = 0
    tX <- t(X)
    while ((sum(abs(U.old - U)) > conv) && (iter < maxit)) {
      iter <- iter + 1
      U.old <- U
      Um <- apply(U, MARGIN = 2, FUN = function(x, a){x^a}, a = m)
      for (c in 1:k){
        H[c, ] <- (t(Um[, c]) %*% X)/sum(Um[, c])
        D[, c] <- rowSums(t(tX - H[c, ])^2)
      }
      InvDm <- (1/D)^(1/(m - 1))
      InvDm <- InvDm/rowSums(InvDm)
      for (i in 1:n) {
        if (min(D[i, ]) == 0) {
          U[i, ] <- rep(0, k)
          U[i, which.min(D[i, ])] <- 1
        }else{
          U[i, ] <- InvDm[i, ]
        }
      }
    }
    func = sum((U^m) * D)
    value[rs] = func
    it[rs] = iter
    if (func < func.opt) {
      U.opt = U
      H.opt = H
      func.opt = func
    }
  }
  cat(paste("iterations needed: ", iter, "\n", sep = ""))
  rownames(H.opt) = paste("Clus", 1:k, sep = " ")
  colnames(H.opt) = cn
  rownames(U.opt) = rn
  colnames(U.opt) = rownames(H.opt)
  names(value) = paste("Start", 1:RS, sep = " ")
  out = list()
  out$U = U.opt
  out$H = H.opt
  out$value = value
  out$iter = it
  out$k = k
  out$m = m
  out$call = match.call()
  return(out)
}