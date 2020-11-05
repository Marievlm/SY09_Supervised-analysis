loiNormale <- function(X, alpha) {
  pv1 <- NULL
  pv <- matrix(data = NA, nrow = length(X) , ncol = 1)
 
  for (i in 1:length(X)){
    pvalue  <- shapiro.test(X[,i])
      if ( pvalue$p.value > alpha)
         pv[i,] <- pvalue$p.value
        
  
}
  pv1$value <- pv
  print(pv)
  }
