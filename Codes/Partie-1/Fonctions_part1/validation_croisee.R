validation_croisée <- function(X,z,i){
  z <- as.data.frame(z)
  Xapp <- NULL
  zapp <- NULL
  Xtst <- NULL
  ztst <- NULL

  n <- nrow(X)
  K <- 10
  taille <- n%/%K
  
  set.seed(5)
  
  alea <- runif(n)
  rang <- rank(alea)
  bloc <- (rang-1) %/% (taille+1)
  bloc <- as.factor(bloc)
  print(summary(bloc))
  
  
  Xtst <- X[bloc!=i,]
  ztst <- z[bloc!=i,]
  union  <- cbind(Xtst,ztst)
  Xtst[sample(1:nrow(Xtst), 30, replace=FALSE), ]
  
  Xapp <- X[bloc==i,]
  zapp <- z[bloc==i,]
 
  union  <- cbind(Xtst,ztst)
  donne_reu<- union[sample(1:nrow(union), nrow(Xapp), replace=FALSE), ]
  Xtst <- donne_reu[,1:ncol(X)]
  ztst <- donne_reu[,(ncol(X)+1)]
  
res <- NULL
res$Xapp <- Xapp
res$zapp <- zapp
res$Xtst <- Xtst
res$ztst <- ztst
res
}

