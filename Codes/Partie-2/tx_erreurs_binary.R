# source("fonctions/Partie2_Programmation.R")
calcul_tx_erreurs_binary <- function(X, N, nbcol){
 
   # initialisation d'une matrice pour récupérer les tx_erreurs_calculés
  m = matrix(data = NA, nrow = N, ncol = 1)
  cB <- matrix(data=NA, nrow=N, ncol =4)
  for ( i in 1:N) {
    donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst

    binary_app <- binaryNBCfit(Xapp,zapp)
    adval_binary <- binaryNBCval( binary_app,Xtst)
    vTst_binary <- adval_binary$pred
    simTst_binary <- length(which(vTst_binary==ztst))
    tx_erreur_tst_binary <- 1 - (1/nrow(Xtst)) * simTst_binary
    
    matConfBin <- table(vTst_binary, ztst )
    cB[i,] <- c(matConfBin[1,1], length(which(1==ztst))-matConfBin[1,1], length(which(2==ztst))- matConfBin[2,2], matConfBin[2,2])

    
    # sauvegarde des taux d'erreurs
    m[i,] <- tx_erreur_tst_binary
  }
  colnames(cB) <- c("1/1","1/2","2/1","2/2")
  cb <- (colSums(cB)/N)
  print("matrice de confusion Binaire")
  print(cb)
  
  colnames(m) <- c("tx_erreur_binary")
  # estimation ponctuelle
  estimation_ponctuelle = (colSums(m)/N)
  
  return(estimation_ponctuelle* 100)
}
