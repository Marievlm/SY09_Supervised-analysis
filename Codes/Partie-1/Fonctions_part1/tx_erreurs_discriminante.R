source("separ1.R")
source("anadisc.R")

calcul_tx_erreurs_discriminante <- function(X, N, nbcol){
    matrice <- NULL
    # initialisation d'une matrice pour récupérer les tx_erreurs_calculés
    m <- matrix(data = NA, nrow = N, ncol = 3)
    cQ <- matrix(data=NA, nrow=N, ncol =4) 
    cL <- matrix(data=NA, nrow=N, ncol =4)
    cB <- matrix(data=NA, nrow=N, ncol =4)
    for ( i in 1:N) {
        # definition d'un nouveau jeu de données à partir du jeu fournis
        # utilisation plusieurs fois de separ1 pour ne pas que Xapp, Xval et Xtst dépendent les uns des autres
        donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
        Xapp <- donn.sep$Xapp
        zapp <- donn.sep$zapp
        
        Xtst <- donn.sep$Xtst
        ztst <- donn.sep$ztst
        
        
        # methode quadratique 
        adq_app <- adq.app(Xapp,zapp)
        adval_adq <- ad.val(adq_app,Xtst)
        vTst_adq <- adval_adq$pred
        simTst_adq <- length(which(vTst_adq==ztst))
        tx_erreur_tst_adq <- 1 - (1/nrow(Xtst)) * simTst_adq
        matConfADQ <- table(vTst_adq, ztst )
        
        
        # methode lineaire
        adl_app <- adl.app(Xapp,zapp)
        adval_adl <- ad.val(adl_app,Xtst)
        vTst_adl <- adval_adl$pred
        simTst_adl <- length(which(vTst_adl==ztst))
        tx_erreur_tst_adl <- 1 - (1/nrow(Xtst)) * simTst_adl
        matConfADL <- table(vTst_adl, ztst )
        
        
        # methode baysien naif
        nba_app <- nba.app(Xapp,zapp)
        adval_nba <- ad.val(nba_app,Xtst)
        vTst_nba <- adval_nba$pred
        simTst_nba <- length(which(vTst_nba==ztst))
        tx_erreur_tst_nba <- 1 - (1/nrow(Xtst)) * simTst_nba
        matConfADB <- table(vTst_nba, ztst )
        
        # sauvegarde des taux d'erreurs
        m[i,] <- c(tx_erreur_tst_adq,tx_erreur_tst_adl,tx_erreur_tst_nba)
    
        # sauvegarde de la matrice de confusion
        cQ[i,] <- c(matConfADQ[1,1], length(which(1==ztst))-matConfADQ[1,1], length(which(2==ztst))- matConfADQ[2,2], matConfADQ[2,2])
        cL[i,] <- c(matConfADL[1,1], length(which(1==ztst))-matConfADL[1,1], matConfADL[1,2], matConfADL[2,2])
        cB[i,] <- c(matConfADB[1,1], length(which(1==ztst))-matConfADB[1,1], length(which(2==ztst))- matConfADB[2,2], matConfADB[2,2])

    }  
    
    colnames(cQ) <- c("1/1","1/2","2/1","2/2")
    colnames(cL) <- c("1/1","1/2","2/1","2/2")
    colnames(cB) <- c("1/1","1/2","2/1","2/2")
    
    colnames(m) <- c("tx_erreur_adq","tx_erreur_adl","tx_erreur_nba")
    
    # estimation ponctuelle
    estimation_ponctuelle <- (colSums(m)/N)*100
    
    # variance des donn?es
    variance <- var(m) 
    
    #estimation matrice de confusion
    cq <- (colSums(cQ)/N)
    
    cl <- (colSums(cL)/N)
    cb <- (colSums(cB)/N)
    
    
    # impression des donn?es
    print("matrice de covariance")
    print(variance)
    print("estimation ponctuelle de taux d'erreur ")
    print(estimation_ponctuelle)
    print("matrice de confusion ADQ")
    print(cq)
    print("matrice de confusion ADL")
    print(cl)
    print("matrice de confusion Bayesien")
    print(cb)
    matrice$m <- m
    matrice$cQ <- cQ
    matrice$cL <- cL
    matrice$cB<- cB
    #matrice
    
    }
