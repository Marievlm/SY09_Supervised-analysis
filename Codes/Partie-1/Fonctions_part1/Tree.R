Tree_arbr <- function (X, z, N) 
  {
  m = matrix(data = NA, nrow = N, ncol = 1)
  confMat <- matrix(data=NA, nrow=N, ncol =4)
  for (i in 1:N){
    
      mat <- as.matrix(cbind(X,z))
     
      donn.sep <- separ1(mat[,1:dim(X)[2]], mat[,dim(X)[2]+1])
      Xapp <- donn.sep$Xapp
      zapp <- donn.sep$zapp
      
      Xtst <- donn.sep$Xtst
      ztst <- donn.sep$ztst
      
      tree <- rpart(zapp~.,data = as.data.frame(Xapp) , method = "class", control=rpart.control(minsplit=5,cp=0))
      
      
        plotcp(tree)
      
      #print(tree$cptable[which.min(tree$cptable[,4]),1])
      tree_Opt <- prune(tree,cp=tree$cptable[which.min(tree$cptable[,4]),1])
      rpart.plot::rpart.plot(tree_Opt)
      
      
      #Validation
      
      #Pr?diction du mod?le sur les donn?es de test
      test_Predict <- predict(tree_Opt,newdata= as.data.frame(Xtst) , type = "class" )
      #print(test_Predict)
      #print(ztst)
      #print(length(which(ztst==1)))
      #Matrice de confusion
      mc<-table(test_Predict,ztst)
      #print(mc)
    
      #Erreur de classement
      erreur <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)
      #print(erreur)
      
        #Taux de pr?diction
      prediction=mc[2,2]/sum(mc[2,])
      #print(prediction)
      
      #Affichage des r?gles de construction de l'arbre
      #print(tree_Opt)

      # sauvegarde des erreurs
      m[i,] <- erreur
      confMat[i, ] <-  c(mc[1,1], mc[2,1], mc[1,2], mc[2,2])
      colnames(confMat) <- c("1/1","1/2","2/1","2/2")
  }
  
  #estimation 
  Tree <- NULL
  
  estimation_ponctuelle <- (colSums(m)/N)
  estimation_conf <- (colSums(confMat)/N)
  
  Tree$Mat_erreur <- m
  Tree$confusion <- confMat
  Tree$Err_Moy <- estimation_ponctuelle
  Tree$Conf_Moy <- estimation_conf
  Tree$regle <- tree_Opt
  Tree
}
  
  