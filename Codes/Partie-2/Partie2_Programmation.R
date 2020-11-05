setwd("D:/utc/UV/GI02/SY09/projet2/fonctions")
source("separ1.R") # without validation set 
source("separ2.R") # with validation set 

#debugSource('D:/utc/UV/GI02/SY09/projet2/Programmation.R')
##### 2.2 programmation #####

binaryNBCfit <- function(X,z)
{
  g <- max(z)
  n <- length(z)
  p <- ncol(X)
  
  param <- NULL
  pik <- NULL
  pkj <- NULL

  
  pik<- rep(0,g)
  pkj <- matrix(0,ncol = p,nrow = g)
  zik <- matrix(0,ncol = g, nrow = n)
  for (i in 1:n) {
    zik[i,z[i]] = 1 
  }
  
  pkj <- t(zik) %*% as.matrix(X) 
  
  for (i in 1:g) {
    nk <- length(z[z == i])
    pik[i] = nk/n
    pkj[i,] <- pkj[i,]/nk
  }

  param$pik <- pik
  param$pkj <- pkj
  param
}

binaryNBCval <- function(param,Xtst)
{
  n <- dim(Xtst)[1]
  p <- dim(Xtst)[2]
  g <- length(param$pik)
  
  prob <- matrix(0,ncol = g,nrow = n)
  
  
  #prob <- as.matrix(Xtst) %*% t(param$pkj) #/ param$pik # ???
  for(k in 1:g)
  {
    pik <- param$pik[k]
    for(i in 1:n)
    {
      prob.ik <- 1
      for(j in 1:p)
      {
        pkj <- param$pkj[k,j]
        prob.ij <- pkj ** Xtst[i,j] * (1-pkj) ** (1 - Xtst[i,j])
        prob.ik <- prob.ik * prob.ij
      }
      prob[i,k] <- prob.ik * pik
    }
  }
  
  prob <- prob / apply(prob,1,sum)
  pred <- max.col(prob)
  
  out <- NULL
  
  out$prob <- prob
  out$pred <- pred

  out
}

##### 2.3 Test #####
# spambase2 <- read.csv("../donnees/spambase2.csv",header = TRUE)
# 
# 
# test <- function(data,Nmax)
# {
# 
#   min_erreur = 1.1
#   for (i in 1:Nmax) {
#     X = data[,2:(ncol(data)-1)]
#     Z = data[,ncol(data)]
#     res = separ1(X,Z)
# 
#     param = binaryNBCfit(res$Xapp, res$zapp)
#     out   = binaryNBCval(param, res$Xtst)
#     erreur = sum(abs(out$pred-res$ztst))/length(res$ztst)
#     if (erreur<min_erreur){
#       min_erreur = erreur
#     }
#   }
#     erreur
# }
# 
# test(spambase2,20)
# 
# X = spambase2[,2:(ncol(spambase2)-1)]
# Z = spambase2[,ncol(spambase2)]
# res = separ1(X,Z)
# 
# param = binaryNBCfit(res$Xapp, res$zapp)
# param$pkj
# out = binaryNBCval(param, res$Xtst)
# erreur = sum(abs(out$pred-res$ztst))/length(res$ztst)
# erreur
