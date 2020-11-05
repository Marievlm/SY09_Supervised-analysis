library(readr)
library("FactoMineR")
library("factoextra")
library(MASS)
library("tree")
library(rpart)
library(rpart.plot)

#setwd("./fonctions/TP8_TP9")
source("logistic.R")
source("mvdnorm.R")
source("prob.ad.R")
source("prob.log.R")
source("prob.log2.R")
source("separ1.R")
source("tx_erreurs_discriminante.R")
source("tx_erreurs_logistique.R")
source("analyse.R")
source("separ1.R")
source("Analyse_Disc.R")
source("LogisReg2.R")
source("Tree.R")
source("tx_erreurs_CV_discriminante.R")
source ("test_loi_normale.R")
source ("validation_croisee.R")



##### breastcancer #####
breastcancer <- read.csv("breastcancer.csv")
summary(breastcancer)
dim(breastcancer)

X <- breastcancer[,3:ncol(breastcancer)-1 ] # probl?me avec X14 pour regression logistiques colonne 15
z <- breastcancer[, ncol(breastcancer)]
x11()
plot(X[,1:12], col=z)
N <- 100


# ACP 
res.pca <- PCA(X,graph = FALSE)
fviz_pca_ind(res.pca,col.ind = z,label = F)
fviz_pca_ind(res.pca,axes = c(1,3),col.ind = z,label = F)
fviz_eig(res.pca, addlabels = TRUE)
fviz_pca_var(res.pca, col.var = "cos2", repel = TRUE) # ?vite le chevauchement de texte)
indiviPCA <- res.pca$ind$coord[,1:2]
fviz_pca_ind(res.pca, geom.ind = "point", col.ind =z)

calcul_tx_erreurs_discriminante(as.matrix(cbind(indiviPCA,z)), N, dim(indiviPCA)[2])

# r?alisation d un test de student
# loi normale 


# calcul taux d'erreurs
calcul_tx_erreurs_discriminante(as.matrix(cbind(X,z)), N, dim(X)[2])

AnalyseDisc(X,z,20,graph = F) # faire un test de significativit? des r?sultats

# analyse de la regression
calcul_tx_erreurs_logistique(as.matrix(cbind(X,z)), N, dim(X)[2], intr=T, 1e-5,doLogQuad=F)
calcul_tx_erreurs_logistique(as.matrix(cbind(X,z)), N, dim(X)[2], intr=F, 1e-5,doLogQuad=F)

# calcul tree
Tree_arbr(X,z,N) # est plus robuste et compl?te 


##### ionosphere  #####     
iono <- read.csv("ionosphere.csv")
X <- iono[,c( 4:(ncol(iono)-1))] # colonne 2 supprim?e
z <- iono[,(ncol(iono))]
plot(X[,1:12],col = z)

#variables centrées réduites
X1 <- (X [which(z==1),]- apply (X [which(z==1),],2, mean ))/ apply (X [which(z==1),],2,var )
X2 <- (X [which(z==2),]- apply (X [which(z==2),],2, mean ))/ apply (X [which(z==2),],2, var )
w <- rbind(X1,X2)
plot(w[,1:12],col = z)

# r?alisation d'une ACP pour mieux comprendre
res.pca <- PCA(X, graph = FALSE,ncp = dim(X)[2])
indiviPCA <- res.pca$ind$coord[,1:2]
fviz_pca_ind(res.pca,col.ind = z)
fviz_eig(res.pca, addlabels = TRUE)
fviz_pca_var(res.pca, col.var = "cos2", repel = TRUE) # ?vite le chevauchement de texte)
# X <- indiviPCA #l'inertie explique par les 2 premieres composants sont pas suffisant 

#execution des mod?les
calcul_tx_erreurs_discriminante(as.matrix(cbind(X,z)), N, dim(X)[2]) #donnees brutes
calcul_tx_erreurs_discriminante(as.matrix(cbind(w,z)), N, dim(X)[2]) #donnees centree

LogisReg2(X, z, intr=F, desc =F ,graph= T,0.00001, N, doLogQuad =F)
calcul_tx_erreurs_logistique(as.matrix(cbind(X,z)), N, dim(X)[2], intr = T, 1e-5,doLogQuad=FALSE)#####??? 16 #### 
calcul_tx_erreurs_logistique(as.matrix(cbind(w,z)), N, dim(w)[2], intr = T, 1e-5,doLogQuad=FALSE)#####??? ne marche pas ####

Tree_arbr(X,z,N)
Tree_arbr(w,z,N)

###### sonar #####

sonar <- read.csv("sonar.csv")
X <- sonar[, 2:(ncol(sonar)-1)]
z <- sonar [,ncol(sonar)]
plot(X[,1:8], col=z)

X1 <- (X [which(z==1),]- apply (X [which(z==1),],2, mean ))#apply (X [which(z==1),],2, var ) 
X2 <- (X [which(z==2),]- apply (X [which(z==2),],2, mean )) # apply (X [which(z==2),],2, var ) 
w<- rbind(X1,X2)
plot(w[,1:8], col=z)

X1 <- (X [which(z==1),]- apply (X [which(z==1),],2, mean ))/apply (X [which(z==1),],2, var ) 
X2 <- (X [which(z==2),]- apply (X [which(z==2),],2, mean )) / apply (X [which(z==2),],2, var ) 
w<- rbind(X1,X2)
plot(w[,1:8], col=z)

#execution des mod?les
calcul_tx_erreurs_discriminante(as.matrix(cbind(X,z)), N, dim(X)[2]) 
calcul_tx_erreurs_discriminante(as.matrix(cbind(w,z)), N, dim(w)[2])
calcul_tx_erreurs_logistique(as.matrix(cbind(X,z)), 100, dim(X)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)
calcul_tx_erreurs_logistique(as.matrix(cbind(w,z)), 100, dim(w)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)
Tree_arbr(X,z,N)
Tree_arbr(w,z,N)
#ACP, quelles sont les variables pesant
res.pca <- PCA(X, graph = FALSE)
indiviPCA <- res.pca$ind$coord[,1:2]
fviz_pca_ind(res.pca,col.ind = z)
fviz_eig(res.pca, addlabels = TRUE)
fviz_pca_var(res.pca, col.var = "cos2")

res.pca1 <- PCA(X1, graph = FALSE)
indiviPCA1 <- res.pca1$ind$coord[,1:2]
fviz_pca_ind(res.pca1)
fviz_eig(res.pca1, addlabels = TRUE)
fviz_pca_var(res.pca1, col.var = "cos2")

res.pca2 <- PCA(X2, graph = FALSE)
indiviPCA2 <- res.pca2$ind$coord[,1:2]
fviz_pca_ind(res.pca2)
fviz_eig(res.pca2, addlabels = TRUE)
fviz_pca_var(res.pca2, col.var = "cos2")

Contribution.PCA1<- as.data.frame (res.pca1$var$contrib[,1:2])
PCA1Axe1 <- row.names.data.frame(Contribution.PCA1[which(Contribution.PCA1$Dim.1 >3),])
PCA1Axe2 <- row.names.data.frame(Contribution.PCA1[which(Contribution.PCA1$Dim.2 >3),])

Contribution.PCA2<- as.data.frame (res.pca2$var$contrib[,1:2])
PCA2Axe1 <- row.names.data.frame(Contribution.PCA2[which(Contribution.PCA2$Dim.1 >3),])
PCA2Axe2 <- row.names.data.frame(Contribution.PCA2[which(Contribution.PCA2$Dim.2 >3),])

data2 <- data.frame(cbind(PCA2Axe2,PCA2Axe1))
data1 <- data.frame(cbind(PCA1Axe1,PCA1Axe2))
data <- cbind(data2,data1) #selection des vecteurs

Xred <- X[,c(1,3,4,5,11,48,50,51,53,54,55,58,60)] #vecteur choisies
calcul_tx_erreurs_logistique(as.matrix(cbind(Xred,z)), 100, dim(Xred)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)
calcul_tx_erreurs_logistique(as.matrix(cbind(wred,z)), 100, dim(wred)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)

calcul_tx_erreurs_discriminante(as.matrix(cbind(Xred,z)), N, dim(Xred)[2])
calcul_tx_erreurs_discriminante(as.matrix(cbind(wred,z)), N, dim(wred)[2])

X1red <- (Xred [which(z==1),]- apply (Xred [which(z==1),],2, mean ))
X2red <- (Xred [which(z==2),]- apply (Xred [which(z==2),],2, mean )) 
wred<- rbind(X1red,X2red)
plot(wred[,1:8], col=z)
Tree_arbr(Xred,z,N)

#test loi normale
X1n <- (X [which(z==1),])
loiNormale(X1n,0.05)
X2n <- (X [which(z==2),])
loiNormale(X2n,0.05)#x 30 et 33
XXn <- X[,c(30,33)]
plot(XXn, col=z)
calcul_tx_erreurs_discriminante(as.matrix(cbind(XXn,z)), N, dim(XXn)[2])

###### spambase #####
spambase <- read.csv("spambase.csv")
X <- spambase[, 2:(ncol(spambase)-1)]
z <- spambase [,ncol(spambase)]
summary(X)

X1 <- (X [which(z==1),]- apply (X [which(z==1),],2, mean ))/apply (X [which(z==1),],2, var ) 
X2 <- (X [which(z==2),]- apply (X [which(z==2),],2, mean ))/apply (X [which(z==2),],2, var ) 
w<- rbind(X1,X2)
plot(X[, 1:5], col=z)
plot(w[, 1:5], col=z)

res.pca <- PCA(X, graph = FALSE,ncp = dim(X)[2])
indiviPCA <- res.pca$ind$coord[,1:2]
fviz_pca_ind(res.pca,col.ind = z)
fviz_eig(res.pca, addlabels = TRUE)
fviz_pca_var(res.pca, col.var = "cos2")

XPCA <- res.pca$ind$coord[,1:50]

#execution des mod?les
calcul_tx_erreurs_discriminante(as.matrix(cbind(X,z)), N, dim(X)[2]) #donn?es brutes
calcul_tx_erreurs_discriminante(as.matrix(cbind(w,z)), N, dim(w)[2]) #donn?es centr?e
calcul_tx_erreurs_discriminante(as.matrix(cbind(XPCA,z)), N, dim(XPCA)[2])
calcul_tx_erreurs_logistique(as.matrix(cbind(X,z)), 5, dim(X)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)
calcul_tx_erreurs_logistique(as.matrix(cbind(XPCA,z)), 5, dim(XPCA)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)
calcul_tx_erreurs_logistique(as.matrix(cbind(w,z)), 5, dim(w)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)
# calcul_tx_erreurs_logistique(as.matrix(cbind(w,z)), 5, dim(w)[2], intr = TRUE, 1e-5,doLogQuad=FALSE)

Tree_arbr(w,z,100)
Tree_arbr(X,z,100)


###### spambase2 #####
spambase2 <- read.csv("spambase2.csv")
X <- spambase2[, 2:(ncol(spambase2)-1)]
z <- spambase2[,ncol(spambase2)]
X1 <- (X [which(z==1),]- apply (X [which(z==1),],2, mean ))/apply (X [which(z==1),],2, var ) # donn?es centr?es
X2 <- (X [which(z==2),]- apply (X [which(z==2),],2, mean )) / apply (X [which(z==2),],2, var ) #donn?es centr?es
w<- rbind(X1,X2)
summary(X)

res.pca <- PCA(X, graph = FALSE,ncp = dim(X)[2])
Xpca <- res.pca$ind$coord[,1:50]

calcul_tx_erreurs_discriminante(as.matrix(cbind(X,z)), N, dim(X)[2]) #9.923679      7.726027     11.415525 
calcul_tx_erreurs_discriminante(as.matrix(cbind(w,z)), N, dim(w)[2]) #14.78474      16.52316      14.78082 
calcul_tx_erreurs_discriminante(as.matrix(cbind(Xpca,z)), N, dim(Xpca)[2]) #14.78474    

calcul_tx_erreurs_logistique(as.matrix(cbind(X,z)), N, dim(X)[2], intr = TRUE, 1e-5,doLogQuad=FALSE) #6.7
calcul_tx_erreurs_logistique(as.matrix(cbind(w,z)), N, dim(w)[2], intr = TRUE, 1e-5,doLogQuad=FALSE) # 6.15786 
calcul_tx_erreurs_logistique(as.matrix(cbind(Xpca,z)), N, dim(Xpca)[2], intr = TRUE, 1e-5,doLogQuad=FALSE) # 6.15786 

Tree_arbr(w,z,100)
Tree_arbr(X,z,100)
Tree_arbr(Xpca,z,100)


