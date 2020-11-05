setwd("D:/utc/UV/GI02/SY09/projet2")

source("tx_erreurs_binary.R")
source("fonctions/separ1.R")
source("fonctions/Partie2_Programmation.R")


spambase2 <- read.csv("D:/utc/UV/GI02/SY09/projet2/donnees/spambase2.csv")

spambase2 <- spambase2[,2:ncol(spambase2)]

calcul_tx_erreurs_binary(spambase2,100,ncol(spambase2)-1)
