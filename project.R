library("MixSim")

#--------------------- Sampling Data ----------------------
# jeu 1
jeu1.Q <- MixSim(MaxOmega = 0.0, BarOmega = 0.0, K = 2, p = 2, sph = TRUE)
jeu1 <- simdataset(n = 500, Pi = jeu1.Q$Pi, Mu = jeu1.Q$Mu, S = jeu1.Q$S)

#jeu 2
jeu2.Q <- MixSim(MaxOmega = 0.10, BarOmega = 0.05, K = 3, p = 2, sph = TRUE)
jeu2 <- simdataset(n = 500, Pi = jeu2.Q$Pi, Mu = jeu2.Q$Mu, S = jeu2.Q$S)

#jeu 3
jeu3.Q <- MixSim(MaxOmega = 0.10, BarOmega = 0.05, K = 3, p = 2, sph = FALSE)
jeu3 <- simdataset(n = 500, Pi = jeu3.Q$Pi, Mu = jeu3.Q$Mu, S = jeu3.Q$S)

#Quick view on the samples

my_plot = function(jeu, predicts, title){
  plot(jeu$X, col = colors[predicts], pch = 19, cex = 0.8, 
       xlab = "", ylab = "", axes = F, main = title)
  box()
}

par(mfrow=c(2,2))
my_plot(jeu1,jeu1$id,"Sample 1")
my_plot(jeu2,jeu2$id,"Sample 2")
my_plot(jeu3,jeu3$id,"Sample 3")

#-----------------------  Discriminant Functions ----------------------
#Fonctions de classif
classification1 = function(x,Mu,sigma){
  #Dans notre cas : lnP(ci ) = lnP(cj )
  # donc : g_i(x)-|x-mu_i|^2
  return(-det(x-Mu)^2)
}

classification2 = function(x,Mu,sigma){
  return(t(Mu)%*%inv(sigma)*x + (log(1/2)-1/2*t(Mu)%*%inv(sigma)%*%Mu))
}

classification3 <- function(x,Mu,sigma){
  return (-1/2*t(x-Mu)%*%solve(sigma)%*%(x-Mu) - 1/2 *log(det(sigma)) +log(1/2))
}

maxClassif = function(jeu#Jeu = JDD / classif (1, 2 ou 3) choix de la fonction classif
                      ,classif,x){
    if(classif == 1){
      print(jeu$Mu)
      a = classification1(x,jeu$Mu[1,], jeu$S[,,1])
      b = classification1(x,jeu$Mu[2,], jeu$S[,,2])
      if (a>b) {
        return(1)
      }else{
        return(2)
      }
      
    } else if (classif == 2 ){
      a = classification2(x,jeu$Mu[1,], jeu$S[,,1])
      b = classification2(x,jeu$Mu[2,], jeu$S[,,2])
      c = classification2(x,jeu$Mu[3,], jeu$S[,,3])
      
    } else if (classif == 3){
      a = classification3(x,jeu$Mu[1,], jeu$S[,,1])
      b = classification3(x,jeu$Mu[2,], jeu$S[,,2])
      c = classification3(x,jeu$Mu[3,], jeu$S[,,3])
      
    }
  
  if (a>b && a>c) {
    return(1)
  }else if(b>a && b>c){
    return(2)
  }else if(c>a && c>b){
    return(3)
  }
}


#id
id <- matrix(0,nrow=500,ncol=9) # col = 9 car on aura 9 tests ;
# col 1 : Jeu1 + classif1
# col 2 : Jeu1 + classif2
# col 3 : Jeu1 + classif3
# col 4 : Jeu2 + classif1
# col 5 : Jeu2 + classif2
# col 6 : Jeu2 + classif3
# col 7 : Jeu3 + classif1
# col 8 : Jeu3 + classif2
# col 9 : Jeu3 + classif3

for (i in 1:500) {
  id[i,1] = maxClassif(jeu1.Q, 1, jeu1$X[i,])
  id[i,2] = maxClassif(jeu1.Q, 2, jeu1$X[i,])
  id[i,3] = maxClassif(jeu1.Q, 3, jeu1$X[i,])
  id[i,4] = maxClassif(jeu2.Q, 1, jeu2$X[i,])
  id[i,5] = maxClassif(jeu2.Q, 2, jeu2$X[i,])
  id[i,6] = maxClassif(jeu2.Q, 3, jeu2$X[i,])
  id[i,7] = maxClassif(jeu3.Q, 1, jeu3$X[i,])
  id[i,8] = maxClassif(jeu3.Q, 2, jeu3$X[i,])
  id[i,9] = maxClassif(jeu3.Q, 3, jeu3$X[i,])
}

#-----------------------  Visualization ----------------------

colors <- c("red", "green", "blue")

plotRealAndPredict = function(jeu, predicts){
  par(mfrow = c(2,2),mar = c(1.4, 1.4, 1.4, 1.4))
  # Premiere classif
  my_plot(jeu,jeu$id,"Original")
  my_plot(jeu,predicts[,1], "Classif_1")
  
  # 2eme 
  my_plot(jeu,predicts[,2],"Classif_2")
  
  # 3eme
  my_plot(jeu,predicts[,3],"Classif_3")
}

plotRealAndPredict(jeu1,id[,c(1:3)])
plotRealAndPredict(jeu2,id[,c(4:6)])
plotRealAndPredict(jeu3,id[,c(7:9)])


#-----------------------   ----------------------   ----------------------
#-----------------------   ----------------------   ----------------------

# par(mfrow = c(1,2),mar = c(0.1, 0.1, 0.1, 0.1))
# 
# plot(jeu1$X, col = colors[jeu1$id], pch = 19, cex = 0.8, 
#      xlab = "", ylab = "", axes = FALSE)
# par(mar = c(0.1, 0.1, 0.1, 0.1))
# plot(jeu1$X, col = colors[id], pch = 19, cex = 0.8, 
#      xlab = "", ylab = "", axes = FALSE)
# table(jeu1$id,id)


# maxClassif = function(x,Mu,sigma){
#   a = classification3(x,jeu1.Q$Mu[1,], jeu1.Q$S[,,1])
#   b = classification3(x,jeu1.Q$Mu[2,], jeu1.Q$S[,,2])
#   if (a>b) {
#     return(1)
#   }else{
#     return(2)
#   }
# }
#id = apply(jeu1$X,FUN = maxClassif,MARGIN = F)

#plotRealAndPredict = function(jeu, predicts){
#  par(mfrow = c(3,2),mar = c(0.1, 0.1, 0.1, 0.1))
#  # Premiere classif
#  my_plot(jeu,jeu$id,"")
#  my_plot(jeu,predict[,1],"")
#  
#  # 2eme 
#  my_plot(jeu,jeu$id,"")
#  my_plot(jeu,predict[,2],"")
#  
#  # 3eme
#  my_plot(jeu,jeu$id,"")
#  my_plot(jeu,predict[,3],"")
# 
#  #plot(jeu$X, col = colors[jeu$id], pch = 19, cex = 0.8, xlab = "", ylab = "", axes = F, main = "Classif3 real")
#  #plot(jeu$X, col = colors[predict[,3]], pch = 19, cex = 0.8, xlab = "", ylab = "", axes = F, main = "Classif3 predict")
#}

### Function 
classification <- function(x,Mu,sigma){
  return (-1/2*t(x-Mu)%*%solve(sigma)%*%(x-Mu) - 1/2 *log(det(sigma)) +log(1/2))
}

#-----------------------   ----------------------   ----------------------
#-----------------------   ----------------------   ----------------------
