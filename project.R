library("MixSim")
library("matlib")

#--------------------- Creating samples with different distributions ----------------------
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

colors <- c("red", "green", "blue")
par(mfrow=c(2,2))
my_plot(jeu1,jeu1$id,"Sample 1")
my_plot(jeu2,jeu2$id,"Sample 2")
my_plot(jeu3,jeu3$id,"Sample 3")

#-----------------------  Discriminant Function ----------------------

discFunc <- function(x,Mu,sigma){
  return (-1/2*t(x-Mu)%*%solve(sigma)%*%(x-Mu) - 1/2 *log(det(sigma)) +log(1/2))
}

classification <- function(x,Mu,cov,k){
  id <<- matrix(0,nrow=500,ncol=1)
  for (i in 1:500){
    a = discFunc(x[i,],Mu[1,], cov[,,1])
    b = discFunc(x[i,],Mu[2,], cov[,,2])
    if(k==3) c = discFunc(x[i,],Mu[3,], cov[,,3]) else c = -Inf
    d=1
    if (a>b && a>c) 
      d=1
    else if(b>a && b>c)
      d=2
    else if(c>a && c>b)
      d=3
    
    id[i] <<- d
  }
}

#-------------- first sample

  par(mfrow=c(3,2))

  classification(jeu1$X,jeu1.Q$Mu,jeu1.Q$S,2)
  my_plot(jeu1,jeu1$id,"Data 1 - Original")
  my_plot(jeu1,id[,1], "Data 1 - Classif")

#------------------- second sample

  classification(jeu2$X,jeu2.Q$Mu,jeu2.Q$S,3)
    
  my_plot(jeu2,jeu2$id,"Data 2 - Original")
  my_plot(jeu2,id[,1], "Data 2 - Classif")
    
#------------- third sample
    
  classification(jeu3$X,jeu3.Q$Mu,jeu3.Q$S,3)
    
  my_plot(jeu3,jeu3$id,"Data 3 - Original")
  my_plot(jeu3,id[,1], "Data 3 - Classif")

  
