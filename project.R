## Exemple
library(mvtnorm)
x.points <- seq(-3,3,length.out=100) 
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(1,1)
sigma <- matrix(c(1,1,1,5),nrow=2) 
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                      mean=mu,sigma=sigma) 
  }
}
contour(x.points,y.points,z)

# jeu 1
jeu1.Q <- MixSim(MaxOmega = 0.0, BarOmega = 0.0, K = 2, p = 2, sph = TRUE)
jeu1 <- simdataset(n = 500, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)

#jeu 2
jeu2.Q <- MixSim(MaxOmega = 0.10, BarOmega = 0.05, K = 3, p = 2, sph = TRUE)
jeu2 <- simdataset(n = 500, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)

#jeu 3
jeu2.Q <- MixSim(MaxOmega = 0.20, BarOmega = 0.04, K = 3, p = 2, sph = FALSE)
jeu2 <- simdataset(n = 500, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)