#### Simulate the normal stochastic process
#### Class 1

sim <- matrix(0, nrow = 100, ncol = 4)
t <- seq(1:4)
for(i in 1:100){
  sim[i, ] = rnorm(4, t + 10, 1)
}

plot(t, sim[1,] , col = 2, type="l", ylim = c(10,16), 
     xlab= "time", ylab="X", main = "Simple Gaussian Process")
for(i in 2:100){
  lines(t, sim[i,], col =i)
}

lines(t, t+10, col =4, lwd =3)

plot(density(sim[,1]), main = "X_1 realizations")
abline(v = t[1] +10, col = 4, lwd = 3)
abline(v = (t[1] +10 + (qnorm(0.025))), lwd =3)
abline(v = (t[1] +10 + (qnorm(0.975))), lwd =3)
