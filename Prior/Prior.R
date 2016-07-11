#Previous knowledge

# Sample size
nobs <- 100
ftatooine <- 0.65
# 70% are spiral

#Prior
alpha <- 1+ftatooine*nobs
beta  <- 1+(1-ftatooine)*nobs

#Plot prior
x<-seq(.005, .995, length=500)
y <-dbeta(x, alpha,beta)
plot(x,y, type="l",ylab="prior",xlab=expression(f[Tatooine]),col="blue",lwd=3)
abline(v=0.65,lty=3,lwd=2)

prior <- dbeta(x,alpha,beta)

# 20 obs 
nobs_new <- 20
ftatooine_new <- 0.75

tatooine <- ftatooine_new *nobs_new
notatooine <- (1-ftatooine_new)*nobs_new

x = seq(0.005, 0.995, length = 500)
like <- dbeta(x,tatooine+1,notatooine+1)
plot(x,like,type="l",col="blue")
abline(v=0.75,lty=3,lwd=2)



post=dbeta(x,alpha+tatooine, beta+notatooine)
m=max(c(prior,like,post))
plot(x,post,type="l", ylab="Density", lty=2, lwd=3,
ylim=c(0,m),col="red")
lines(x,like,lty=1, lwd=3,col="blue")
lines(x,prior,lty=3, lwd=3,col="green")
legend("topleft",c("Prior","Likelihood","Posterior"),
lty=c(3,1,2), lwd=c(3,3,3), col=c("green","blue","red"))
  


# Plot a mosaic 
par(mfrow = c(2, 3))
par(cex = 0.6)
par(mar = c(3, 3, 2, 2), oma = c(1, 1, 1, 1))
v_obs<-c(1,5,25,50,100,500)
for(i in 1:6){
n_spirals <-v_obs[i]*tatooine
n_nospirals <- v_obs[i]*notatooine
post=dbeta(x,alpha+n_spirals, beta+n_nospirals)
like <- dbeta(x,n_spirals+1,n_nospirals+1)
m=max(c(prior,like,post))
plot(x,post,type="l", ylab="Density", lty=2, lwd=3,
     ylim=c(0,m),col="red")
lines(x,like,lty=1, lwd=3,col="blue")
lines(x,prior,lty=3, lwd=3,col="green")
legend("topleft",c("Prior","Likelihood","Posterior",paste("Nobs=",v_obs[i]*tatooine+v_obs[i]*notatooine,sep="")),
       lty=c(3,1,2,1), lwd=c(3,3,3,0), col=c("green","blue","red"))
}