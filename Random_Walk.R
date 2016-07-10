# A simple  Random Walk experiment


#Plot the position at N-th  step for nsim simulations  

plot_RW<-function(nsim,N){
for(i in 1:nsim){
plot(cumsum(runif(20,-1,1)),xlim=c(1,16),ylim=c(-6,6),type="l",col="cyan3",
     ylab="position",xlab="step")
par(new=TRUE)
}
}

# Plot the positon for each of 20 steps for 50 simulations
plot_RW(50,20)




# Plot a histogram of the final position
plot_hist<-function(nsim,N){
par(new=F)
pos <- replicate(nsim,cumsum(runif(N,-1,1)) ) 
hist(pos)
}

plot_hist(50,20)


