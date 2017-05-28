


# Theoretically........ ---------------------------------------------------
curve(dcauchy(x) * 2,lwd=3, col='red', from=-10, to = 10)
curve(dnorm(x),add=TRUE,col="black",lwd=3, from =-10, to = 10)
text(5,0.35,labels=expression(k~f[y](x)),col='red')
text(5,0.3,labels=expression(f[X](x)),col="black")
legend(x="topleft",lty=1,lwd=1.4,col=c("red","black"),legend=c("Std_Cauchy","Std_Normal"))
title(main="A/R")

# Acceptance_Rejection_Algorithm ------------------------------------------
AR=function(dtarget,dauxiliary,rauxiliary,k){
  count=0
  E=0
  
  while(E==0){
    candidate = rauxiliary(1)
    acc_prob=dtarget(candidate)/(k*dauxiliary(candidate))
    E = sample(c(1,0),prob=c(acc_prob, 1-acc_prob),size=1)
    count=count+1
  }
  
  return(list(draw=candidate,computational_effort=count))
  
}
# Functions ---------------------------------------------------------------
q=function(x){
  dcauchy(x)
}
draw_from_q=function(n){
  rcauchy(n)
}
f=function(x){
  dnorm(x)
}
# Run the Algorithm -------------------------------------------------------
mcsize=100000
draw_vec=rep(NA,mcsize)
effort_vec=rep(NA,mcsize)
prob_vec=rep(NA,mcsize)
k = 2
for(i in 1:mcsize){
  
  DD=AR(dtarget=f,dauxiliary=q,rauxiliary=draw_from_q,k)
  draw_vec[i] = DD$draw
  effort_vec[i] = DD$computational_effort
}
# Estimate for Acceptance Probability -------------------------------------
print ('An Estimate for Acceptance Probability')
prob = 1/effort_vec
# In simulation, the Inverse of Computational Effort for each Single point should be its acceptance
#Probability, and So the Overall mean for 100,000 Such Points should give us an estimate
# for acceptance probablity
mean(1/effort_vec) 
# Graphical Behaviour of accpeted samples ---------------------------------
hist(draw_vec,freq=FALSE,col='orchid')
curve(f(x),add=TRUE,lwd=4, col='blue')


