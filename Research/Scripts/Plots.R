setwd("C:\\Users\\admin-ccook\\Desktop\\Research")
load("Data/SimulatedData.Rdata")
load("Data/VB_results.Rdata")
load("Data/CG_results.Rdata")

library(ggplot2)
K=rep(c(rep(2,3),rep(5,3),rep(10,3)),4)
V=rep(c(50,100,500),12)
P=c(rep(0.01,9),rep(0.1,9),rep(1,9),rep(10,9))

#Use Hdist and mse to see the dist between true theta/beta and estimated
Hdist_theta=matrix(nrow=36,ncol=2)
colnames(Hdist_theta)=c("VB","CG")
MSE_theta=matrix(nrow=36,ncol=2)
colnames(MSE_theta)=c("VB","CG")
for(i in 1:36){
  Hdist_theta[i,1]=1/K[i]*(1/sqrt(2)*sqrt(sum((sqrt(Corpus[[i]]$Theta)-
                  sqrt(VB_results[[i]]@gamma))^2)))
  Hdist_theta[i,2]=1/K[i]*(1/sqrt(2)*sqrt(sum((sqrt(Corpus[[i]]$Theta)-
                  sqrt(t(CG_results[[i]]$theta)[,c(K[i]:1)]))^2)))
  MSE_theta[i,1]=1/(K[i]*10)*sum((Corpus[[i]]$Theta-VB_results[[i]]@gamma)^2)
  MSE_theta[i,2]=1/(K[i]*10)*sum((Corpus[[i]]$Theta-
                 t(CG_results[[i]]$theta)[,c(K[i]:1)])^2)  
}

Hdist_beta=matrix(nrow=36,ncol=2)
colnames(Hdist_beta)=c("VB","CG")
MSE_beta=matrix(nrow=36,ncol=2)
colnames(MSE_beta)=c("VB","CG")
for(i in 1:36){
  Hdist_beta[i,1]=1/K[i]*(1/sqrt(2)*sqrt(sum((sqrt(Corpus[[i]]$Beta)-
                  sqrt(exp(VB_results[[i]]@beta)))^2)))
  Hdist_beta[i,2]=1/K[i]*(1/sqrt(2)*sqrt(sum((sqrt(Corpus[[i]]$Beta)-
                  sqrt(CG_results[[i]]$beta))^2)))
  MSE_beta[i,1]=1/(K[i]*V[i])*sum((Corpus[[i]]$Beta-exp(VB_results[[i]]@beta))^2)
  MSE_beta[i,2]=1/(K[i]*V[i])*sum((Corpus[[i]]$Beta-CG_results[[i]]$beta)^2)  
}

#gather data for ggplot2
data=data.frame(K=c(K,K), type=c(rep("VB",36),rep("CG",36)), P=c(P,P), 
                theta=c(Hdist_theta[,1], Hdist_theta[,2]), V=c(V,V), 
                beta=c(Hdist_beta[,1],Hdist_beta[,2]), 
                mse_theta=c(MSE_theta[,1],MSE_theta[,2]),
                mse_beta=c(MSE_beta[,1],MSE_beta[,2]))
data$K=factor(K,levels=c(2,5,10),
              labels=c("2topics","5topics","10topics"))
data$type=factor(data$type,levels=c("VB","CG"),labels=c("VB","CG"))
data$P=factor(P,levels=c(0.01,0.1,1,10),
              labels=c("hp=0.01","hp=0.1","hp=1","hp=10"))  


# Scatterplots for paper and presentation using ggplot2
qplot(V, theta, data=data, shape=type, color=type, 
      facets=K~P, size=I(4),main="Theta",
      xlab="Vocab Size", ylab="Hellinger Distance") 

qplot(V, beta, data=data, shape=type, color=type, 
      facets=K~P, size=I(4),main="Beta",
      xlab="Vocab Size", ylab="Hellinger Distance") 

qplot(V, mse_theta, data=data, shape=type, color=type, 
      facets=K~P, size=I(4),main="Theta",
      xlab="Vocab Size", ylab="MSE") 

qplot(V, mse_beta, data=data, shape=type, color=type, 
      facets=K~P, size=I(4),main="Beta",
      xlab="Vocab Size", ylab="MSE") 















