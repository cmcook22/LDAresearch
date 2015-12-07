setwd("C:\\Users\\admin-ccook\\Desktop\\Research")
source("Scripts/functions.R")
load("Data/SimulatedData.Rdata")
library(topicmodels)

#Simulated data is in Corpus, a list with elements 3-38 that contain 

#Apply CGS and VB and save estimates 

VB_results=list()
K=rep(c(rep(2,3),rep(5,3),rep(10,3)),4)
  
for (i in 1:36){
  VB_results[[i]]=LDA(t(Corpus[[i]]$tdm),k=K[i], method="VEM",
      control=list(seed=2))
  print(i)
}
save(VB_results,file = "./Data/VB_results.Rdata")

CG_results1_mine=list()
for (i in 1:22){
  CG_results1_mine[[i]]=my_Gibbs(Corpus[[i]]$tdm,K=K[i],alpha=1,eta=1)
  print(i)
}

save(CG_results_mine1,file="./Data/CG_results_mine1.Rdata")

CG_results_mine2=list()
for (i in 23:30){
  CG_results_mine2[[i]]=my_Gibbs(Corpus[[i]]$tdm,K=K[i],alpha=1,eta=1)
  print(i)
}
save(CG_results_mine2,file="./Data/CG_results_mine2.Rdata")

CG_results_mine3=list()
for (i in 31:36){
  CG_results_mine3[[i]]=my_Gibbs(Corpus[[i]]$tdm,K=K[i],alpha=1,eta=1)
  print(i)
}
save(CG_results_mine3,file="./Data/CG_results_mine3.Rdata")

load("Data/CG_results_mine1.Rdata")
load("Data/CG_results_mine2.Rdata")
CG_results=list()
for(i in 1:22){
  CG_results[[i]]=CG_results_mine[[i]]
}
for(i in 23:30){
  CG_results[[i]]=CG_results_mine2[[i]]
}
for(i in 31:36){
  CG_results[[i]]=CG_results_mine3[[i]]
}
save(CG_results,file="./Data/CG_results.Rdata")