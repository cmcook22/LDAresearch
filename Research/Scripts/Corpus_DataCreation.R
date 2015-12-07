setwd("C:\\Users\\admin-ccook\\Desktop\\Research")
source("Scripts/functions.R")

#Simulate the datasets
P=c(0.01,0.1,1,10)
K=c(2,5,10)
V=c(50,100,500)
D=10
Corpus=list()
l=0

for(p in 1:length(P)){
  for(k in 1:length(K)){
    for(v in 1:length(V)){
      l=l+1
      Corpus[[l]]=simulateCorpus(K=K[k],D=D,N=V[v],DocLen=rep(V[v]/D,D),eta=rep(P[p],V[v]),
                    alpha=rep(P[p],K[k]))
      print(l)
    }
  }
}


save(Corpus,file = "./Data/SimulatedData.Rdata")
#I have my 36 datasets listed in Corpus slots 3 to 38 saved as SimulatedData!!!
#Now I need to run both my Gibbs and VEM to see the time of each, 
  #the accuracy, and the iterations...
