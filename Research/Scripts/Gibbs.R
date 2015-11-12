###My Gibbs fitting and Collapsed Gibbs
my_sample=function(x){
  K=length(x)
  hold=sample(c(1:K),size=1,prob=x)
  return(hold);
}
my_sum<-function(x){
  hold=apply(x[,-1],1,my_table,K=2)[-1,]
  hold2=hold[,which(hold[1,]+hold[2,]!=0)]
  return(hold2);
}
my_table<-function(x,K){
  hold=factor(x,levels=c("NA",1:K))
  return(table(hold));
}
my_Gibbs<-function(docTerm, #docTerm matrix from the Corpus
                   K, # of topics
                   alpha, #
                   eta, #
                   iters) #Max number of iterations
  {
  require("gtools")
  #What we know
  data=as.matrix(docTerm) #term doc matrix 
  D=dim(data)[2] # of documents
  V=dim(data)[1] # length of vocab
  
  #
  if(eta==0.2){eta=0.21}
  #Storage
  Tassign0=list() #holds topic assignments at each step
  Tcounts0=list();for(i in 1:D){Tcounts0[[i]]=matrix(0,nrow=V,ncol=K)} #count matrix
  
  #Set initial values such that each document has the same topic and the topics are 
  #uniformly dist across the documents and get count of topics per document
  for(i in 1:D){
    Tassign0[[i]]=cbind(data[,i],rep((i%%K+1),length(data[,i])))
    Tcounts0[[i]][,(i%%K+1)]=rep(1,length(Tcounts0[[i]][,i%%K+1]))
    for (j in 1:V){
      if(Tassign0[[i]][j,1]==0) {
        Tassign0[[i]][j,2]=NA
        Tcounts0[[i]][j,]=NA
        }
      if(Tassign0[[i]][j,1]>1) {
        Tcounts0[[i]][j,(i%%K+1)]=Tassign0[[i]][j,1]
        }
    }
  }  
  rm(i,j)
  
  #Storage 
  theta=matrix(nrow=K,ncol=D)#holds dist of topics over docs
  beta=matrix(nrow=K,ncol=V)#holds dist of topics over words
  #MCMC Gibbs Sampler
  for(i in 1:iters){
    for (d in 1:D){
      probs=matrix(nrow=V,ncol=K) #holds probs of topic assignments at each step
      for(k in 1:K){
        for(v in 1:V){
          wordDocC=do.call("rbind", lapply(Tcounts0, "[", v, ))[-d,k] 
          wordCorpusC=do.call(rbind,Tcounts0)[,k]
          probs[v,k]=((sum(Tcounts0[[d]][-v,k],na.rm=T)+alpha)*(sum(wordDocC,na.rm=T)
            +eta))/(sum(wordCorpusC,na.rm=T)+(eta*V)-1)
        }
        theta[k,d]=(alpha+sum(Tcounts0[[d]][,k],na.rm=T))/
          ((K*alpha)+(sum(data[,d])))
      }
      probs=probs/apply(probs,1,sum)
      rm(v,k)
      #store new topic assignments
      Tassign0[[d]]=cbind(Tassign0[[d]],apply(probs,1,my_sample))
      Tassign0[[d]][which(is.na(Tassign0[[d]][,2])),i+2]=NA
      #get new counts
      
      for (v in 1:V){
        if(is.na(Tassign0[[d]][v,2+i])){Tcounts0[[d]][v,]=NA}
        else {
          Tcounts0[[d]][v,]=rep(0,K)
          Tcounts0[[d]][v,Tassign0[[d]][v,2+i]]=Tassign0[[d]][v,1]
        }
      }
      rm(v)
    }
    for(v in 1:V){
      for (k in 1:K ){
          wordDocC=do.call("rbind", lapply(Tcounts0, "[", v, ))[,k] 
          wordCorpusC=do.call(rbind,Tcounts0)[,k]
          beta[k,v]=(eta+sum(wordDocC,na.rm=T))/((V*eta)+sum(wordCorpusC,na.rm=T))
      }
    }
    rm(v,k)
  }
  final_topics=do.call("rbind",lapply(lapply(Tassign0,t),"[",iters,))
  Ddist=lapply(Tassign0,my_sum)
  results=list(topicassign=Tassign0,theta=theta,beta=beta,topics=final_topics,
               docDist=lapply(Ddist,"/",iters+1))
  return(results);                 
}

#####check....
test=my_Gibbs(TERMS, K=2, alpha=0.2, eta=0.2, iters=5000) 
test$docDist
test$topics
test$theta
test$beta
