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
                   tol=10^(-5), #Tolerance of Hdist 
                   iters=500) #Max number of iterations
  {
  require("gtools")
  
  #What we know
  data=as.matrix(docTerm) #term doc matrix 
  D=dim(data)[2] # of documents
  V=dim(data)[1] # length of vocab
  
  #Storage
  Tassign0=list() #holds topic assignments at each step
  Tcounts0=list();for(i in 1:D){Tcounts0[[i]]=matrix(0,nrow=V,ncol=K)} #count matrix
  theta_old=matrix(0,nrow=K,ncol=D)#holds dist of topics over docs
  theta=matrix(nrow=K,ncol=D)#holds dist of topics over docs
  beta=matrix(nrow=K,ncol=V)#holds dist of topics over words
  Hdist=rep(NA,iters)
  
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
          if(probs[v,k]<0){ probs[v,k]=0}
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
   
  #Check to see if we can break the loop early
  iterations=i
  Hdist[i]=1/K*(1/sqrt(2)*sqrt(sum((sqrt(theta_old)-sqrt(theta))^2)))
  if(iterations>500){if(Hdist[i]<tol){break;}}
  theta_old=theta 
    
  #Calculate beta estimate  
  for(v in 1:V){
    for (k in 1:K ){
        wordDocC=do.call("rbind", lapply(Tcounts0, "[", v, ))[,k] 
        wordCorpusC=do.call(rbind,Tcounts0)[,k]
        beta[k,v]=(eta+sum(wordDocC,na.rm=T))/((V*eta)+sum(wordCorpusC,na.rm=T))
    }
  }
    rm(v,k)
  
  }
  
  #Save results to output 
  final_topics=do.call("rbind",lapply(lapply(Tassign0,t),"[",iterations+2,))
  Ddist=lapply(Tassign0,my_sum)
  results=list(topicassign=Tassign0,theta=theta,beta=beta,topics=final_topics,
               docDist=lapply(Ddist,"/",iterations+1),max_iter=iterations,Hdist=Hdist)
  return(results);                 
}





#Functions to simulate a Corpus and Get its Word/Doc Distributions
simulateCorpus <- function(K, # Number of Topics
                           D, # number of documents
                           N, # Length of Vocab
                           DocLen=1, #Vector of document lengths
                           eta, #prior for topics over vocabulary
                           alpha, #prior for documents over topics
                           lambda) #Mean for Pois dist of words per doc
{
  ####Check inputs to model
  if(length(eta)!=N){stop("Error: Hyperparameter for Vocab is not correct Dimension"); return;}
  if(length(alpha)!=K){print("Error: Hyperparameter for Topics is not correct Dimension");return;}
  if(length(DocLen)==1){print("Warning: Leanth of Documents will be randomly generated from Poisson Distribution")
    DocLen=rpois(D,lambda=lambda)
  }  
  
  
  #####Draw priors for topic distributions over the vocabulary 
  require("gtools")
  Beta=cbind(rdirichlet(K,eta)) #k rows, N cols 
  
  #Storage for final topics and words
  documents=list()
  tdm=matrix(0,nrow=N,ncol=D)
  Word_Topic_Count=matrix(0,nrow=N,ncol=K)
  Theta=matrix(nrow=D,ncol=K)
  #####Create each document in the Corpus
  for(i in 1:D){
    #Draw prior for document distribution over topics
    Theta[i,]=rdirichlet(1,alpha) # 1 row, 3 columns
    
    #Get the jth word for document i
    hold=matrix(nrow=DocLen[i],ncol=2)
    colnames(hold)=c("topics","words")
    for (j in 1:DocLen[i]){
      #Draw the topic for the word
      hold[j,1]=which(rmultinom(1,1,Theta[i,])==1)
      #Draw the actual word
      hold[j,2]=which(rmultinom(1,1,Beta[hold[j,1],])==1)
    }
    documents[[i]]=hold
    tdm[sort(unique(hold[,2])),i]=table(hold[,2])
    for(v in 1:dim(hold)[1]){
      Word_Topic_Count[hold[v,2],hold[v,1]]=Word_Topic_Count[hold[v,2],
                                                             hold[v,1]]+1
    }
  }
  results=list(documents=documents,tdm=tdm,word_counts=Word_Topic_Count,Theta=Theta,
               Beta=Beta)
  return(results);
}

Doc_Topic_Count<-function(results,K){
  #Check results are from simulateCorpus
  if(is.list(results)==FALSE){stop("Error: Results must be the results from simulateCorpus"); return;}
  D=length(results)
  hold=matrix(nrow=D,ncol=K)
  #Get topic dist for each doc
  for (i in 1:D){
    hold[i,]=table(factor(results[[i]][,1],levels=seq(1,K)))
  }
  return(hold);
}








