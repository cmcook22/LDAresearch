#Functions to simulate a Corpus and Get its Word/Doc Distributions
simulateCorpus <- function(K, # Number of Topics
                           D, # number of documents
                           N, # Length of Vocab
                           DocLen=1, #Vector of document lengths
                           eta, #prior for topics over vocabulary
                           alpha, #prior for documents over topics
                           lambda) 
  {
    ####Check inputs to model
    if(length(eta)!=N){stop("Error: Hyperparameter for Vocab is not correct Dimension"); return;}
    #if(sum(eta)!=1){stop("Error: Hyperparameter for Vocab does not sum to 1"); return;}
    if(length(alpha)!=K){print("Error: Hyperparameter for Topics is not correct Dimension");return;}
    #if(sum(alpha)!=1){stop("Error: Hyperparameter for Topics does not sum to 1");return;}
    if(length(DocLen)!=D){print("Warning: Leanth of Documents will be randomly generated from Poisson Distribution")
                          DocLen=rpois(D,lambda=lambda)
    }  
  
    
    #####Draw priors for topic distributions over the vocabulary 
    require("gtools")
    Beta=cbind(rdirichlet(K,eta)) #k rows, N cols 
    
    #Storage for final topics and words
    documents=list()
    tdm=matrix(0,nrow=N,ncol=D)
    Word_Topic_Count=matrix(0,nrow=N,ncol=K)
    #####Create each document in the Corpus
    for(i in 1:D){
      #Draw prior for document distribution over topics
      Theta=rdirichlet(1,alpha) # 1 row, 3 columns
      
      #Get the jth word for document i
      hold=matrix(nrow=DocLen[i],ncol=2)
      colnames(hold)=c("topics","words")
      for (j in 1:DocLen[i]){
        #Draw the topic for the word
        hold[j,1]=which(rmultinom(1,1,Theta)==1)
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
    results=list(documents=documents,tdm=tdm,word_counts=Word_Topic_Count)
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

Word_Topic_Count<-function(results,K){
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

####Testing...
Corpus=simulateCorpus(K=3, # Topics
              D=10, # documents
              N=100, # Vocab
              eta=rep(0.01,100), 
              alpha=c(0.3,0.3,0.4),lambda=10) 

Doc_Topic_Count(Corpus$documents,K=3)




