#Examples of mine...
#Functions to clean the code, these are just a first pass and can/should be better
setwd("C:\\Users\\admin-ccook\\Desktop\\Scitor\\RFP\\")
source("Scripts/functions.R")
setwd("C:\\Users\\admin-ccook\\Desktop\\Research\\Data\\")
#Needed libraries
library(tm)
library(topicmodels)
library(lda)
#library(stringr)
library(lsa)
library(tm)
#Create my Corpus of 5 sentences...
new_data=DirSource(directory="Pets2")
corpus=VCorpus(new_data,readerControl=list(reader=readPlain,language='en_CA',load=TRUE))
corpus<-clean.corpus(corpus)
writeCorpus(corpus,"C:\\Users\\admin-ccook\\Desktop")
TERMS=TermDocumentMatrix(corpus)

#Test out other two algorithms and their results...
corpus=TERMS

D=dim(corpus)[2]
V=dim(corpus)[1]
K=2 #Topics should be pets and food 
iter=5000

t1=Sys.time()

fit1=LDA(t(corpus),k=K,method="VEM",control=
           list(seed=2))

t2=Sys.time()
t2-t1


#turn doc term matrix into the correct form for lda pkg
####Preprocess Data
documents=dtm2ldaformat(t(corpus), omit_empty = TRUE)
vocab=Terms(corpus)


t1 <- Sys.time()
fit2=lda.collapsed.gibbs.sampler(documents = documents[[1]], K = K, vocab = vocab,
                                 num.iterations = iter, alpha = 0.4,
                                 eta = 0.4, initial = NULL, burnin = 5000,
                                 compute.log.likelihood = FALSE)
t2 <- Sys.time()

t2-t1

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#Results

dim(fit2$document_sums)
distribution=fit2$document_sums*(1/apply(fit2$document_sums,2,sum))
distribution

fit1@wordassignments$i
exp(fit1@beta)
fit1@gamma

fit1@wordassignments[[1]]
fit1@wordassignments[[5]]

fit2
