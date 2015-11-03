##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#Topicmodels pkg in R
library(tm)
library(topicmodels)
library(lda)
#library(stringr)
library(lsa)

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#Functions to clean the code, these are just a first pass and can/should be better
source("Scripts/functions.R")

#skill bank data is is corpus called employees
#Tasks are in corpus called corpus
new_data=DirSource(directory="Data\\Text\\Dir2")
jobs=VCorpus(new_data,readerControl=list(reader=readPlain,language='en_CA',load=TRUE))
jobs<-clean.corpus(jobs)
jobs_V=TermDocumentMatrix(jobs)


#resume data in corpus called resumes
new_data=DirSource(directory="Data\\Text\\ClearedResumes")
resumes=VCorpus(new_data,readerControl=list(reader=readPlain,language='en_CA',load=TRUE))
resumes<-clean.corpus(resumes)
resumes_V=TermDocumentMatrix(resumes)


TERMS=TermDocumentMatrix(c(resumes,jobs))

#focus on the resumes only...k=10????
corpus=TERMS

D=dim(corpus)[2]
V=dim(corpus)[1]
K=10
iter=5000

t1=Sys.time()

fit1=LDA(corpus,k=K,method="Gibbs",control=
           list(seed=2,iter=iter))

t2=Sys.time()
t2-t1


#turn doc term matrix into the correct form for lda pkg
####Preprocess Data
documents=dtm2ldaformat(t(corpus), omit_empty = TRUE)
vocab=Terms(corpus)

t1 <- Sys.time()
fit2=lda.collapsed.gibbs.sampler(documents = documents[[1]], K = K, vocab = vocab,
                                 num.iterations = iter, alpha = 0.02,
                                 eta = 0.02, initial = NULL, burnin = 0,
                                 compute.log.likelihood = FALSE)
t2 <- Sys.time()

t2-t1

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#Results

dim(fit2$document_sums)
distribution=fit2$document_sums*(1/apply(fit2$document_sums,2,sum))

results=my_cos(r=136,ta=4,distribution)
apply(results,2,which.max)
apply(results,2,order)
















##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#Test with provided data
data(AssociatedPress)
D=dim(AssociatedPress)[1]
V=dim(AssociatedPress)[2]

t1=Sys.time()

fit1=LDA(AssociatedPress,k=5,method="Gibbs",control=
           list(seed=2,iter=5000))

t2=Sys.time()
t2-t1


#turn doc term matrix into the correct form for lda pkg
####Preprocess Data
documents=dtm2ldaformat(AssociatedPress, omit_empty = TRUE)
vocab=Terms(AssociatedPress)
documents_new=list()

for (i in 1:D){
  documents_new[[i]]=documents[[1]][[i]]
}

t1 <- Sys.time()
fit2=lda.collapsed.gibbs.sampler(documents = documents_new, K = 5, vocab = vocab,
                                 num.iterations = 5000, alpha = 0.02,
                                 eta = 0.02, initial = NULL, burnin = 0,
                                 compute.log.likelihood = FALSE)
t2 <- Sys.time()

t2-t1

theta <- t(apply(fit2$document_sums + 0.02, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit2$topics) + 0.02, 2, function(x) x/sum(x)))

# Compute some statistics related to the data set:
doc.length <- sapply(documents_new, function(x) sum(x[2, ]))
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <-apply(inspect(AssociatedPress),2,sum)

News <- list(phi = phi,
             theta = theta,
             doc.length = doc.length,
             vocab = vocab,
             term.frequency = term.frequency)



library(LDAvis)
library(servr)
# create the JSON object to feed the visualization:
json <- createJSON(phi = News$phi,
                   theta = News$theta,
                   doc.length = News$doc.length,
                   vocab = News$vocab,
                   term.frequency = News$term.frequency)
serVis(json, out.dir = 'vis', open.browser = TRUE)


