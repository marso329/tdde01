#for loading excel
library('xlsx')

distance <- function(X, Y) {
    XHAT <- X / sqrt(rowSums(X^2))
    YHAT <- Y / sqrt(rowSums(Y^2))
    C <- XHAT %*% t(YHAT)
    D <- 1.0 - C
    return(D)
}

spam <- function(indices, training) {
    spamid <- ncol(training) #  last.
    return(training[indices, spamid])
}

knearest=function(data,k,newdata) {
	#number of samples in learning data
	n1=dim(data)[1]
	#number of samples in test data
	n2=dim(newdata)[1]
	#number of words
	p=dim(data)[2]
	#create a numeric type for the number of samples in test data
	Prob=numeric(n2)
	#remove spam column
	X=as.matrix(data[,-p])
	#remove spam column
	Xn=as.matrix(newdata[-p])
	#calculate distance matrix
	dist <- distance(X, Xn)
	#dont know why this is needed
	X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
	#results vector
	Prob = c()  
	#for every sample	
	for (i in 1:n2 ){
		#get the K nearest neighbors
		nearest_neighbor = order(dist[,i])[1:k]
		#To keep track of how many of them are spam		
		nr_of_spam = 0;
		#for every nearest neighbor
    	for (index in nearest_neighbor){
      		if(data[index,]$Spam == 1){
        	nr_of_spam = nr_of_spam+1
		}
		Prob[i] = nr_of_spam/k
	}
#MISSING: use the computed distance matrix to find 
    #which observations are the nearest neighbors to case #i
#MISSING: derive probability value 'Prob[i]' by using the
    #target values of the nearest neighbors
  }
  return(Prob)
}



ROC=function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    #TPR[i]=#insert formula for TPR
    #FPR[i]=#insert formula for FPR
  }
  return (list(TPR=TPR,FPR=FPR))
}

#load data
data <- read.xlsx("spambase.xlsx", 1,header=TRUE)
# number of samples, should be 2740
n=dim(data)[1]
#seed the RNG
set.seed(12345)
#sample 50percent of the vectors
id=sample(1:n, floor(n*0.5))
#set train data to 50 percent of the data
train=data[id,]
#set test data to the other 50 percent
test=data[-id,]
k5 <- knearest(train, 5, test)
print(k5)
