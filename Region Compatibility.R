#Region compatibility function
#Input = the leaves the objects are predicted to be in for tree 1 (predI) and tree 2 (predII)
RC.Function <- function(predI, predII){
  nrows<-length(predI)
  # Ranking the predicted nodes/leaves so there are no leaves without predictions and the nleaves objects display the proper value
  predI <-dense_rank(predI);
  predII <- dense_rank(predII);
  
  leaf_assign_frameI <- as.data.frame(predI)
  leaf_assign_frameII <- as.data.frame(predII)
  nleavesI <- max(predI)   #value for the amount of leaves in each prediction
  nleavesII <- max(predII)
  
  # calculate m1 values: a vector is made with the probability assignment of the leaves (proportion of objects in leaves) of the first tree
  v1_matrix<-matrix(rep(0,nrows*nleavesI),nrow = nrows, ncol=nleavesI )
  for (i in 1:nleavesI){
    v1_matrix[,i] <- leaf_assign_frameI$predI==i 
  }
  vec_m1 <- apply(v1_matrix,2,mean) 
  
  # calculate m2 values vec_m2
  v2_matrix<-matrix(rep(0,nrows*nleavesII),nrow = nrows, ncol=nleavesII )
  for (j in 1:nleavesII){
    v2_matrix[,j] <- leaf_assign_frameII$predII==j 
  }
  vec_m2 <- apply(v2_matrix,2,mean) 
  
  # create m1-m2 projection
  #step 1: exclude if m1 == m2
  booleanMatrix<-matrix(rep(FALSE,nleavesI*nleavesII),nrow = nleavesI,ncol = nleavesII)
  v1com<-c()
  v2com<-c()
  for (i in 1:nleavesI) {
    for (j in 1:nleavesII) {
      booleanMatrix[i,j]<- identical(v1_matrix[,i],v2_matrix[,j])
      if(booleanMatrix[i,j]==TRUE){
        v1com<-c(v1com,i)
        v2com<-c(v2com,j)
      }
      vec_m2[j]<-ifelse(sum(booleanMatrix[i,j])>=1,0,vec_m2[j])
    }
    vec_m1[i]<-ifelse(sum(booleanMatrix[i,])>=1,0,vec_m1[i])
    
  }
  #common/shared values are removed
  
  if(length(v1com)>0){
    vec_m1end<-vec_m1[-v1com]
  }else{
    vec_m1end<-vec_m1
  }
  
  if(length(v1com)>0){
    vec_m2end<-vec_m2[-v2com]
  }else{
    vec_m2end<-vec_m2
  }
  
  # step 2: sort vectors by size
  v1 <- sort(vec_m1end)
  v2 <- sort(vec_m2end)
  
  ordv1<-order(vec_m1)
  ordv2<-order(vec_m2)
  
  ordv1<-ordv1[!ordv1%in%v1com]
  ordv2<-ordv2[!ordv2%in%v2com]
  
  #step 3: concatenate remaining values:
  
  vec_m1_minus_m2 <- c(v1, (-1)*(v2))
  
  library(dplyr)
  #Jaccard matrix
  if (length(vec_m1_minus_m2) == 0){
    R_jac <- 0
  } else{
    
    
    # A is a list of vectors for each of the decision regions containing their predicted objects
    A<-list()
    for (i in 1:(nleavesI-length(v1com))) {
      A[[i]]<-which(leaf_assign_frameI$predI==ordv1[i])
    }
    
    lA<-length(A)
    
    for (j in 1:(nleavesII-length(v2com))) {
      A[[lA+j]]<-which(leaf_assign_frameII$predII==ordv2[j])
    }
    # Jaccard Matrix is created where the number of rows (n ) and the number of columns (p ) is equal to the length of vector mA-mB
    Jaccard_matrix <- matrix(nrow = length(A), ncol = length(A))
    #the values of the matrix are calculated by dividing the shared elements/objects all decision regions the by the total number of elements
    for (i in 1:length(A)){
      for(j in 1:length(A)){
        x1 <- A[[i]]
        x2 <- A[[j]]
        Jacc_val <- length(intersect(x1, x2))/length(union(x1,x2))
        Jaccard_matrix[i,j] <- Jacc_val
      }
    }
    #Region compatability with jaccard index is calculated according to Wang et al.(2018)
    R_jac <- sqrt(t(vec_m1_minus_m2) %*% Jaccard_matrix %*% vec_m1_minus_m2)
  }
  return(R_jac) 
}

