
#Function for Regression Trees
benchmarkfunction.regression <- function(K,k, kdata,I, disc.vector) #Function for regression trees
{
  #K = total number of benchmark iterations
  #I = iteration row to start with (for saving at the right place)
  #k = total number of current benchmark iterations (starting from I)
  #disc.vector = vector indicating the categories the variables are to be discretized into
  
  Mean.Absolute.Distance <- matrix(0, nrow = K, ncol= 3)   #Creating the object for the mean absolute distances between predictions, per method and iteration
  Concordance.Correlation <- matrix(0, nrow = K, ncol= 3)  #Creating the object for the concordance correlation between predictions, per method and iteration  
  Region.Compatibility <- matrix(0, nrow = K, ncol= 3)
  colnames(Mean.Absolute.Distance)<-c("rpart", "evtree", "ETree")
  colnames(Concordance.Correlation)<-c("rpart", "evtree", "ETree") 
  colnames(Region.Compatibility)<-c("rpart", "evtree", "ETree") 
  
  i <- I
  while(i < (k + I)) {
    cat("\n","Iteration is ", i) 
    set.seed(i * 1000)                                                           #seed for reproducibility
    
    #creating bootstrap samples for training of size .9N and OOB for testing
    
    index<-1:nrow(kdata)
    colnames(kdata)[length(kdata)] <- "ytrain"
    boot.index1 <- sample(index,size=round(.9*nrow(kdata)), replace = TRUE)
    boot.index2 <- sample(index,size=round(.9*nrow(kdata)), replace = TRUE)
    trainindex<-sort(unique(c(boot.index1,boot.index2)))
    testindex <- index[-trainindex]
    
    train1 <- kdata[boot.index1,]
    train2 <- kdata[boot.index2,]
    test <- kdata[testindex,]
    
    ##method 1 rpart
    control_rpart <-rpart.control(maxdepth = 3,minbucket = 10, minsplit = 20, xval=0, cp=0.0001) 
    out_rpart1 <- rpart(ytrain ~ ., data=train1, control=control_rpart)
    predict_rpart1 <- as.numeric(predict(out_rpart1, newdata= test)) # prediction using the first trained tree on the test set
    
    predict_rpart1_leaf <- rpart.predict.leaves(out_rpart1, newdata= test,type= "where")
    
    out_rpart2 <- rpart(ytrain ~ ., data=train2, control=control_rpart)
    predict_rpart_train1 <- as.numeric(predict(out_rpart2, newdata= train2))
    predict_rpart2 <- as.numeric(predict(out_rpart2, newdata= test)) # prediction using the second trained tree on the test set
    predict_rpart2_leaf <- rpart.predict.leaves(out_rpart2, newdata= test,type= "where")
    
    #calculation of mean absolute distance
    Mean.Absolute.Distance[i,1] <- (mean(abs(predict_rpart1 - predict_rpart2)))
    #calculation of concordance correlation
    Concordance.Correlation[i,1] <- CCC(as.numeric(predict_rpart1), as.numeric(predict_rpart2))$rho.c[,1]          
    
    Region.Compatibility[i,1] <- RC.Function(predict_rpart1_leaf, predict_rpart2_leaf)
    
    
    
    # method 2 evtree
    control_evtree <- evtree.control(maxdepth = 3,minbucket = 10, minsplit = 20, alpha=0.0001) 
    
    out_evtree1 <- evtree(ytrain ~ ., data=train1,control=control_evtree)
    predict_evtree1 <- as.numeric(predict(out_evtree1, newdata= test))
    predict_evtree1_leaf <- predict(out_evtree1, newdata= test,type= "node")
    
    out_evtree2 <- evtree(ytrain ~ ., data=train2,control=control_evtree)
    predict_evtree2 <- as.numeric(predict(out_evtree2, newdata= test))
    predict_evtree2_leaf <- predict(out_evtree2, newdata= test,type= "node")
    
    
    Mean.Absolute.Distance[i,2] <- (mean(abs(predict_evtree1 - predict_evtree2)))
    Concordance.Correlation[i,2] <- CCC(predict_evtree1, predict_evtree2)$rho.c[,1]
    Region.Compatibility[i,2] <- RC.Function(predict_evtree1_leaf, predict_evtree2_leaf)
    
    # method 3 ETree
    
    control1<-ETree.control(measure = 0, maxsize = NULL, maxdepth = 3, minbucket = 10, ncv=0, minheterogeneity = 0.0001 )
    out_ETree1<-ETree(ytrain ~ ., data=train1, control=control1, discretize = disc.vector) 
    
    predict_ETree1 <- as.numeric(predict(out_ETree1, newdata= test)) 
    predict_ETree1_leaf <- predict(out_ETree1, newdata= test, type= "matrix")
    
    out_ETree2<-ETree(ytrain ~ ., data=train2, control=control1, discretize = disc.vector)
    predict_ETree2 <- as.numeric(predict(out_ETree2, newdata= test)) 
    predict_ETree2_leaf <- predict(out_ETree2, newdata= test, type= "matrix") 
    
    Mean.Absolute.Distance[i,3] <- (mean(abs(predict_ETree1 - predict_ETree2)))
    Concordance.Correlation[i,3] <- CCC(predict_ETree1, predict_ETree2)$rho.c[,1]
    
    Region.Compatibility[i,3] <- RC.Function(predict_ETree1_leaf[,1],predict_ETree2_leaf[,1])
    
    i <- i+1
  }
  obj <- list(Mean.Absolute.Distance= Mean.Absolute.Distance, Concordance.Correlation = Concordance.Correlation, Region.Compatibility = Region.Compatibility)
}


#Function for Classification Trees
benchmarkfunction.classification <- function(K,k, kdata,I, disc.vector) 
{
  #K = total number of benchmark iterations
  #I = iteration row to start with (for saving at the right place)
  #k = total number of current benchmark iterations (starting from I)
  #disc.vector = vector indicating the categories the variables are to be discretized into
  
  Average.class.agreement <- matrix(0, nrow = K, ncol= 3)
  Cohen.Kappa <- matrix(0, nrow = K, ncol= 3)
  Mean.Absolute.Distance <- matrix(0, nrow = K, ncol= 3)  
  Concordance.Correlation <- matrix(0, nrow = K, ncol= 3)  
  Region.Compatibility <- matrix(0, nrow = K, ncol= 3)
  
  
  colnames(Average.class.agreement)<-c("rpart", "evtree", "ETree")
  colnames(Cohen.Kappa)<-c("rpart", "evtree", "ETree") 
  colnames(Region.Compatibility)<-c("rpart", "evtree", "ETree") 
  colnames(Mean.Absolute.Distance)<-c("rpart", "evtree", "ETree")
  colnames(Concordance.Correlation)<-c("rpart", "evtree", "ETree") 
  
  i <- I
  while(i < (k + I)) {
    cat("\n","Iteration is ", i) 
    set.seed(i * 1000)
    
    index<-1:nrow(kdata)
    colnames(kdata)[length(kdata)] <- "ytrain"
    boot.index1 <- sample(index,size=round(.9*nrow(kdata)), replace = TRUE)
    boot.index2 <- sample(index,size=round(.9*nrow(kdata)), replace = TRUE)
    trainindex<-sort(unique(c(boot.index1,boot.index2)))
    testindex <- index[-trainindex]
    
    train1 <- kdata[boot.index1,]
    train2 <- kdata[boot.index2,]
    test <- kdata[testindex,]
    
    
    
    ##method 1 rpart
    control_rpart <-rpart.control(maxdepth = 3,minbucket = 10, minsplit = 20, xval=0, cp=0.0001) 
    
    out_rpart1 <- rpart(ytrain ~ ., data=train1, control=control_rpart)
    predict_rpart1 <- predict(out_rpart1, newdata= test,type= "class") 
    predict_rpart1_leaf <- rpart.predict.leaves(out_rpart1, newdata= test,type= "where")
    predict_rpart1_prob <- predict(out_rpart1, newdata= test, type= "prob") 
    
    out_rpart2 <- rpart(ytrain ~ ., data=train2, control=control_rpart)
    predict_rpart2 <- predict(out_rpart2, newdata= test,type= "class") 
    predict_rpart2_leaf <- rpart.predict.leaves(out_rpart2, newdata= test,type= "where")
    predict_rpart2_prob <- predict(out_rpart2, newdata= test, type= "prob") 
    
    
    Average.class.agreement[i,1] <- sum(predict_rpart1 == predict_rpart2)/ length(predict_rpart1)
    Cohen.Kappa[i,1] <-  cohen.kappa(as.data.frame(cbind(predict_rpart1, predict_rpart2)))$kappa
    
    Mean.Absolute.Distance[i,1] <- mean(abs(predict_rpart1_prob[,2] - predict_rpart2_prob[,2]))
    Concordance.Correlation[i,1] <- CCC(predict_rpart1_prob[,2], predict_rpart2_prob[,2])$rho.c[,1]  
    
    Region.Compatibility[i,1] <- RC.Function(predict_rpart1_leaf, predict_rpart2_leaf)
    
    # method 2 evtree
    control_evtree <- evtree.control(maxdepth = 3,minbucket = 10, minsplit = 20, alpha=0.0001) 
    
    out_evtree1 <- evtree(ytrain ~ ., data=train1,control=control_evtree)
    predict_evtree1 <- predict(out_evtree1, newdata= test)
    predict_evtree1_leaf <- predict(out_evtree1, newdata= test,type= "node")
    predict_evtree1_prob <- predict(out_evtree1, newdata= test, type= "prob") 
    
    out_evtree2 <- evtree(ytrain ~ ., data=train2,control=control_evtree)
    predict_evtree2 <- predict(out_evtree2, newdata= test)
    predict_evtree2_leaf <- predict(out_evtree2, newdata= test,type= "node")
    predict_evtree2_prob <- predict(out_evtree2, newdata= test, type= "prob") 
    
    
    Average.class.agreement[i,2] <- sum(predict_evtree1 == predict_evtree2)/ length(predict_evtree1)
    Cohen.Kappa[i,2] <- cohen.kappa(as.data.frame(cbind(predict_evtree1, predict_evtree2)))$kappa
    
    Mean.Absolute.Distance[i,2] <- mean(abs(predict_evtree1_prob[,2] - predict_evtree2_prob[,2]))
    Concordance.Correlation[i,2] <- CCC(predict_evtree1_prob[,2], predict_evtree2_prob[,2])$rho.c[,1]
    
    Region.Compatibility[i,2] <- RC.Function(predict_evtree1_leaf, predict_evtree2_leaf)
    
    # method 3 ETree
    #classification: measure = 1
    control1<-ETree.control(measure = 1, maxsize = NULL, maxdepth = 3, minbucket = 10, ncv=0, minheterogeneity = 0.0001 )
    
    out_ETree1<-ETree(ytrain ~ ., data=train1, control=control1, discretize = disc.vector)
    predict_ETree1 <- predict(out_ETree1, newdata= test) 
    predict_ETree1_leaf <- predict(out_ETree1, newdata= test, type= "matrix")
    predict_ETree1_prob <- predict(out_ETree1, newdata= test, type= "prob") 
    
    out_ETree2<-ETree(ytrain ~ ., data=train2, control=control1, discretize = disc.vector) 
    predict_ETree2 <- predict(out_ETree2, newdata= test) 
    predict_ETree2_leaf <- predict(out_ETree2, newdata= test, type= "matrix") 
    predict_ETree2_prob <- predict(out_ETree2, newdata= test, type= "prob") 
    
    
    Average.class.agreement[i,3] <- sum(predict_ETree1 == predict_ETree2)/ length(predict_ETree1)
    Cohen.Kappa[i,3] <- cohen.kappa(as.data.frame(cbind(predict_ETree1, predict_ETree2)))$kappa
    
    Mean.Absolute.Distance[i,3] <- (mean(abs(predict_ETree1_prob[,2] - predict_ETree2_prob[,2])))
    Concordance.Correlation[i,3] <- CCC(predict_ETree1_prob[,2], predict_ETree2_prob[,2])$rho.c[,1]
    
    Region.Compatibility[i,3] <- RC.Function(predict_ETree1_leaf[,1],predict_ETree2_leaf[,1])
    i <- i+1
  }
  obj <- list(Average.class.agreement= Average.class.agreement, Cohen.Kappa = Cohen.Kappa, Region.Compatibility = Region.Compatibility, 
              Mean.Absolute.Distance = Mean.Absolute.Distance, Concordance.Correlation = Concordance.Correlation)
}








