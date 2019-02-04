## Auxiliar functions to perform the experimets

####################################
######## Sampling functions ######## 
####################################

index.sample = function(response,grid = seq(0.1,0.5,0.1),n.train = 300,theta.train = 0.5,
                        n.target.min = 100,n.sim = 100){
  
  n.total = length(response)
  theta = mean(response)
  
  n1 = sum(response)
  n0 = n.total - n1
  
  if(n1 < n.train/2 | n0 < n.train/2){
    
    return("n < n.min")
    
  }
  
  n.complement = n.total - n.train
  n1.complement = n1 - theta.train*n.train
  n0.complement = n.complement - n1.complement
  theta.complement = n1.complement/n.complement
  n.max = min(n1.complement*2,n0.complement)
  
  n.temp = n.target.min
  i = n.target.min
  
  while(n.max %/% i != 0){
    
    n.temp = n.max %/% i * i
    
    i = i * 10
    
  }
  
  n = n.temp* matrix(c(1 - grid,grid),ncol = 2)
  
  sample=replicate(length(grid),
                   list(train  = matrix(ncol = n.train,nrow = n.sim),
                        target = matrix(ncol = n.temp,nrow = n.sim),
                        inf = list()),
                   simplify=F)
  
  zero.index = which(response == 0)
  one.index = which(response == 1)
  
  for(i in 1:length(grid)){
    
    for(j in 1:n.sim){
      #print((j + n.sim*(i-1))/(length(grid)*n.sim))  
      train.zero.index = sample(zero.index,size = n.train/2,replace = F)
      train.one.index  = sample(one.index, size = n.train/2,replace = F)
      sample[[i]]$train[j, ] = c(train.zero.index,train.one.index)
      
      temp=seq(1,length(response))
      target.index = temp[-sample[[i]]$train[j, ]]
      target.zero.index = sample(target.index[response[target.index] == 0],
                                 size = n[i,1],replace = F)
      target.one.index = sample(target.index[response[target.index] == 1],
                                size = n[i,2],replace = F)
      
      sample[[i]]$target[j, ] = c(target.zero.index,target.one.index)
    }
    
    sample[[i]]$inf = cbind(t(n[i, ]),sum(n[i, ]),theta.train,grid[i])  
    colnames(sample[[i]]$inf) = c("n0","n1","ntotal","theta_tr","theta_tg")
    
  }
  
  
  
  return(sample)
  
}


####################################
########## Quantification ##########
####################################

quantification.prior.shift = function(x.train,
                                      y.train,
                                      x.target,
                                      y.target,
                                      g,
                                      nL){
  
  ## Inputs:
  ## x.train  - a (nL x d) data.frame cotaining the covariates in the training sample
  ## y.train  - a (nL x 1) vector containing the response in the training sample
  ## x.target - a (nU x d) data.frame containing the covariates in the target sample
  ## g        - a list containing the methods that will be considered
  
  ##Outputs: 
  ##out      - a list containing the proportions estimatives for each method
  
  
  n.methods   = length(g)
  p           = vector(length = n.methods)
  mu.g        = vector(length = n.methods)
  mu.g0       = vector(length = n.methods)
  mu.g1       = vector(length = n.methods)
  var.g1      = vector(length = n.methods)
  var.g0      = vector(length = n.methods)
  index.0   = which(y.train==0)
  index.1   = which(y.train==1)
  
  ntr = nrow(x.train)
  
  ntg = nrow(x.target)
  
  gx = matrix(ncol = ntr + ntg, nrow = n.methods)
  
  thetatr = mean(y.train)
  
  
  
  ## Evaluate the neceser quantities for both ratio and combined estimators 
  for(i in 1:n.methods){
    
    gx[i,] = g[[i]]$func(y.train,x.train,x.target,g[[i]]$extra)
    
  }
  
  if(n.methods == 1){
    
    mu.g   = mean(gx[ ,-c(1:ntr)])
    mu.g0  = mean(gx[ ,index.0])
    mu.g1  = mean(gx[ ,index.1])
    var.g0 = var(gx[ ,index.0])
    var.g1 = var(gx[ ,index.1])
    
  }else{
    
    mu.g   = apply(gx[ ,-c(1:ntr)],1,mean)
    mu.g0  = apply(gx[ ,index.0],1,mean)
    mu.g1  = apply(gx[ ,index.1],1,mean)
    var.g0 = apply(gx[ ,index.0],1,var)
    var.g1 = apply(gx[ ,index.1],1,var)
    
  }
  
  ## Ratio estimator estimative
  p = (mu.g-mu.g0)/(mu.g1-mu.g0)
  p[p<0] = 0
  p[p>1] = 1
  
  
  out.ratio = data.frame(method = names(g),estimative = p,
                         mu_g = mu.g,mu_g0 = mu.g0,mu_g1 = mu.g1,
                         var_g0 = var.g0,var_g1 = var.g1)
  
  
  ## Combined approach
  
  ## Sampling target sample to combined approach
  
  out.comb = matrix(nrow = n.methods,ncol = 4*length(nL))
  out.comb.new = matrix(nrow = n.methods,ncol = 4*length(nL))
  out.names = NULL
  for(j in 1:length(nL)){
    
    index.comb = sample(1:ntg,nL[j])+ntr   
    
    
    ## Ratio without labeled observations 
    
    
    if(n.methods == 1){
      gx.temp = gx[ ,-index.comb] 
      mu.g.comb   = mean(gx.temp[-c(1:ntr)])
      mu.g0.comb  = mean(gx.temp[index.0])
      mu.g1.comb  = mean(gx.temp[index.1])
      var.g0.comb = var(gx.temp[index.0])
      var.g1.comb = var(gx[index.1])
      
    }else{
      gx.temp = gx[ ,-index.comb] 
      mu.g.comb   = apply(gx.temp[ ,-c(1:ntr)],1,mean)
      mu.g0.comb  = apply(gx.temp[ ,index.0],1,mean)
      mu.g1.comb  = apply(gx.temp[ ,index.1],1,mean)
      var.g0.comb = apply(gx.temp[ ,index.0],1,var)
      var.g1.comb = apply(gx.temp[ ,index.1],1,var)
    }
    
    p.comb = (mu.g.comb-mu.g0.comb)/(mu.g1.comb-mu.g0.comb)
    p.comb[p.comb<0] = 0
    p.comb[p.comb>1] = 1
    ntg.comb = ntg - nL[j]
    
    ## Variance ratio estimator
    temp1 = 1/((mu.g1.comb - mu.g0.comb)^2)
    temp2 = ((1/temp1)*p.comb*(1-p.comb) + var.g1.comb*p.comb + var.g0.comb*(1-p.comb))/ntg.comb
    temp3 = (var.g0.comb*((1-p.comb)^2))/(ntr*(1-thetatr)) + (var.g1.comb*(p.comb^2))/(ntr*thetatr)
    
    varRatio = temp1*(temp2 + temp3)
    
    theta.labeled = mean(y.target[index.comb - ntr])
    
    # Sem corrigir
    varLabeled = theta.labeled*(1-theta.labeled)/nL[j]
    
    w = varLabeled/(varLabeled + varRatio)
    
    comb = w*p.comb + (1-w)*theta.labeled
    comb[comb<0] = 0
    comb[comb>1] = 1
    
    out.comb[ ,1+4*(j-1)] = p.comb
    out.comb[ ,2+4*(j-1)] = theta.labeled
    out.comb[ ,3+4*(j-1)] = w
    out.comb[ ,4+4*(j-1)] = comb
    
    
    ## Corrigindo
    
    if(theta.labeled == 0){ 
      
      varLabeled = 1/4
      
      w = varLabeled/(varLabeled + varRatio)
      
      comb = w*p.comb + (1-w)*theta.labeled
      comb[comb < 0] = 0
      comb[comb > 1] = 1
      
      out.comb.new[ ,1+4*(j-1)] = p.comb
      out.comb.new[ ,2+4*(j-1)] = theta.labeled
      out.comb.new[ ,3+4*(j-1)] = w
      out.comb.new[ ,4+4*(j-1)] = comb
      
    }else{
      
      out.comb.new[ ,1+4*(j-1)] = out.comb[ ,1+4*(j-1)]
      out.comb.new[ ,2+4*(j-1)] = out.comb[ ,2+4*(j-1)]
      out.comb.new[ ,3+4*(j-1)] = out.comb[ ,3+4*(j-1)]
      out.comb.new[ ,4+4*(j-1)] = out.comb[ ,4+4*(j-1)]
      
    }
    
    out.names = c(out.names,paste0(c("Ratio","Labeled","w","comb"),sep = "_",nL[j]))
    
  }
  
  out.comb = data.frame(out.comb)
  out.comb.new = data.frame(out.comb.new)
  
  names(out.comb) = out.names
  names(out.comb.new) = out.names
  
  
  out.comb = cbind(method = names(g),out.comb)
  
  out.comb.new = cbind(method = names(g),out.comb.new)
  
  out = list(ratio = out.ratio,comb = out.comb,comb.new = out.comb.new)
  
  return(out)
}



####################################
############## Methods #############
####################################

#install.packages("class")
#install.packages("randomForest")
#install.packages("fields")
#install.packages("geigen")
library(class)
library(randomForest)
library(fields)
library(geigen)
library(FNN)

## Forman using random forest classifier
## "extra" parameter is a list cotaining only the n.tree parameter 
g.forman.random = function(y.train,x.train,x.target,extra){
  set.seed(10)
  n.tree = extra$n.tree
  g.x.train = vector(length = nrow(x.train))
  covariates = colnames(x.train)
  formula = paste(covariates,collapse="+")
  formula = paste("as.factor(response) ~ ",formula)
  data = as.data.frame(cbind(response=y.train,x.train))
  
  for(i in 1:nrow(x.train)){
    
    fit = randomForest(as.formula(formula),method = "class",data=data[-i, ],
                       importance = TRUE, ntree = n.tree)
    
    g.x.train[i] = as.numeric(predict(fit,x.train[i, ]))-1
    
  }
  
  fit = randomForest(as.formula(formula),method = "class",data=data,
                     importance = TRUE, ntree=n.tree)
  
  g.x = c(g.x.train,as.numeric(predict(fit,x.target))-1)
  
  return(g.x)
  
}

## Bella using random forest estimator
## "extra" parameter is a list cotaining only the n.tree parameter 
g.bella.random = function(y.train,x.train,x.target,extra){
  
  n.tree=extra$n.tree
  
  covariates = names(x.train)
  g.x.train = vector(length = nrow(x.train))
  formula = paste(covariates,collapse="+")
  formula = paste("as.factor(response) ~ ",formula)
  data = cbind(response = y.train,x.train)
  
  for(i in 1:nrow(x.train)){
    
    fit = randomForest(as.formula(formula),method = "class",data=data[-i, ],
                       importance = TRUE, ntree=n.tree)
    
    g.x.train[i] = predict(fit,x.train[i, ],type = "prob")[,"1"]
    
  }
  
  
  fit = randomForest(as.formula(formula),method = "class",data=cbind(response = y.train,x.train),
                     importance = TRUE, ntree = n.tree)
  
  
  g.x = c(g.x.train,
          predict(fit,newdata = x.target,type = "prob")[,"1"])
  
  return(g.x)
  
}

## Ratio estimator using RKHS
## "extra" parameter is a list cotaining "lambda", "kernel" wich represent a kernel function and
## "bandwidth" wich represent the kernel function bandwidth

## gaussian.kernel is a secundary function of Kernel's Method
gaussian.kernel=function(x.data.1,x.data.2,extra)
{
  return(exp(-(fields::rdist(x.data.1,x.data.2))^2/extra$bandwidth))
}

## linear.kernel is a secundary function of Kernel's Method
linear.kernel = function(x.data.1,x.data,extra){
  
  return(x.data.1 %*% t(x.data))
  
}

## compute_weights is a secundary function of Kernel's Method
compute_weights <- function(kernel.matrix,which.label.0,which.label.1,
                            lambda,theta.hat)
{
  mu.0=colMeans(kernel.matrix[which.label.0,])
  mu.1=colMeans(kernel.matrix[which.label.1,])
  M=(mu.1-mu.0)%*%t(mu.1-mu.0)
  Sigma.0=cov(kernel.matrix[which.label.0,])
  Sigma.1=cov(kernel.matrix[which.label.1,])
  
  weight_N_1 <- (theta.hat^2)/mean(which.label.1)
  weight_N_0 <- ((1-theta.hat)^2)/mean(which.label.0)
  N=(weight_N_1*Sigma.1+weight_N_0*Sigma.0)
  espectral=eigen(N)
  U.inverse=solve(espectral$vectors)
  N.inverse=t(U.inverse)%*%diag(1/(espectral$values+lambda))%*%U.inverse
  decomposition=eigen(N.inverse%*%M)
  weights=Re(decomposition$vectors[,1])     
  
  return(weights)
}

g.kernel.leave=function(y.train,x.train,x.target,
                        extra=list(lambda=0.001,kernel=gaussian.kernel)){
  
  forman.random = list(func = g.forman.random,
                       extra = list(n.tree = 100))
  
  g=list(forman.random = forman.random)
  
  extra$theta.hat = quantification.prior.shift(x.train  = x.train,
                                               y.train = y.train,
                                               x.target = x.target,
                                               y.target = y.target,
                                               g = g,nL = nL)$ratio$estimative
  
  if(extra$theta.hat==0 | extra$theta.hat==1)
    extra$theta.hat=0.5 
  
  all = rbind(x.train,x.target)
  id = which(sapply(all, sum) != 0)
  
  aux.scale=scale(all[,id])
  x.train=aux.scale[1:nrow(x.train),]
  x.target=aux.scale[-c(1:nrow(x.train)),]
  which.estimate=sample(1:nrow(x.train),round(0.3*nrow(x.train)))
  
  sample.size=min(c(nrow(x.train),400))
  sample=sample(1:nrow(x.train),sample.size)
  bandwidth.grid=quantile(c(rdist(x.train[sample,],x.train[sample,])^2),
                          probs=c(0.01,0.015,0.02,0.025,0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95,0.99))
  temp.index = which(bandwidth.grid == 0)
  
  if(length(temp.index)) bandwidth.grid = bandwidth.grid[-temp.index]
  
  prob.grid = c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  mse=matrix(NA,length(bandwidth.grid),length(prob.grid))
  
  lambda.grid.matrix <- matrix(NA,length(bandwidth.grid),length(prob.grid))
  
  for(band in 1:length(bandwidth.grid))
  {
    kernel.matrix=extra$kernel(x.train[-which.estimate,],
                               x.train[-which.estimate,],
                               extra=list(bandwidth=bandwidth.grid[band]))
    
    mu.0=colMeans(kernel.matrix[y.train[-which.estimate]==0,])
    mu.1=colMeans(kernel.matrix[y.train[-which.estimate]==1,])
    Sigma.0=cov(kernel.matrix[y.train[-which.estimate]==0,])
    Sigma.1=cov(kernel.matrix[y.train[-which.estimate]==1,])
    
    M=(mu.1-mu.0)%*%t(mu.1-mu.0)
    weight_N_1 <- (extra$theta.hat^2)/mean(y.train[-which.estimate]==1)
    weight_N_0 <- ((1-extra$theta.hat)^2)/mean(y.train[-which.estimate]==0)
    N=(weight_N_1*Sigma.1+weight_N_0*Sigma.0)
    
    lambda.grid = quantile(abs(N),probs=prob.grid)
    index.temp = which(lambda.grid == 0)
    shift = length(index.temp)
    if(shift != 0) lambda.grid = lambda.grid[-index.temp]
    
    espectral=try(eigen(N),silent = TRUE)
    
    if(identical(class(espectral),"try-error"))
      next;
    
    U.inverse=try(solve(espectral$vectors),silent = TRUE)
    
    if(identical(class(U.inverse),"try-error"))
      next;
    
    for(lambda in 1:length(lambda.grid))
    {
      N.inverse=try(t(U.inverse)%*%diag(1/(espectral$values+lambda.grid[lambda]))%*%U.inverse,
                    silent = T)
      
      if(identical(class(N.inverse),"try-error"))
        next;
      
      decomposition=try(eigen(N.inverse%*%M),silent = TRUE)
      if(identical(class(decomposition),"try-error"))
        next;
      
      weights=Re(decomposition$vectors[,1])  
      if(mean(weights<0)>0.5)
        weights <- -weights
      
      g.estimate=extra$kernel(x.train[which.estimate,],
                              x.train[-which.estimate,],
                              extra=list(bandwidth=bandwidth.grid[band]))%*%weights
      mean.0=mean(g.estimate[y.train[which.estimate]==0])
      mean.1=mean(g.estimate[y.train[which.estimate]==1])
      var.0=var(g.estimate[y.train[which.estimate]==0])
      var.1=var(g.estimate[y.train[which.estimate]==1])
      
      mse[band,lambda + shift]=1/(nrow(x.train)*(mean.1-mean.0)^2)*
        (extra$theta.hat^2*var.1/mean(y.train==1)+(1-extra$theta.hat)^2*var.0/mean(y.train==0))+
        1/(nrow(x.target)*(mean.1-mean.0)^2)*(extra$theta.hat*var.1+(1-extra$theta.hat)*var.0)

    }
    
    lambda.grid.matrix[band,] <- lambda.grid
  } 
  
  index=which(mse==min(mse,na.rm = T),arr.ind=TRUE)
  best.band=bandwidth.grid[index[1]]
  best.lambda=lambda.grid.matrix[index[1],index[2]]
  kernel.matrix=extra$kernel(x.train,
                             x.train,
                             extra=list(bandwidth=best.band))
  weights=compute_weights(kernel.matrix,y.train==0,
                          y.train==1,best.lambda,extra$theta.hat)
  
  weights_direction=weights
  kernel.matrix_direction=kernel.matrix
  gx.tg <- extra$kernel(x.target,x.train,
                        extra=list(bandwidth=best.band))%*%weights
  
  
  nFolds=nrow(x.train)
  which.fold=1:nFolds
  sim_vec= gx.tr=rep(NA,nrow(x.train))
  
  for(i in 1:nFolds) # estimate g on the i-th fold
  {
    kernel.matrix=extra$kernel(x.train[which.fold!=i, ,drop=FALSE],
                               x.train[which.fold!=i, ,drop=FALSE],
                               extra=list(bandwidth=best.band))
    weights=compute_weights(kernel.matrix,y.train[which.fold!=i]==0,
                            y.train[which.fold!=i]==1,best.lambda,extra$theta.hat)
    
    sim <- weights %*% kernel.matrix_direction[which.fold!=i,,drop=FALSE] %*% weights_direction
    if(sim<0)
      weights <- -weights
    sim_vec[i] <- sim
    gx.tr[which.fold==i] <- extra$kernel(x.train[which.fold==i, ,drop=FALSE],
                                         x.train[which.fold!=i, ,drop=FALSE],
                                         extra=list(bandwidth=best.band))%*%weights
    
  }
  
  return(c(gx.tr,gx.tg))
  
}



## Forman using Logistic Regression
## There is no "extra" argument
g.logistic=function(y.train,x.train,x.target,extra){
  
  data=cbind(y.train,x.train)
  
  gx.train = vector(length = nrow(x.train))
  
  for(i in 1:nrow(x.train)){
    
    model=glm(y.train ~ . ,family = "binomial",data=data[-i,])
    
    gx.train[i] = ifelse(predict(model,newdata=x.train[i,],type = "response")>= 0.5,1,0)
    
  }
  
  model=glm(y.train ~ . ,family = "binomial",data=data)
  
  gx.target = ifelse(predict(model,newdata=x.target,type = "response")>= 0.5,1,0)
  
  return(c(gx.train,gx.target))
}

## Bella using Logistic Regression
## There is no "extra" argument
g.bella.logistic=function(y.train,x.train,x.target,extra){
  
  data=cbind(y.train,x.train)
  
  gx.train = vector(length = nrow(x.train))
  
  for(i in 1:nrow(x.train)){
    
    model=glm(y.train ~ . ,family = "binomial",data=data[-i,])
    
    gx.train[i] = predict(model,newdata=x.train[i,],type = "response")
    
  }
  
  model=glm(y.train ~ . ,family = "binomial",data=data)
  
  gx.target = predict(model,newdata=x.target,type = "response")
  
  return(c(gx.train,gx.target))
}



## Forman using KNN classifier
normalize <- function(data) {
  data=as.data.frame(data)
  index.col=sapply(data,is.numeric)
  data[,index.col]=(data[,index.col] - min(data[,index.col])) /(max(data[,index.col]) - min(data[,index.col]))
  return (data)
}

g.forman.knn=function(y.train,x.train,x.target,extra = NULL){
  
  all = rbind(x.train,x.target)
  id = which(apply(all,MARGIN = 2, FUN = function(x){return(length(unique(x)))}) != 0)
  aux.scale=scale(all[,id])
  
  x.train=aux.scale[1:nrow(x.train),]
  x.target=aux.scale[-c(1:nrow(x.train)),]
  
  gx.train = factor(levels = c(0,1))
  
  y.train = as.factor(y.train)
  
  n = 50
  grid = round(seq(1,round(nrow(x.train)/2),length.out = n))
  k = error = vector(length =  n)
  
  for(i in 1:n){
    teste = knn.cv(x.train, y.train, k = grid[i], prob = FALSE)
    pred = factor(teste[1:nrow(x.train)],levels = levels(y.train))
    k[i] = grid[i]
    error[i] = mean(y.train != pred)
  }
  
  
  k.best = k[which.min(error)]  
  
  for( i in 1:nrow(x.train)){
    
    #print(i)
    
    gx.train[i] = knn(train=x.train[-i, ],test=x.train[i,],
                      cl = y.train[-i],k = k.best)[1]
    
  }
  gx.target = knn(train=x.train,test=x.target,
                  cl = y.train,k = k.best)
  
  return(as.numeric(c(gx.train,gx.target))-1)
}


## Bella using KNN classifier
g.bella.knn = function(y.train,x.train,x.target,extra = NULL){
  
  all = rbind(x.train,x.target)
  id = which(sapply(all, sum) != 0)
  aux.scale=scale(all[,id])
  
  x.train=aux.scale[1:nrow(x.train),]
  x.target=aux.scale[-c(1:nrow(x.train)),]
  
  gx.train = vector(length = nrow(x.train))
  
  n = 50
  grid = round(seq(1,round(nrow(x.train)/2),length.out = n))
  k = error = vector(length =  n)
  
  for(i in 1:n){
    teste = knn.reg(train = x.train, y = y.train, k = grid[i])
    k[i] = grid[i]
    error[i] = teste$PRESS
  }
  
  k.best = k[which.min(error)]
  
  for( i in 1:nrow(x.train)){
    
    gx.train[i] = knn.reg(train=x.train[-i, ],test=x.train[i,],
                          y = y.train[-i],k = k.best)$pred
    
  }
  
  gx.target =  knn.reg(train=x.train,test=x.target,
                       y = y.train,k = k.best)$pred
  
  return(c(gx.train,gx.target))
  
} 


## EM method
em_method = function(x.train,y.train,x.target,y.target,eps = 10e-60,n.inter = 1000,g){
  
  n.methods   = length(g)
  theta = vector(length = length(g))
  
  for(j in 1:n.methods){
    
    p_train = mean(y.train)
    p_target_0 = p_train
    
    g_target = g[[j]]$func(y.train,x.train,x.target,g[[j]]$extra)[-c(1:length(y.train))]
    
    p_w_x = vector(length = length(y.target))
    
    error = 10000
    interection = 1
    
    while (error > eps  & n.inter >= interection) {
      
      for(i in 1:length(p_w_x)){
        
        num = ((p_target_0/p_train)*g_target[i])
        den = ((1-p_target_0)/(1-p_train))*(1-g_target[i]) + (p_target_0/p_train)*g_target[i]
        
        p_w_x[i] = num/den
        
        
      }
      
      error = abs(p_target_0 - mean(p_w_x))
      interection = interection + 1
      p_target_0 = mean(p_w_x)
      
    }
    
    theta[j] = p_target_0
    
  }
  
  out.em = data.frame(method = paste0(names(g)),estimative = theta,
                      mu_g = NA,mu_g0 = NA,mu_g1 = NA,
                      var_g0 = NA,var_g1 = NA)
  
  return(out.em)
  
}