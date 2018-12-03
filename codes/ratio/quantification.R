quantification.prior.shift = function(x.train,
                                      y.train,
                                      x.target,
                                      y.target,
                                      g,
                                      nL){
    
  ##Inputs:
  ##x.train  - a (nL x d) data.frame cotaining the covariates in the training sample
  ##y.train  - a (nL x 1) vector containing the response in the training sample
  ##x.target - a (nU x d) data.frame containing the covariates in the target sample
  ##g        - a list containing the methods that will be considered
  
  ##Outputs: 
  ##out      - a list containing the proportions estimatives for each method
  
  
  n.methods   = length(g)
  p           = vector(length = n.methods)
  #p.comb      = vector(length = n.methods)
  mu.g        = vector(length = n.methods)
  #mu.g.comb   = vector(length = n.methods)
  mu.g0       = vector(length = n.methods)
  #mu.g0.comb  = vector(length = n.methods)
  mu.g1       = vector(length = n.methods)
  #mu.g1.comb  = vector(length = n.methods)
  var.g1      = vector(length = n.methods)
  #var.g1.comb = vector(length = n.methods)
  var.g0      = vector(length = n.methods)
  #var.g0.comb = vector(length = n.methods)
  index.0   = which(y.train==0)
  index.1   = which(y.train==1)
  
  ntr = nrow(x.train)
  
  ntg = nrow(x.target)
  
  gx = matrix(ncol = ntr + ntg, nrow = n.methods)
  
  thetatr = mean(y.train)

  
  
  ##Evaluate the necessers quantities to ratio and combined estimators 
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
  
  ##Ratio estimator estimative
  p = (mu.g-mu.g0)/(mu.g1-mu.g0)
  p[p<0] = 0
  p[p>1] = 1
  
  
  out.ratio = data.frame(method = names(g),estimative = p,
                         mu_g = mu.g,mu_g0 = mu.g0,mu_g1 = mu.g1,
                         var_g0 = var.g0,var_g1 = var.g1)
  
  
  ##Combined approach
    
  ##Sampling target sample to combined approach
  
  out.comb = matrix(nrow = n.methods,ncol = 4*length(nL))
  out.comb.new = matrix(nrow = n.methods,ncol = 4*length(nL))
  out.names = NULL
  for(j in 1:length(nL)){
    
    index.comb = sample(1:ntg,nL[j])+ntr   
    
    
    ##Ratio without labeled observations 
    
    
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
    
    ##Variance ratio estimator
    temp1 = 1/((mu.g1.comb - mu.g0.comb)^2)
    temp2 = ((1/temp1)*p.comb*(1-p.comb) + var.g1.comb*p.comb + var.g0.comb*(1-p.comb))/ntg.comb
    temp3 = (var.g0.comb*((1-p.comb)^2))/(ntr*(1-thetatr)) + (var.g1.comb*(p.comb^2))/(ntr*thetatr)
    
    varRatio = temp1*(temp2 + temp3)
    
    theta.labeled = mean(y.target[index.comb - ntr])
    
    #Sem corrigir
    varLabeled = theta.labeled*(1-theta.labeled)/nL[j]
    
    w = varLabeled/(varLabeled + varRatio)
    
    comb = w*p.comb + (1-w)*theta.labeled
    comb[comb<0] = 0
    comb[comb>1] = 1
    
    out.comb[ ,1+4*(j-1)] = p.comb
    out.comb[ ,2+4*(j-1)] = theta.labeled
    out.comb[ ,3+4*(j-1)] = w
    out.comb[ ,4+4*(j-1)] = comb
    
    
    ##Corrigindo
    
    if(theta.labeled == 0){    
    varLabeled = 1/4

    w = varLabeled/(varLabeled + varRatio)
    
    comb = w*p.comb + (1-w)*theta.labeled
    comb[comb<0] = 0
    comb[comb>1] = 1
    
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



#quantification.prior.shift(x.train,y.train,x.target,y.target,g,nL)
