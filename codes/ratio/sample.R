## Sampling codes

## Finding ntr and ntg values for a theta grid

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
      print((j + n.sim*(i-1))/(length(grid)*n.sim))  
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

