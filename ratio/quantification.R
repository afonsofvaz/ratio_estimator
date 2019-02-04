## Specifying the g's functions and parameters
forman.random = list(func=g.forman.random,extra=list(n.tree=1000))
bella.random = list(func=g.bella.random,extra=list(n.tree=1000))

forman.logistic = list(func=g.logistic,extra=NULL)
bella.logistic = list(func=g.bella.logistic,extra=NULL)

forman.knn = list(func = g.forman.knn)
bella.knn = list(func = g.bella.knn)

kernel.gaussian = list(func=g.kernel.leave,extra=list(lambda=0.001,kernel=gaussian.kernel,
                                                    bandwidth=NULL))
kernel.linear = list(func=g.kernel.leave,extra=list(lambda=0.001,kernel=linear.kernel,
                                                  bandwidth=NULL))

g=list(forman.random = forman.random,bella.random = bella.random,
       forman.logistic = forman.logistic,bella.logistic = bella.logistic,
       forman.knn = forman.knn,bella.knn = bella.knn,
       kernel.gaussian = kernel.gaussian,kernel.linear = kernel.linear)

theta.grid = seq(0.1,0.5,by = 0.1)
n.sim = 100
nL = c(10,20,30,50)

#########################################
######### Cancer dataset ################
#########################################

set.seed(10)

source(file ="codes/aux")

load(file = "codes/ratio/inputs/datasets/dataset.cancer.RData")
load(file = "codes/ratio/inputs/samples/sample.cancer.RData")

i=1
j=1

x.train=dataset.cancer[sample.cancer[[i]]$train[j,],
                       -c(which(names(dataset.cancer)=="response"))]
y.train=dataset.cancer[sample.cancer[[i]]$train[j,],"response"]

x.target=dataset.cancer[sample.cancer[[i]]$target[j,],
                        -c(which(names(dataset.cancer)=="response"))]
y.target=dataset.cancer[sample.cancer[[i]]$target[j,],"response"]

out.cancer = NULL

out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)

out.cancer.ratio = cbind(Id = rep(j,length(g)), n0 = rep(sample.cancer[[i]]$inf[1,1],length(g)),
                         n1 = rep(sample.cancer[[i]]$inf[1,2],length(g)),
                         theta_tg = rep(theta.grid[i],length(g)),
                         out$ratio)

out.cancer.comb.new = cbind(Id = rep(j,length(g)), n0 = rep(sample.cancer[[i]]$inf[1,1],length(g)),
                            n1 = rep(sample.cancer[[i]]$inf[1,2],length(g)),
                            theta_tg = rep(theta.grid[i],length(g)),
                            out$comb.new)

for(j in 1:n.sim){
  for(i in 1:length(theta.grid)){
    
    print((i +length(theta.grid)*(j-1))/(length(theta.grid)*n.sim))
    
    x.train=dataset.cancer[sample.cancer[[i]]$train[j,],
                           -c(which(names(dataset.cancer)=="response"))]
    y.train=dataset.cancer[sample.cancer[[i]]$train[j,],"response"]
    
    x.target=dataset.cancer[sample.cancer[[i]]$target[j,],
                            -c(which(names(dataset.cancer)=="response"))]
    y.target=dataset.cancer[sample.cancer[[i]]$target[j,],"response"]
    
    out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)
    
    out.cancer.ratio = rbind(out.cancer.ratio,cbind(Id = rep(j,length(g)), n0 = rep(sample.cancer[[i]]$inf[1,1],length(g)),
                                                    n1 = rep(sample.cancer[[i]]$inf[1,2],length(g)),
                                                    theta_tg = rep(theta.grid[i],length(g)),
                                                    out$ratio))
    
    out.cancer.comb.new = rbind(out.cancer.comb.new,cbind(Id = rep(j,length(g)), n0 = rep(sample.cancer[[i]]$inf[1,1],length(g)),
                                                          n1 = rep(sample.cancer[[i]]$inf[1,2],length(g)),
                                                          theta_tg = rep(theta.grid[i],length(g)),
                                                          out$comb.new))
    
    
    save(out.cancer.ratio,file = "codes/ratio/outputs/results/out.cancer.ratio.RData")
    save(out.cancer.comb.new,file = "codes/ratio/outputs/results/out.cancer.comb.new.RData")
    
  }
}



#########################################
########## Block dataset ################
#########################################

set.seed(10)

source(file ="codes/aux")

load(file = "codes/ratio/inputs/datasets/dataset.block.RData")
load(file = "codes/ratio/inputs/samples/sample.block.RData")

i=1
j=1

x.train=dataset.block[sample.block[[i]]$train[j,],
                       -c(which(names(dataset.block)=="response"))]
y.train=dataset.block[sample.block[[i]]$train[j,],"response"]

x.target=dataset.block[sample.block[[i]]$target[j,],
                        -c(which(names(dataset.block)=="response"))]
y.target=dataset.block[sample.block[[i]]$target[j,],"response"]

out.block = NULL

out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)

out.block.ratio = cbind(Id = rep(j,length(g)), n0 = rep(sample.block[[i]]$inf[1,1],length(g)),
                         n1 = rep(sample.block[[i]]$inf[1,2],length(g)),
                         theta_tg = rep(theta.grid[i],length(g)),
                         out$ratio)

out.block.comb.new = cbind(Id = rep(j,length(g)), n0 = rep(sample.block[[i]]$inf[1,1],length(g)),
                            n1 = rep(sample.block[[i]]$inf[1,2],length(g)),
                            theta_tg = rep(theta.grid[i],length(g)),
                            out$comb.new)

for(j in 1:n.sim){
  for(i in 1:length(theta.grid)){
    
    print((i +length(theta.grid)*(j-1))/(length(theta.grid)*n.sim))
    
    x.train=dataset.block[sample.block[[i]]$train[j,],
                           -c(which(names(dataset.block)=="response"))]
    y.train=dataset.block[sample.block[[i]]$train[j,],"response"]
    
    x.target=dataset.block[sample.block[[i]]$target[j,],
                            -c(which(names(dataset.block)=="response"))]
    y.target=dataset.block[sample.block[[i]]$target[j,],"response"]
    
    out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)
    
    out.block.ratio = rbind(out.block.ratio,cbind(Id = rep(j,length(g)), n0 = rep(sample.block[[i]]$inf[1,1],length(g)),
                                                    n1 = rep(sample.block[[i]]$inf[1,2],length(g)),
                                                    theta_tg = rep(theta.grid[i],length(g)),
                                                    out$ratio))
    
    out.block.comb.new = rbind(out.block.comb.new,cbind(Id = rep(j,length(g)), n0 = rep(sample.block[[i]]$inf[1,1],length(g)),
                                                          n1 = rep(sample.block[[i]]$inf[1,2],length(g)),
                                                          theta_tg = rep(theta.grid[i],length(g)),
                                                          out$comb.new))
    
    
    save(out.block.ratio,file = "codes/ratio/outputs/results/out.block.ratio.RData")
    save(out.block.comb.new,file = "codes/ratio/outputs/results/out.block.comb.new.RData")
    
  }
}




#########################################
########## Spam dataset #################
#########################################

set.seed(10)

source(file ="codes/aux")

load(file = "codes/ratio/inputs/datasets/dataset.spam.RData")
load(file = "codes/ratio/inputs/samples/sample.spam.RData")

i=1
j=1

x.train=dataset.spam[sample.spam[[i]]$train[j,],
                       -c(which(names(dataset.spam)=="response"))]
y.train=dataset.spam[sample.spam[[i]]$train[j,],"response"]

x.target=dataset.spam[sample.spam[[i]]$target[j,],
                        -c(which(names(dataset.spam)=="response"))]
y.target=dataset.spam[sample.spam[[i]]$target[j,],"response"]

out.spam = NULL

out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)

out.spam.ratio = cbind(Id = rep(j,length(g)), n0 = rep(sample.spam[[i]]$inf[1,1],length(g)),
                         n1 = rep(sample.spam[[i]]$inf[1,2],length(g)),
                         theta_tg = rep(theta.grid[i],length(g)),
                         out$ratio)

out.spam.comb.new = cbind(Id = rep(j,length(g)), n0 = rep(sample.spam[[i]]$inf[1,1],length(g)),
                            n1 = rep(sample.spam[[i]]$inf[1,2],length(g)),
                            theta_tg = rep(theta.grid[i],length(g)),
                            out$comb.new)

for(j in 1:n.sim){
  for(i in 1:length(theta.grid)){
    
    print((i +length(theta.grid)*(j-1))/(length(theta.grid)*n.sim))
    
    x.train=dataset.spam[sample.spam[[i]]$train[j,],
                           -c(which(names(dataset.spam)=="response"))]
    y.train=dataset.spam[sample.spam[[i]]$train[j,],"response"]
    
    x.target=dataset.spam[sample.spam[[i]]$target[j,],
                            -c(which(names(dataset.spam)=="response"))]
    y.target=dataset.spam[sample.spam[[i]]$target[j,],"response"]
    
    out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)
    
    out.spam.ratio = rbind(out.spam.ratio,cbind(Id = rep(j,length(g)), n0 = rep(sample.spam[[i]]$inf[1,1],length(g)),
                                                    n1 = rep(sample.spam[[i]]$inf[1,2],length(g)),
                                                    theta_tg = rep(theta.grid[i],length(g)),
                                                    out$ratio))
    
    out.spam.comb.new = rbind(out.spam.comb.new,cbind(Id = rep(j,length(g)), n0 = rep(sample.spam[[i]]$inf[1,1],length(g)),
                                                          n1 = rep(sample.spam[[i]]$inf[1,2],length(g)),
                                                          theta_tg = rep(theta.grid[i],length(g)),
                                                          out$comb.new))
    
    
    save(out.spam.ratio,file = "codes/ratio/outputs/results/out.spam.ratio.RData")
    save(out.spam.comb.new,file = "codes/ratio/outputs/results/out.spam.comb.new.RData")
    
  }
}


#########################################
########## Bank dataset #################
#########################################

set.seed(10)

source(file ="codes/aux")

load(file = "codes/ratio/inputs/datasets/dataset.bank.RData")
load(file = "codes/ratio/inputs/samples/sample.bank.RData")

i=1
j=1

x.train=dataset.bank[sample.bank[[i]]$train[j,],
                       -c(which(names(dataset.bank)=="response"))]
y.train=dataset.bank[sample.bank[[i]]$train[j,],"response"]

x.target=dataset.bank[sample.bank[[i]]$target[j,],
                        -c(which(names(dataset.bank)=="response"))]
y.target=dataset.bank[sample.bank[[i]]$target[j,],"response"]

out.bank = NULL

out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)

out.bank.ratio = cbind(Id = rep(j,length(g)), n0 = rep(sample.bank[[i]]$inf[1,1],length(g)),
                         n1 = rep(sample.bank[[i]]$inf[1,2],length(g)),
                         theta_tg = rep(theta.grid[i],length(g)),
                         out$ratio)

out.bank.comb.new = cbind(Id = rep(j,length(g)), n0 = rep(sample.bank[[i]]$inf[1,1],length(g)),
                            n1 = rep(sample.bank[[i]]$inf[1,2],length(g)),
                            theta_tg = rep(theta.grid[i],length(g)),
                            out$comb.new)

for(j in 1:n.sim){
  for(i in 1:length(theta.grid)){
    
    print((i +length(theta.grid)*(j-1))/(length(theta.grid)*n.sim))
    
    x.train=dataset.bank[sample.bank[[i]]$train[j,],
                           -c(which(names(dataset.bank)=="response"))]
    y.train=dataset.bank[sample.bank[[i]]$train[j,],"response"]
    
    x.target=dataset.bank[sample.bank[[i]]$target[j,],
                            -c(which(names(dataset.bank)=="response"))]
    y.target=dataset.bank[sample.bank[[i]]$target[j,],"response"]
    
    out = quantification.prior.shift(x.train,y.train,x.target,y.target = y.target,nL = nL,g)
    
    out.bank.ratio = rbind(out.bank.ratio,cbind(Id = rep(j,length(g)), n0 = rep(sample.bank[[i]]$inf[1,1],length(g)),
                                                    n1 = rep(sample.bank[[i]]$inf[1,2],length(g)),
                                                    theta_tg = rep(theta.grid[i],length(g)),
                                                    out$ratio))
    
    out.bank.comb.new = rbind(out.bank.comb.new,cbind(Id = rep(j,length(g)), n0 = rep(sample.bank[[i]]$inf[1,1],length(g)),
                                                          n1 = rep(sample.bank[[i]]$inf[1,2],length(g)),
                                                          theta_tg = rep(theta.grid[i],length(g)),
                                                          out$comb.new))
    
    
    save(out.bank.ratio,file = "codes/ratio/outputs/results/out.bank.ratio.RData")
    save(out.bank.comb.new,file = "codes/ratio/outputs/results/out.bank.comb.new.RData")
    
  }
}
















