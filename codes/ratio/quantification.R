## Specifying the g's functions and parameters

forman.random=list(func=g.forman.random,extra=list(n.tree=1000))
bella.random=list(func=g.bella.random,extra=list(n.tree=1000))

forman.logistic=list(func=g.logistic,extra=NULL)
bella.logistic=list(func=g.bella.logistic,extra=NULL)

kernel.gaussian=list(func=g.kernel.leave,extra=list(lambda=0.001,kernel=gaussian.kernel,
                                                    bandwidth=NULL))

kernel.linear=list(func=g.kernel.leave,extra=list(lambda=0.001,kernel=linear.kernel,
                                                  bandwidth=NULL))

forman.knn = list(func = g.forman.knn)


bella.knn = list(func = g.bella.knn)


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
source(file = "codes/quantification.R")

load(file = "outputs/datasets/dataset.cancer.RData")
load(file = "outputs/samples/sample.cancer.RData")

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
    
    
    save(out.cancer.ratio,file = "outputs/results/out.cancer.ratio.teste.RData")
    save(out.cancer.comb.new,file = "outputs/results/out.cancer.comb.new.teste.RData")
    ind=c(i,j)
    save(ind,file = "outputs/results/out.cancer.teste.RData")
    
  }
}


#########################################
######### Candles dataset ###############
#########################################





#########################################
########## Block dataset ################
#########################################





#########################################
########## Spam dataset #################
#########################################




#########################################
########## Bank dataset #################
#########################################