## Parameters
## mu in {0.5,1,1.5,2}
## n_labeled in {100,500}
## B = 200
## k in {1,2}
## n_grid = 10000
## n_methods = 4
## n_rows = n_methods*n_grid*B*|mu|*|k| = 4*10000*200*2*2
## cols = c("method","result","real","k","b","mu")

n_methods = 3
n_grid = 10000
B = 200
k_length = 2
mu_length = 4
n_labeled_length = 2


results = matrix(nrow = n_methods*n_grid*B*k_length*mu_length*n_labeled_length,
                 ncol = 7)
results = as.data.frame(results)
names(results) = c("method","result","real","k","b","mu","n_L")
head(results)

B = 100
mu = 0.5
n_labeled = 200

for(B in 1:200){

  print(B)

  for(mu in c(0.5,1,1.5,2)){
     if(identical(mu,0.5))
       id_mu = 1
     if(identical(mu,1))
       id_mu = 2
     if(identical(mu,1.5))
       id_mu = 3
     if(identical(mu,2))
       id_mu = 4

    for(n_labeled in c(200,500)){
      if(identical(n_labeled,200))
        id_nl = 1
      if(identical(n_labeled,500))
        id_nl = 2

      for(k in c(1,2)){
        test = try(load(paste0("ratio_regression/outputs/results/",mu,"_",k,"_",n_labeled,"_",B,".RData")),
                   silent = T)
        if(identical(class(test),"try-error"))
          next;
        
        index_start = (k-1)*n_grid*n_methods + (id_nl-1)*2*n_grid*n_methods +
                      (id_mu-1)*2*2*n_grid*n_methods + (B-1)*2*2*4*n_grid*n_methods + 1
        index_stop = index_start + n_grid*n_methods -1
        index_int = index_start:index_stop
        
        results$method[index_int] = c(rep("CC",n_grid),rep("Ratio",n_grid),
                                      rep("Labeled",n_grid))
        results$real[index_int] = rep(sample_reg$rz_grid,n_methods)
        results$result[index_int] = c(cc_reg$predict,ratio_reg,labeled_reg$predict)
        results$k[index_int] = rep(k,length(index_int))
        results$b[index_int] = rep(B,length(index_int))
        results$mu[index_int] = rep(mu,length(index_int))
        results$n_L = rep(n_labeled,length(index_int))
      }
    }
  }
}


require(dplyr)
results = na.omit(results)
results$error = (results$real - results$result)^2
data_error = results%>% group_by(method,k,mu,b) %>% summarise(EQM = sqrt(mean(error)))


require(ggplot2)
g1 = data_error %>% ggplot(aes(y = EQM, x = method,colour = method)) +geom_boxplot()+
    facet_wrap(k ~ mu, ncol = 4) + theme_minimal()
ggsave(g1,file = "ratio_regression/outputs/plots/regression_error.pdf",width = 10,height = 8,limitsize = FALSE)