###Simulation
source("ratio_regression/funcs.R")

## mu in {0.5,1,1.5,2}
## n_labeled in {100,500}
## B = 200

for(B in 1:200){

  print(B)

  for(mu in c(0.5,1,1.5,2)){

    for(n_labeled in c(200,500)){

      for(k in c(1,2)){


      sample_reg = sampleGenerate(mu = mu,n_labeled = n_labeled,k = k)

      cc_reg = quantCCEstimator(y_train = sample_reg$y_train,gx_train = sample_reg$x_train > 0,
                              gx_target = sample_reg$x_target > 0,
                              z_target = sample_reg$z_target,z_grid = sample_reg$z_grid)

      ratio_reg = quantRatioEstimator(y_train = sample_reg$y_train,gx_train = sample_reg$x_train > 0,
                                    rz_grid = cc_reg$predict)

      labeled_reg = try(labeledEstimator(y_labeled = sample_reg$y_labeled,
                                         z_labeled = sample_reg$z_labeled,
                                         z_grid = sample_reg$z_grid),silent = T)

      if(identical(class(labeled_reg),"try-error"))
        next;

      w1 = try(wEstimation(n_resample = 100,sample_reg = sample_reg)$w,
               silent = T)

      w2 = try(wEstimation2(n_resample = 100,sample_reg = sample_reg)$w,
               silent = T)

      if(identical(class(w1),"try-error") |
         identical(class(w2),"try-error"))
        next;

      save.image(paste0("ratio_regression/outputs/results/",mu,"_",k,"_",n_labeled,"_",B,".RData"))

      }
    }
  }
}


