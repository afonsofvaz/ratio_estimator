## Dependencies
#install.packages("locfit")
require(locfit)


## Regression function
sinRegFunc = function(x,k = 1){

  return((sin(2*pi*k*x)+1)/2)

}

## Sample generation
sampleGenerate = function(n_train = 1000,n_target = 1000,
                          n_grid = 10000,mu = 1,
                          k = 1,theta_target = 0.5,sigma = 1,
                          n_labeled = 100){

  ##Generate z_target and z_grid
  z_target = runif(n_target,0,1)
  z_grid = seq(0,1,length.out = n_grid)

  ##Generate rz_target and rz_grid
  rz_target = sinRegFunc(z_target,k = k)
  rz_grid   = sinRegFunc(z_grid,k = k)

  ##Generate y_target
  y_target    = rbinom(n_target,1,rz_target)
  id_target_0 = which(y_target == 0)
  n_target_0  = length(id_target_0)
  id_target_1 = which(y_target == 1)
  n_target_1  = length(id_target_1)

  ##Generate x_target
  x_target = vector(length = n_target)
  x_target[id_target_0] = rnorm(n = n_target_0,-mu,sigma)
  x_target[id_target_1] = rnorm(n = n_target_1, mu,sigma)

  ##Generate y_train
  y_train    = rbinom(n_train,1,0.5)
  id_train_0 = which(y_train == 0)
  n_train_0  = length(id_train_0)
  id_train_1 = which(y_train == 1)
  n_train_1  = length(id_train_1)

  ##Generate x_train
  x_train = vector(length = n_train)
  x_train[id_train_0] = rnorm(n = n_train_0,-mu,sigma)
  x_train[id_train_1] = rnorm(n = n_train_1, mu,sigma)

  ## Gererate labeled sample
  id_labeled = sample(1:n_target,n_labeled)
  y_labeled = y_target[id_labeled]
  z_labeled = z_target[id_labeled]

  return(list(y_target = y_target,
              x_target = x_target,
              z_target = z_target,
              rz_target = rz_target,
              z_grid = z_grid,
              rz_grid = rz_grid,
              y_train = y_train,
              x_train = x_train,
              y_labeled = y_labeled,
              z_labeled = z_labeled))
}

## Nadaraya-Watson Estimator

nwEstimator = function(y,x, x_grid = x,bw_grid = quantile(x,probs = seq(0.05,0.95,0.05)),
                       prop_test = 0.3){

  out = list()
  n = length(y)
  id_train = sample(1:n,round(prop_test*n))
  data_reg = cbind(y,x)
  data_reg = as.data.frame(data_reg)
  out$cv_error = vector(length = length(bw_grid))

  for(i in 1:length(bw_grid)){
    nw_model = locfit(formula = y ~ .,data = data_reg[id_train, ],alpha = c(0,bw_grid[i]),deg = 0,maxk = 1000)

    out$cv_error[i] = mean((predict(nw_model,newdata = data_reg[-id_train, ])-data_reg$y[-id_train])^2)
  }

  out$bw_grid = bw_grid
  out$bw_best = bw_grid[which.min(out$cv_error)]
  out$nadaraya_error = sqrt(out$cv_error[which.min(out$cv_error)])
  out$nw_model = locfit(formula = y ~ .,data = data_reg,alpha = c(0,out$bw_best),deg = 0)
  out$adjusted = predict(out$nw_model,newdata = x)
  out$predict = predict(out$nw_model,newdata = x_grid)
  out$x_grid = x_grid

  return(out)
}

## Classify and Count Estimator
quantCCEstimator = function(y_train,gx_train,gx_target,z_target,z_grid){

  return(c(nwEstimator(y = gx_target,x = z_target,x_grid = z_grid)))

}


## Ratio Estimator
quantRatioEstimator = function(y_train,gx_train,rz_grid){

  id_train_0 = which(y_train == 0)
  id_train_1 = which(y_train == 1)

  ratio_regression = (rz_grid - mean(gx_train[id_train_0]))/(mean(gx_train[id_train_1]) - mean(gx_train[id_train_0]))
  return(ratio_regression)
}


## Labeled Estimator
labeledEstimator = function(y_labeled,z_labeled, z_grid = z_labeled){

  return(c(nwEstimator(y = y_labeled,x = z_labeled,x_grid = z_grid)))

}


## Variance estimation via Bootstrap
wEstimation = function(n_resample,sample_reg){

  ratio_matrix = labeled_matrix = matrix(ncol = length(sample_reg$z_grid),nrow = n_resample)

  for(i in 1:n_resample){

    id_resample_ratio_target = sample(1:length(sample_reg$x_target),replace = T)
    id_resample_ratio_train = sample(1:length(sample_reg$x_train),replace = T)
    id_resample_labeled = sample(1:length(sample_reg$y_labeled),replace = T)

    cc_reg = quantCCEstimator(y_train = sample_reg$y_train[id_resample_ratio_train],
                              gx_train = sample_reg$x_train[id_resample_ratio_train] > 0,
                              gx_target = sample_reg$x_target[id_resample_ratio_target] > 0,
                              z_target = sample_reg$z_target[id_resample_ratio_target],
                              z_grid = sample_reg$z_grid)

    ratio_matrix[i, ] = quantRatioEstimator(y_train = sample_reg$y_train[id_resample_ratio_train],
                                            gx_train = sample_reg$x_train[id_resample_ratio_train] > 0,
                                            rz_grid = cc_reg$predict)
    labeled_matrix[i,] = labeledEstimator(y_labeled = sample_reg$y_labeled[id_resample_labeled],
                                          z_labeled = sample_reg$z_labeled[id_resample_labeled],
                                          z_grid = sample_reg$z_grid)$predict
  }

  ratio_var_grid = apply(ratio_matrix,2,FUN = var)
  labeled_var_grid = apply(labeled_matrix,2,FUN = var)
  w = labeled_var_grid/(labeled_var_grid + ratio_var_grid)
  return(list(w = w,
              ratio_var_grid = ratio_var_grid,
              labeled_var_grid = labeled_var_grid,
              ratio_matrix = ratio_matrix,
              labeled_matrix = labeled_matrix))
}


## Variance estimation via Bootstrap
wEstimation2 = function(n_resample,sample_reg){

  ratio_matrix = labeled_matrix = matrix(ncol = length(sample_reg$z_grid),nrow = n_resample)

  for(i in 1:n_resample){

    id_resample_ratio_target = sample(1:length(sample_reg$x_target),replace = T)
    id_resample_ratio_train = sample(1:length(sample_reg$x_train),replace = T)
    id_resample_labeled = sample(1:length(sample_reg$y_labeled),replace = T)

    cc_reg = quantCCEstimator(y_train = sample_reg$y_train[id_resample_ratio_train],
                              gx_train = sample_reg$x_train[id_resample_ratio_train] > 0,
                              gx_target = sample_reg$x_target[id_resample_ratio_target] > 0,
                              z_target = sample_reg$z_target[id_resample_ratio_target],
                              z_grid = sample_reg$z_grid)

    ratio_matrix[i, ] = quantRatioEstimator(y_train = sample_reg$y_train[id_resample_ratio_train],
                                            gx_train = sample_reg$x_train[id_resample_ratio_train] > 0,
                                            rz_grid = cc_reg$predict)
    labeled_matrix[i,] = labeledEstimator(y_labeled = sample_reg$y_labeled[id_resample_labeled],
                                          z_labeled = sample_reg$z_labeled[id_resample_labeled],
                                          z_grid = sample_reg$z_grid)$predict
  }



  ratio_var_grid = apply(ratio_matrix,2,FUN = var)
  labeled_var_grid = apply(labeled_matrix,2,FUN = var)

  ratio_mean_grid = apply(ratio_matrix,2,FUN = mean)
  labeled_mean_grid = apply(labeled_matrix,2,FUN = mean)


  cc_reg = quantCCEstimator(y_train = sample_reg$y_train,
                            gx_train = sample_reg$x_train > 0,
                            gx_target = sample_reg$x_target > 0,
                            z_target = sample_reg$z_target,
                            z_grid = sample_reg$z_grid)

  ratio_reg = quantRatioEstimator(y_train = sample_reg$y_train,
                                          gx_train = sample_reg$x_train > 0,
                                          rz_grid = cc_reg$predict)

  labeled_reg = labeledEstimator(y_labeled = sample_reg$y_labeled,
                                        z_labeled = sample_reg$z_labeled,
                                        z_grid = sample_reg$z_grid)$predict

  labeled_bias_grid = (labeled_reg - labeled_mean_grid)^2

  ratio_bias_grid = (ratio_reg - ratio_mean_grid)^2

  w = (labeled_var_grid+labeled_bias_grid)/((labeled_var_grid+labeled_bias_grid)
                                          +  (ratio_bias_grid+ratio_var_grid))

  return(list(w = w,
              ratio_var_grid = ratio_var_grid,
              labeled_var_grid = labeled_var_grid,
              ratio_matrix = ratio_matrix,
              labeled_matrix = labeled_matrix))
}
