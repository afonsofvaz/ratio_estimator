##Dependencias
#install.packages("locfit")
require(locfit)

sinRegFunc = function(x,k = 1){

  return((sin(2*pi*k*x)+1)/2)

}

nwEstimator = function(y,x, x_grid = x,bw_grid = quantile(x,probs = seq(0.05,0.95,0.05)),
                       prop_test = 0.3){

  out = list()
  n = length(y)
  id_train = sample(1:n,round(prop_test*n))
  data_reg = cbind(y,x)
  data_reg = as.data.frame(data_reg)
  out$cv_error = vector(length = length(bw_grid))

  for(i in 1:length(bw_grid)){
    nw_model = locfit(formula = y ~ .,data = data_reg[id_train, ],alpha = c(0,bw_grid[i]),deg = 0)

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

## teste
n = 30
x_grid = seq(0,1,length.out = 1000)
x = runif(n)
y =  sinRegFunc(x)
# #bw_grid = quantile((x-mean(x))^2,probs = seq(0.1,0.9,0.05))
teste = nwEstimator(x = x,y = y,x_grid = x_grid)
plot(teste$bw_grid,teste$cv_error)
plot(x,y)
lines(sort(teste$x_grid),teste$predict[order(teste$x_grid)])

sampleGenerate = function(n_train = 1000,n_target = 1000,
                          n_grid = 10000,mu = 1,
                          k = 1,theta_target = 0.5,sigma = 1){

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
  x_train[id_train_0] = rnorm(n = n_train_0,mu_0,sigma)
  x_train[id_train_1] = rnorm(n = n_train_1,mu_1,sigma)

  return(list(y_target = y_target,
              x_target = x_target,
              z_target = z_target,
              rz_target = rz_target,
              z_grid = z_grid,
              rz_grid = rz_grid,
              y_train = y_train,
              x_train = x_train))
}

##Teste
# args(sampleGenerate)
# n_train = 100
# n_target = 100
# n_grid = 1000
# mu_0 = -1
# mu_1 =  1
# k = 1
# theta_target = 0.5
# sigma = 1
teste = sampleGenerate(mu_0 = -2, mu_1 = 2)
mean(teste$y_target)
length(teste$y_target)
plot(teste$z_target,teste$y_target)
lines(sort(teste$z_target),teste$rz_target[order(teste$z_target)])
lines(sort(teste$z_grid),teste$rz_grid[order(teste$z_grid)])

quantCCEstimator = function(y_train,gx_train,gx_target,z_target,z_grid){

  return(c(nwEstimator(y = gx_target,x = z_target,x_grid = z_grid)))

}

testeCC = quantCCEstimator(y_train   = teste$y_train,
                           gx_train  = teste$x_train > 0,
                           gx_target = teste$x_target > 0,
                           z_target  = teste$z_target,
                           z_grid = teste$z_grid)

errorCC = sqrt(mean((testeCC$predict - teste$rz_grid)^2))


plot(testeCC$bw_grid,testeCC$cv_error)
plot(teste$z_target,teste$x_target > 0)
lines(sort(testeCC$x_grid),testeCC$predict[order(testeCC$x_grid)])
lines(teste$z_grid,teste$rz_grid)

testeBella = quantCCEstimator(y_train   = teste$y_train,
                              gx_train  = teste$x_train,
                              gx_target = teste$x_target,
                              z_target  = teste$z_target,
                              z_grid = teste$z_grid)


# errorBella = sqrt(mean((testeBella$predict - teste$rz_grid)^2))
#
#
# plot(testeCC$bw_grid,testeBella$cv_error)
# plot(teste$z_target,teste$x_target)
# lines(sort(testeBella$x_grid),testeBella$predict[order(testeBella$x_grid)])
# lines(teste$z_grid,teste$rz_grid)


quantRatioEstimator = function(y_train,gx_train,rz_grid){

  id_train_0 = which(y_train == 0)
  id_train_1 = which(y_train == 1)

  ratio_regression = (rz_grid - mean(gx_train[id_train_0]))/(mean(gx_train[id_train_1]) - mean(gx_train[id_train_0]))
  return(ratio_regression)
}

testeRatioCC = quantRatioRegEstimator(y_train = teste$y_train,gx_train = teste$x_train > 0, rz_grid =  testeCC$predict )
testeRatioBella = quantRatioRegEstimator(y_train = teste$y_train,gx_train = teste$x_train, rz_grid =  testeCC$predict)
errorRatioCC = sqrt(mean((testeRatioCC - teste$rz_grid)^2))
errorRatioBella = sqrt(mean((testeRatioBella - teste$rz_grid)^2))


plot(teste$z_grid,teste$rz_grid)
lines(teste$z_grid,testeRatioCC,col = "red")
lines(teste$z_grid,testeRatioBella,col = "green")
lines(teste$z_grid,testeCC$predict,col = "yellow")

errorCC
errorRatioCC
errorRatioBella

quantRatioRegEstimatior = function(y_train,gx_train,gx_target,z_target,y_labeled,z_labeled){
  out = list()
  n_target = length(z_target)
  id_train_reg = sample(1:n_target,0.7*n_target)
  #grid_bw = quantile(gx_target,probs = seq(0.05,0.95,0.05))
  grid_bw = seq(0.05,0.95,0.001)
  error = vector(length = length(grid_bw))

  for(i in 1:length(grid_bw)){
    reg_gx_z = locfit(formula = gx_target[id_train_reg] ~ z_target[id_train_reg],
                      alpha=c(0,grid_bw[i]),deg = 0)

    error[i] = mean((predict(reg_gx_z,newdata = z_target[-id_train_reg])-gx_target[-id_train_reg])^2)
  }

  id_best_bw = which.min(error)
  out$nadaraya_error = sqrt(error[id_best_bw])
  reg_gx_z = locfit(formula = gx_target ~ z_target,alpha=c(0,grid_bw[id_best_bw]),deg = 0)
  ajust_reg_gx_z = fitted(reg_gx_z)
  id_train_0 = which(y_train == 0)
  id_train_1 = which(y_train == 1)

  out$ratio_regression = (ajust_reg_gx_z - mean(gx_train[id_train_0]))/(mean(gx_train[id_train_1]) - mean(gx_train[id_train_0]))
  out$cc_regression = ajust_reg_gx_z
  out$classifier_error = mean(gx_train != y_train)

  return(out)

}











teste = quantRatioRegEstimatior(y_train = y_train,gx_train = x_train > 0 ,gx_target = x_target > 0,
                        z_target = z_target,labeled = NULL)

z_grid = seq(0,1,length.out = 100)
reg_grid = sinRegFunc(z_grid,5)
plot(z_grid,reg_grid)
lines(sort(z_target),teste$ratio_regression[order(z_target)],col = 2)
lines(sort(z_target),teste$cc_regression[order(z_target)],col = 3)
error_ratio = sqrt(mean((sinRegFunc(z_target,5) - teste$ratio_regression)^2))
error_cc = sqrt(mean((sinRegFunc(z_target,5) - teste$cc_regression)^2))
error_ratio
error_cc
teste$classifier_error
teste$nadaraya_error










