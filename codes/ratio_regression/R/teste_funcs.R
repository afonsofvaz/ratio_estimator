source("R/funcs.R")

args(sampleGenerate)



sample_reg = sampleGenerate(mu = 0.5,n_labeled = 500)

plot(sample_reg$z_grid,sample_reg$rz_grid)
plot(sample_reg$z_target,sample_reg$rz_target)
points(sample_reg$z_target,sample_reg$y_target)
hist(sample_reg$x_target)
hist(sample_reg$x_train)

args(quantCCEstimator)

cc_reg = quantCCEstimator(y_train = sample_reg$y_train,gx_train = sample_reg$x_train > 0,
                          gx_target = sample_reg$x_target > 0,
                          z_target = sample_reg$z_target,z_grid = sample_reg$z_grid)

plot(sample_reg$z_grid,sample_reg$rz_grid)
lines(cc_reg$x_grid,cc_reg$predict,col = "red")

ratio_reg = quantRatioEstimator(y_train = sample_reg$y_train,gx_train = sample_reg$x_train > 0,
                                rz_grid = cc_reg$predict)

lines(cc_reg$x_grid,ratio_reg)

labeled_reg = labeledEstimator(y_labeled = sample_reg$y_labeled,
                               z_labeled = sample_reg$z_labeled,
                               z_grid = sample_reg$z_grid)
plot(sample_reg$z_grid,labeled_reg$predict)
lines(sample_reg$z_grid,sample_reg$rz_grid)
plot(labeled_reg$bw_grid,labeled_reg$cv_error)

require(dplyr)
require(ggplot2)



data_cc = data.frame(Method = rep("CC",length(sample_reg$z_grid)),
                     z = sample_reg$z_grid,
                     rz = cc_reg$predict,
                     Real = sample_reg$rz_grid)

data_ratio = data.frame(Method = rep("Ratio",length(sample_reg$z_grid)),
                        z = sample_reg$z_grid,
                        rz = ratio_reg,
                        Real = sample_reg$rz_grid)

data_labeled = data.frame(Method = rep("Labeled",length(sample_reg$z_grid)),
                          z = sample_reg$z_grid,
                          rz = labeled_reg$predict,
                          Real = sample_reg$rz_grid)

data_real = data.frame(Method = rep("Real",length(sample_reg$z_grid)),
                       z = sample_reg$z_grid,
                       rz = sample_reg$rz_grid,
                       Real = sample_reg$rz_grid)

w = wEstimation2(n_resample = 100,sample_reg = sample_reg)$w

plot(sample_reg$z_grid,w)

data_comb = data.frame(Method = rep("Combined",length(sample_reg$z_grid)),
                       z = sample_reg$z_grid,
                       rz = w*data_ratio$rz + (1-w)*data_labeled$rz,
                       Real = sample_reg$rz_grid)

data_reg = rbind(data_real,data_cc,data_labeled,data_ratio,data_comb)

data_reg %>% filter(Method %in% c("Real","Ratio","Labeled","Combined"))  %>%
  ggplot(aes(x = z,y = rz,col = Method,linetype = Method)) + geom_path(size = 2) + theme_minimal()

data_reg %>% filter(Method %in% c("Real","Ratio","Labeled","Combined"),0.25 < z  & z < 0.75) %>%
  group_by(Method) %>% summarise(EQM = mean((Real - rz)^2))



head(data_reg)

