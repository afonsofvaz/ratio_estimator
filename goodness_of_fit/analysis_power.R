library(ggplot2)
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top")

results <- readRDS("goodness_of_fit/outputs/results/powerMC_gaussian.RDS")
mean=apply(results$p,1,function(x)mean(x<0.05,na.rm=T))
se=sqrt(apply(results$p,1,function(x){
  mean(x<0.05,na.rm=T)*mean(x>=0.05,na.rm=T)/sum(!is.na(x))
}))
data=data.frame(model="Gaussian",mu.0=results$mu[1]+results$shift.grid,power=mean,se=se)

results <- readRDS("goodness_of_fit/outputs/results/powerMC_exponential.RDS")
mean=apply(results$p,1,function(x)mean(x<0.05,na.rm=T))
se=sqrt(apply(results$p,1,function(x){
  mean(x<0.05,na.rm=T)*mean(x>=0.05,na.rm=T)/sum(!is.na(x))
}))
data=rbind(data,
           data.frame(model="Exponential",mu.0=results$mu[1]+results$shift.grid,power=mean,se=se))

results <- readRDS("goodness_of_fit/outputs/results/powerMC_normal_exponential.RDS")
mean=apply(results$p,1,function(x)mean(x<0.05,na.rm=T))
se=sqrt(apply(results$p,1,function(x){
  mean(x<0.05,na.rm=T)*mean(x>=0.05,na.rm=T)/sum(!is.na(x))
}))
data=rbind(data,
           data.frame(model="Gaussian_exponential",mu.0=results$mu[1]+results$shift.grid,power=mean,se=se))


results <- readRDS("goodness_of_fit/outputs/results/powerMC_beta.RDS")
mean=apply(results$p,1,function(x)mean(x<0.05,na.rm=T))
se=sqrt(apply(results$p,1,function(x){
  mean(x<0.05,na.rm=T)*mean(x>=0.05,na.rm=T)/sum(!is.na(x))
}))
data=rbind(data,
           data.frame(model="Beta",mu.0=results$mu[1]+results$shift.grid,power=mean,se=se))


library(tidyverse)

range <- data %>% group_by(model) %>% summarise(range=max(mu.0)-min(mu.0))
data <- data %>% left_join(range)

data$true_theta=1  
data$true_theta[data$model=="Gaussian"]=0
eps <- 0.01
pdf("goodness_of_fit/outputs/plots/power_all.pdf",width = 10,height = 7)
ggplot(data,aes(x=mu.0,y=power))+geom_point()+
  geom_line(aes(x=mu.0,y=power))+xlab(expression(gamma))+
  ylab("Power")+ geom_hline(yintercept = 0.05) + 
  geom_vline(aes(xintercept = true_theta),linetype=2) + 
  geom_errorbar(aes(ymin=power-2*se, ymax=power+2*se,
                    width=eps*range), colour="black")+
  facet_wrap( ~ model, ncol=2,scales="free_x")
dev.off()
