library(tidyverse)
source("ratio/quantification_multiclass.R")

prob <- c(0.25,0.1,0.65)

mu_mua <- rep(0,10)
mu_mub <- rep(0.75,10)
mu_muc <- rep(1.25,10)
Sigma <- diag(1,10)
B <- 5000
n_grid <- round(exp(seq(log(100),log(2000),length.out = 50)))
risk_projected=risk_not_projected=matrix(NA,length(n_grid),B)
for(ii in 1:B)
{
  print(ii/B)
  for(jj in seq_along(n_grid))
  {
    x_labeled <- matrix(NA,n_grid[jj],10)
    x_unlabeled <- matrix(NA,n_grid[jj],10)
    
    y_labeled <- runif(n_grid[jj])
    y_labeled <- ifelse(y_labeled>0.8,"a",ifelse(y_labeled<0.3,"b","c"))
    x_labeled[y_labeled=="a",] <- MASS::mvrnorm(n=sum(y_labeled=="a"),mu_mua,
                                                Sigma)
    x_labeled[y_labeled=="b",] <- MASS::mvrnorm(n=sum(y_labeled=="b"),mu_mub,
                                                Sigma)
    x_labeled[y_labeled=="c",] <- MASS::mvrnorm(n=sum(y_labeled=="c"),mu_muc,
                                                Sigma)
    
    y_unlabeled <- runif(n_grid[jj])
    y_unlabeled <- ifelse(y_unlabeled>(1-prob[1]),"a",ifelse(y_unlabeled<prob[2],"b","c"))
    x_unlabeled[y_unlabeled=="a",] <- MASS::mvrnorm(n=sum(y_unlabeled=="a"),mu_mua,
                                                    Sigma)
    x_unlabeled[y_unlabeled=="b",] <- MASS::mvrnorm(n=sum(y_unlabeled=="b"),mu_mub,
                                                    Sigma)
    x_unlabeled[y_unlabeled=="c",] <- MASS::mvrnorm(n=sum(y_unlabeled=="c"),mu_muc,
                                                    Sigma)
    
    fit <- glmnet::cv.glmnet(x = x_labeled,y=y_labeled,family="multinomial")
    ga <- function(x)
    {
      pred <- predict(fit,x,type="response")  
      return(pred[,1,1]>mean(y_labeled=="a"))  
    }
    gb <- function(x)
    {
      pred <- predict(fit,x,type="response")  
      return(pred[,2,1]>mean(y_labeled=="b"))  
    }
    g <- list(a=ga,b=gb)
    y_labeled <- runif(n_grid[jj])
    y_labeled <- ifelse(y_labeled>0.8,"a",ifelse(y_labeled<0.3,"b","c"))
    x_labeled[y_labeled=="a",] <- MASS::mvrnorm(n=sum(y_labeled=="a"),mu_mua,
                                                Sigma)
    x_labeled[y_labeled=="b",] <- MASS::mvrnorm(n=sum(y_labeled=="b"),mu_mub,
                                                Sigma)
    x_labeled[y_labeled=="c",] <- MASS::mvrnorm(n=sum(y_labeled=="c"),mu_muc,
                                                Sigma)
    
    estimates <- try(estimate_prevalence(x_labeled,y_labeled,
                        x_unlabeled,g),silent = TRUE)
    if(class(estimates)=="try-error")
      next;
    risk_projected[jj,ii] <- sum((estimates$projected-prob)^2)
    risk_not_projected[jj,ii] <- sum((estimates$not_projected-prob)^2)
    
  }
}

saveRDS(risk_not_projected,"ratio/outputs/results/risk_not_projected.RDS")
saveRDS(risk_projected,"ratio/outputs/results/risk_projected.RDS")


plot(n_grid,rowMeans(risk_not_projected),col=2)
points(n_grid,rowMeans(risk_projected))
abline(h=0)

data_plot <- data.frame(n=c(n_grid,n_grid),
                        risk=c(rowMeans(risk_not_projected,na.rm=TRUE),rowMeans(risk_projected,na.rm=TRUE)),
                        estimator=rep(c("Not Projected","Projected"),each=nrow(risk_projected)))

ggplot(data_plot)+
  geom_line(aes(x=n,y=risk,color=estimator),size=2)+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal(base_size = 12)+
  ylab("Risk")


ggplot(data_plot)+
  #geom_line(aes(x=n,y=risk,color=estimator),size=2)+
  geom_smooth(aes(x=n,y=risk,color=estimator),se=FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal(base_size = 12)+
  ylab("Risk (log scale)")+
  xlab("Sample size (log scale)")+
  scale_color_discrete(name="Ratio Estimator")
ggsave(filename = "ratio/outputs/plots/multiclass.png")

  
