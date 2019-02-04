library(olpsR)

estimate_prevalence <- function(x_labeled,y_labeled,x_unlabeled,g)
{
  # g is a named list of functions; the names correspond to the 
  # classes that are predicted by each function (e.g., list(a=g1,b=g2))
  # notice that one of the classes does not have function associate to it
  
  # the function returns the estimates in the same order as they 
  # appear in the list; the last estimate corresponds to the class
  # that does not have a function associated to it
  name_extra_class <- setdiff(unique(y_labeled),names(g))
  g_hat <- matrix(NA,length(g),1)
  G <- matrix(NA,length(g),length(g)+1)
  for(ii in 1:length(g))
  {
    g_hat[ii] <- mean(g[[ii]](x_unlabeled))
    for(jj in 1:(length(g)+1))
    {
      if(jj==(length(g)+1))
      {
        G[ii,jj]  <- mean(g[[ii]](x_labeled[y_labeled==name_extra_class,]))    
      } else {
        G[ii,jj]  <- mean(g[[ii]](x_labeled[y_labeled==names(g)[[jj]],]))
      }
    }
  }
  G <- rbind(G,1)
  g_hat <- c(g_hat,1)
  theta_hat <- solve(G,g_hat)
  theta_hat_projected <- projsplx_2(theta_hat)
  return(list(projected=theta_hat_projected,
              not_projected=theta_hat))
}