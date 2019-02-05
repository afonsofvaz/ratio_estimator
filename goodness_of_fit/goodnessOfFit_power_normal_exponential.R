#' Computes test statistic for testing prior shift
#'
#' @param sample 
#' @param p.tg 
#' @param xgrid 
#'
#' @return
#' @export
#'
#' @examples
test=function(sample,xgrid=NULL)
{
  if(is.null(xgrid))
    xgrid=seq(min(sapply(sample,min)),
              max(sapply(sample,max)),
              length.out = 2000)
  
  F0tr=ecdf(sample$sample.tr.0)
  F1tr=ecdf(sample$sample.tr.1)
  Ftg=ecdf(sample$sample.tg)
  Ftg=Ftg(xgrid)
  
  F1tr_grid=F1tr(xgrid)
  F0tr_grid=F0tr(xgrid)
  
  p.tg.grid=seq(0,1,length.out = 1000)
  stat=Inf
  for(i in seq_along(p.tg.grid))
  {
    Ftr=p.tg.grid[i]*F1tr_grid+(1-p.tg.grid[i])*F0tr_grid
    stat.local=max(abs(Ftr-Ftg))
    if(stat.local<stat)
    {
      stat=stat.local
      p.tg=p.tg.grid[i]
    }
  }
  
  
  return(list(stat=stat,p.tg=p.tg))
}


#' Bootstrap for computing pvalue for testing prior shift
#'
#' @param sample 
#' @param B 
#' @param test.stat 
#'
#' @return
#' @export
#'
#' @examples
MC.pvalue <- function(sample,B=1000,
                      test) {
  test.statistics <- test(sample)
  p.tg=test.statistics$p.tg
  test.statistic=test.statistics$stat
  test.MC=p.MC=rep(NA,B)
  density0=density(sample$sample.tr.0)
  density1=density(sample$sample.tr.1)
  for (i in 1:B) {
    MC.sample <- simulator(density0,density1,
                           sample$sample.tr.0,
                           sample$sample.tr.1,
                           p.tg,
                           length(sample$sample.tg))
    #thetaboot <- estimator(boot.sample)
    test.MC[i] <- test(MC.sample)$stat
  }
  p <- (sum(test.MC >= test.statistic)+1)/(B+1)
  return(p)
}

#' Outputs bootstrap sample
#'
#' @param sample.tr 
#' @param sample.tr 
#' @param sample.tg 
#' @param p.tg 
#'
#' @return
#' @export
#'
#' @examples
simulator=function(density0,density1,sample.tr.0,
                   sample.tr.1,
                   p.tg,ntg)
{
  ntr.0=length(sample.tr.0)
  ntr.1=length(sample.tr.1)
  
  bw.tr.0 <- density0$bw
  means <- sample(sample.tr.0, ntg*(1-p.tg), replace = TRUE)
  sample.tg.0=rnorm(ntg*(1-p.tg), mean = means, sd = bw.tr.0)
  means <- sample(sample.tr.0, ntr.0, replace = TRUE)
  sample.tr.0=rnorm(ntr.0, mean = means, sd = bw.tr.0)
  
  
  bw.tr.1 <- density1$bw
  means <- sample(sample.tr.1, ntg*(p.tg), replace = TRUE)
  sample.tg.1=rnorm(ntg*(p.tg), mean = means, sd = bw.tr.1)
  means <- sample(sample.tr.1, ntr.1, replace = TRUE)
  sample.tr.1=rnorm(ntr.1, mean = means, sd = bw.tr.1)
  
  return(list(sample.tr.0=sample.tr.0,
              sample.tr.1=sample.tr.1,
              sample.tg=c(sample.tg.0,sample.tg.1)))
}


set.seed(400)
ptr=0.6
ntr=200
p.tg=0.2
ntg=200
mu=c(1,1)
shift.grid=seq(-1,1,length.out = 40)
ntot=ntr+ntg



B=500
stat=p=p.adj=matrix(NA,length(shift.grid),B)
for(b in 1:B)
{
  print(b/B)
  
  for(s in seq_along(shift.grid))
  {
    mu.shift=mu
    mu.shift[1]=mu[1]+shift.grid[s]
    
    # tr:
    ytr=runif(ntr)<p.tg
    xtr=rep(NA,length(ytr))
    xtr[ytr==1]=rexp(sum(ytr==1),1/mu[2])
    xtr[ytr==0]=rnorm(sum(ytr==0),mu[1])
    #hist(xtr)
    
    
    # tg:
    ytg=runif(ntg)<p.tg
    xtg=rep(NA,length(ytg))
    xtg[ytg==1]=rexp(sum(ytg==1),1/mu[2])
    xtg[ytg==0]=rnorm(sum(ytg==0),mu.shift[1])
    #hist(xtg)
    
    sample=list(sample.tr.0=xtr[ytr==0],
                sample.tr.1=xtr[ytr==1],
                sample.tg=xtg)
    
    result= MC.pvalue(sample,B=1000,test)
    
    
    p[s,b]=result
    saveRDS(list(p=p,mu=mu,shift.grid=shift.grid),"goodness_of_fit/outputs/results/powerMC_normal_exponential.RDS")
  }
  
}
