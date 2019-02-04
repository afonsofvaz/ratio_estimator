# install.packages(plyr)
# install.packages(dplyr)
# install.packages(ggplot2)
# install.packages(xtable)
require(plyr)
require(dplyr)
require(ggplot2)
require(xtable)

## Auxiliar functions
mean_2se=function (x, mult = 1)
{
  x <- stats::na.omit(x)
  se <- mult * sqrt(stats::var(x)/length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - 2*se, ymax = mean + 2*se)
}

mean_0se=function (x, mult = 1)
{
  x <- stats::na.omit(x)
  se <-rep(0,length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - 2*se, ymax = mean + 2*se)
}


##Loading datasets

## cancer
load("codes/ratio/outputs/results/out.spam.ratio.RData")
load("codes/ratio/outputs/results/out.spam.comb.new.RData")
load("codes/ratio/outputs/results/out.spam.em.RData")
out.spam.ratio$Dataset = rep("spam",nrow(out.spam.ratio))
out.spam.comb.new$Dataset = rep("spam",nrow(out.spam.comb.new))
out.spam.em$Dataset = rep("spam",nrow(out.spam.em))

## block
load("codes/ratio/outputs/results/out.block.ratio.RData")
load("codes/ratio/outputs/results/out.block.comb.new.RData")
load("codes/ratio/outputs/results/out.block.em.RData")
out.block.ratio$Dataset = rep("block",nrow(out.block.ratio))
out.block.comb.new$Dataset = rep("block",nrow(out.block.comb.new))
out.block.em$Dataset = rep("block",nrow(out.block.em))

## bank
load("codes/ratio/outputs/results/out.bank.ratio.RData")
load("codes/ratio/outputs/results/out.bank.comb.new.RData")
load("codes/ratio/outputs/results/out.bank.em.RData")
out.bank.ratio$Dataset = rep("bank",nrow(out.bank.ratio))
out.bank.comb.new$Dataset = rep("bank",nrow(out.bank.comb.new))
out.bank.em$Dataset = rep("bank",nrow(out.bank.em))

## spam
load("codes/ratio/outputs/results/out.spam.ratio.RData")
load("codes/ratio/outputs/results/out.spam.comb.new.RData")
load("codes/ratio/outputs/results/out.spam.em.RData")
out.spam.ratio$Dataset = rep("spam",nrow(out.spam.ratio))
out.spam.comb.new$Dataset = rep("spam",nrow(out.spam.comb.new))
out.spam.em$Dataset = rep("spam",nrow(out.spam.em))


dataratio = rbind(out.spam.ratio,out.cancer.ratio,out.candles.ratio,out.block.ratio,out.bank.ratio,
                  out.cancer.em,out.candles.em,out.bank.em,out.block.em,out.spam.em)

datacomb = rbind(out.spam.comb.new,out.cancer.comb.new,out.candles.comb.new,out.block.comb.new,out.bank.comb.new)

## Generate table of experiment information
dataratio$n = dataratio$n0 + dataratio$n1
n.inf = aggregate(dataratio$n,by = list(dataratio$Dataset),FUN = mean) 
colnames(n.inf) = c("dataset","n")
n.inf = n.inf %>% dplyr::arrange(desc(-n))
xtable(n.inf,digits = 0,caption = "Experiments information",label = "fig:expinf")
names.ordered = n.inf$dataset

## Ratio analysis

## Taking the estimatives via classify and count method
cc_temp  = dataratio %>% filter(method %in% c("forman.knn","forman.logistic","forman.random"))
cc_temp$estimative = cc_temp$mu_g
cc_temp$method = gsub("forman.","",cc_temp$method)
cc_temp$method = paste0("cc.",cc_temp$method)
dataratio = rbind(dataratio,cc_temp)

## Ordering levels by dataset sizes
dataratio$Dataset = factor(dataratio$Dataset,levels = names.ordered)

## Calculating error values
dataratio$error = sqrt((dataratio$estimative - dataratio$theta_tg)^2) 

## Organize methods labels
dataratio$method[dataratio$method == "kernel"] = "kernel.gaussian"
dataratio$method = as.factor(as.character(dataratio$method))
levels.methods = c("bella.knn","bella.logistic","bella.random","forman.knn","forman.logistic","forman.random",
                   "cc.knn","cc.logistic","cc.random","em.knn",         
                   "em.logistic","em.random","kernel.gaussian","kernel.linear")
dataratio$method = factor(dataratio$method,levels = levels.methods)
dataratio$method = revalue(dataratio$method,c("bella.knn" = "Bella-knn","forman.knn" = "Forman-knn",
                                              "cc.knn" = "CC-knn",
                                              "bella.logistic" = "Bella-LR","forman.logistic" = "Forman-LR",
                                              "cc.logistic" = "CC-LR",
                                              "em.knn" = "EM-knn",         
                                              "em.logistic" = "EM-LR",
                                              "em.random" = "EM-RF",
                                              "bella.random" = "Bella-RF","forman.random" = "Forman-RF",
                                              "cc.random" = "CC-RF",
                                              "kernel.gaussian" = "RKHS-Gauss", "kernel.linear" = "RKHS-Linear"))


## Creating auxliar variabel to plot dashed lines
dataratio$aux1 = rep(NA,nrow(dataratio))
dataratio$aux1[dataratio$method %in% c("Bella-knn","Bella-LR","Bella-RF")] = 1
dataratio$aux1[dataratio$method %in% c("Forman-knn","Forman-LR","Forman-RF")] = 2
dataratio$aux1[dataratio$method %in% c("CC-knn","CC-LR","CC-RF")] = 3
dataratio$aux1[dataratio$method %in% c("EM-knn","EM-LR","EM-RF")] = 4
dataratio$aux1[dataratio$method %in% c("RKHS-Gauss","RKHS-Linear")] = 4



## ploting error confidence interval by method and dataset
g1 = dataratio %>% 
  ggplot(aes(x = method,y = error)) +  coord_trans(y = "log10") + facet_grid(Dataset ~ theta_tg) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = -90),axis.title.y = element_text(angle = -90)) + 
  stat_summary(fun.data = "mean_2se", colour = "blue", size = 0.7) +
  stat_summary(fun.data = "mean_0se", colour = "red", size = 0.5) + 
  labs(x = "Method",y = "Error") + geom_segment(aes(x = 3.5, y = 0.009, xend = 3.5, yend = 0.2),linetype='dashed') +
  geom_segment(aes(x = 6.5, y = 0.009, xend = 6.5, yend = 0.2),linetype='dashed') + 
  geom_segment(aes(x = 9.5, y = 0.009, xend = 9.5, yend = 0.2),linetype='dashed') +
  geom_segment(aes(x = 12.5, y = 0.009, xend = 12.5, yend = 0.2),linetype='dashed')

ggsave(g1,file = "codes/ratio/outputs/plots/error_ratio_all.png",width = 12,height = 8,limitsize = FALSE)

## ploting error confidence interval by method and dataset (without dataset)
dataratio_temp = dataratio[!(dataratio$method %in% c("Bella-knn","Forman-knn","CC-knn","EM-knn")), ]

g1 = dataratio_temp %>% 
  ggplot(aes(x = method,y = error)) +  coord_trans(y = "log10") + facet_grid(Dataset ~ theta_tg) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90)) +
  stat_summary(fun.data = "mean_2se", colour = "blue", size = 0.7) +
  stat_summary(fun.data = "mean_0se", colour = "red", size = 0.5) + 
  labs(x = "Method",y = "Error") + geom_segment(aes(x = 2.5, y = 0.009, xend = 2.5, yend = 0.2),linetype='dashed') +
  geom_segment(aes(x = 4.5, y = 0.009, xend = 4.5, yend = 0.2),linetype='dashed') + 
  geom_segment(aes(x = 6.5, y = 0.009, xend = 6.5, yend = 0.2),linetype='dashed') +
  geom_segment(aes(x = 8.5, y = 0.009, xend = 8.5, yend = 0.2),linetype='dashed')


ggsave(g1,file = "codes/ratio/outputs/plots/error_ratio.png",width = 12,height = 8,limitsize = FALSE)

## Table
## install.packages(tidyr)
require(tidyr)

tab = data.frame(dataratio %>% dplyr::group_by(Dataset,theta_tg,method) %>% dplyr::summarise(Mean = mean(error)))
tab = tab %>%  tidyr::spread(key = "method",value = "Mean")

temp1 = tab[ ,-c(1,2)]
temp2 = tab[ , c(1,2)]

## recording position of the smaller values
index = vector(length = nrow(tab_print))

for(i in 1:nrow(temp1)){
  index[i] = which.min(temp1[i, ])
}

for(i in 1:nrow(temp1)){
  temp1[i, index[i]] = 1
  temp1[i,-index[i]] = 0
}

tab_min = cbind(temp2,temp1)

tab_min = tab_min %>% gather("Method","Ind","Bella-knn":"RKHS-Linear")
tab_min$aux2 = rep(NA,nrow(por_metodo))
tab_min$aux2[tab_min$Method %in% c("Bella-knn","Bella-LR","Bella-RF")] = "Bella"
tab_min$aux2[tab_min$Method %in% c("Forman-knn","Forman-LR","Forman-RF")] = "Forman"
tab_min$aux2[tab_min$Method %in% c("CC-knn","CC-LR","CC-RF")] = "CC"
tab_min$aux2[tab_min$Method %in% c("EM-knn","EM-LR","EM-RF")] = "EM"
tab_min$aux2[tab_min$Method %in% c("RKHS-Gauss","RKHS-Linear")] = "RKHS"

g1 = tab_min %>% group_by(theta_tg,teste) %>% summarise(freq = sum(Ind)) %>% 
  ggplot(aes(x = aux2, y = freq)) + geom_bar(stat="identity") + facet_grid(. ~ theta_tg) + theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90)) +   ylab("Frequency") + xlab("Method type")
ggsave(g1,file = "codes/ratio/outputs/plots/counting_theta.pdf",width = 10,height = 8,limitsize = FALSE)


g2 = por_metodo %>% group_by(Dataset,teste) %>% summarise(freq = sum(Ind)) %>% 
  ggplot(aes(x = teste, y = freq)) + geom_bar(stat="identity") + facet_grid(. ~ Dataset) + theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90))  + xlab("Method type")  +   ylab("Frequency")
ggsave(g2,file = "codes/ratio/outputs/plots/counting_dataset.pdf",width = 10,height = 8,limitsize = FALSE)


tab_bar = tab_min %>% gather("Method","Ind","Bella-knn":"RKHS-Linear") %>% group_by(Method) %>% summarise(freq = sum(Ind)) %>% 
  arrange(desc(freq)) 
tab_bar$method = factor(tab_bar$Method,levels = tab_bar$method)

g3 = tab_bar %>% ggplot(aes(x = Method, y = Counting)) + geom_bar(stat="identity") + theme_bw(base_size = 14) +
  theme(axis.text = element_text(angle = 90,size = 16),axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=18)) +
  ylab("Frequency")
ggsave(g3,file = "codes/ratio/outputs/plots/couting.pdf",width = 10,height = 8,limitsize = FALSE)


## Confidence interval coverage
ntr = 300
thetatr = 0.5

##Variance ratio estimator
temp1 = 1/((dataratio$mu_g1 - dataratio$mu_g0)^2)
temp2 = ((1/temp1)*dataratio$estimative*(1-dataratio$estimative) + dataratio$var_g1*dataratio$estimative + dataratio$var_g0*(1-dataratio$estimative))/(dataratio$n)
temp3 = (dataratio$var_g0*((1-dataratio$estimative)^2))/(ntr*(1-thetatr)) + (dataratio$var_g1*(dataratio$estimative^2))/(ntr*thetatr)

dataratio$var = temp1*(temp2 + temp3)

dataratio$li = dataratio$estimative - qnorm(0.975)*sqrt(dataratio$var)
dataratio$ls = dataratio$estimative + qnorm(0.975)*sqrt(dataratio$var)
dataratio$ind_int = dataratio$theta_tg[1] >= dataratio$li[1] & dataratio$theta_tg <= dataratio$ls

coverage = dataratio %>% dplyr::filter(!(method %in% c("CC-LR","CC-knn","CC-RF"))) %>% 
  dplyr::group_by(Dataset,theta_tg) %>% 
  dplyr::summarise(Coverage = mean(ind_int))  
coverage %>% ggplot(aes(x = theta_tg,y = Dataset,fill = Coverage)) + geom_tile()

pdf("codes/ratio/outputs/plots/coverage.pdf")
ggplot(data = coverage, aes(theta_tg, Dataset, fill = Coverage))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green", mid = "yellow", 
                       midpoint = 0.95, limit = c(0.9,1), space = "Lab", 
                       name="Coverage \n") +
  theme_minimal(base_size = 14) + 
  theme(axis.text.x = element_text( vjust = 1, 
                                    size = 12, hjust = 1),
        axis.text.y  = element_text( vjust = 1, 
                                     size = 12, hjust = 1),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text(colour="black", size=12),
        axis.title.x=element_text(size=18)) + 
  xlab(expression(theta)) +
  geom_text(aes(theta_tg,Dataset , label = round(Coverage,2)), color = "black", size = 6) 
dev.off()





## Combined
datacomb$Dataset = factor(dataratio$Dataset,levels = names.ordered)

tempcomb = datacomb %>% select(-Ratio_10,-Labeled_10,
                               -Ratio_20,-Labeled_20,
                               -Ratio_30,-Labeled_30,
                               -Ratio_50,-Labeled_50) %>% 
  unite(col = n_10,w_10,comb_10) %>% 
  unite(col = n_20,w_20,comb_20) %>% 
  unite(col = n_30,w_30,comb_30) %>%
  unite(col = n_50,w_50,comb_50) %>% 
  gather(n_10,n_20,n_30,n_50,key = "labeled_size",value = "Information") %>% 
  separate(col = Information,into = c("Weight","Estimative"),sep = "_",convert = T) %>% 
  mutate(error = sqrt((Estimative - theta_tg)^2 ))


tempcomb1 = datacomb %>% select(-w_10,-w_20,-w_30,-w_50) %>% 
  unite(col = n_10,Labeled_10,comb_10,Ratio_10) %>% 
  unite(col = n_20,Labeled_20,comb_20,Ratio_20) %>%
  unite(col = n_30,Labeled_30,comb_30,Ratio_30) %>%
  unite(col = n_50,Labeled_50,comb_50,Ratio_50) %>%
  gather(n_10,n_20,n_30,n_50,key = "labeled_size",value = "Information") %>%
  separate(col = Information,into = c("Lab","Comb","Ratio"),sep = "_",convert = T)  %>%
  gather(Lab:Ratio,key = Type,value = "Estimative") %>%
  mutate(error = sqrt((Estimative - theta_tg)^2))

tempcomb1$labeled_size = gsub("n_","",tempcomb1$labeled_size)
tempcomb$labeled_size = gsub("n_","",tempcomb$labeled_size)


tempcomb1$method = revalue(tempcomb1$method,c("bella.knn" = "Bella-knn","forman.knn" = "Forman-knn",
                                              
                                              "bella.logistic" = "Bella-LR","forman.logistic" = "Forman-LR",
                                              
                                              "bella.random" = "Bella-RF","forman.random" = "Forman-RF",
                                              
                                              "kernel.gaussian" = "RKHS-Gauss", "kernel.linear" = "RKHS-Linear"))

tempcomb$method = revalue(tempcomb$method,c("bella.knn" = "Bella-knn","forman.knn" = "Forman-knn",
                                            
                                            "bella.logistic" = "Bella-LR","forman.logistic" = "Forman-LR",
                                            
                                            "bella.random" = "Bella-RF","forman.random" = "Forman-RF",
                                            
                                            "kernel.gaussian" = "RKHS-Gauss", "kernel.linear" = "RKHS-Linear"))

tempcomb1$Type = as.factor(tempcomb1$Type)
names(tempcomb1)[names(tempcomb1) == "Type"] = "Estimator"
tempcomb1$Estimator = as.factor(tempcomb1$Estimator)
levels(tempcomb1$Estimator) = c("Combined","Labeled","Ratio")
names(tempcomb)[names(tempcomb) == "theta_tg"] = "Theta"
tempcomb$Theta = as.factor(tempcomb$Theta)


pdf("codes/ratio/outputs/plots/comb.pdf", width = 12,height = 9)

pd <- position_dodge(.2)
tempcomb1 %>% filter(theta_tg == "0.1")  %>%  
  ggplot(aes(x=labeled_size,y=error,colour=Estimator,group=Estimator)) +  coord_trans(y = "log10") +
  geom_point(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(color=Estimator, shape=Estimator ),size = 2.5) +
  geom_line(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(linetype=Estimator )) +
  stat_summary(fun.data = "mean_2se",  size = 0.4) +
  facet_grid(Dataset  ~ method) + theme_bw(base_size = 14) + 
  theme(legend.position="top") +
  labs(x = "Labeled size",y = "Error")

pd <- position_dodge(.2)
tempcomb1 %>% filter(theta_tg == "0.2")  %>%  
  ggplot(aes(x=labeled_size,y=error,colour=Estimator,group=Estimator)) +  coord_trans(y = "log10") +
  geom_point(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(color=Estimator, shape=Estimator ),size = 2.5) +
  geom_line(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(linetype=Estimator )) +
  stat_summary(fun.data = "mean_2se",  size = 0.4) +
  facet_grid(Dataset  ~ method) + theme_bw(base_size = 14) + 
  theme(legend.position="top") +
  labs(x = "Labeled size",y = "Error")

pd <- position_dodge(.2)
tempcomb1 %>% filter(theta_tg == "0.3")  %>%  
  ggplot(aes(x=labeled_size,y=error,colour=Estimator,group=Estimator)) +  coord_trans(y = "log10") +
  geom_point(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(color=Estimator, shape=Estimator ),size = 2.5) +
  geom_line(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(linetype=Estimator )) +
  stat_summary(fun.data = "mean_2se",  size = 0.4) +
  facet_grid(Dataset  ~ method) + theme_bw(base_size = 14) + 
  theme(legend.position="top") +
  labs(x = "Labeled size",y = "Error")

pd <- position_dodge(.2)
tempcomb1 %>% filter(theta_tg == "0.4")  %>%  
  ggplot(aes(x=labeled_size,y=error,colour=Estimator,group=Estimator)) +  coord_trans(y = "log10") +
  geom_point(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(color=Estimator, shape=Estimator ),size = 2.5) +
  geom_line(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(linetype=Estimator )) +
  stat_summary(fun.data = "mean_2se",  size = 0.4) +
  facet_grid(Dataset  ~ method) + theme_bw(base_size = 14) + 
  theme(legend.position="top") +
  labs(x = "Labeled size",y = "Error")


pd <- position_dodge(.2)
tempcomb1 %>% filter(theta_tg == "0.5")  %>%  
  ggplot(aes(x=labeled_size,y=error,colour=Estimator,group=Estimator)) +  coord_trans(y = "log10") +
  geom_point(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(color=Estimator, shape=Estimator ),size = 2.5) +
  geom_line(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(linetype=Estimator )) +
  stat_summary(fun.data = "mean_2se",  size = 0.4) +
  facet_grid(Dataset  ~ method) + theme_bw(base_size = 14) + 
  theme(legend.position="top") +
  labs(x = "Labeled size",y = "Error")

pd <- position_dodge(.2)
ggplot(tempcomb,aes(x=labeled_size,y=Weight,colour=Theta,group=Theta)) +
  geom_point(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(color=Theta, shape=Theta ),size = 2) +
  geom_line(stat="summary",stat_params=list(fun.y="mean"),position=pd,aes(linetype=Theta )) + 
  stat_summary(fun.data = "mean_2se",  size = 0.4) + coord_trans(y = "log10")+
  facet_grid(Dataset  ~ method,scales = "free_y") + theme_bw(base_size = 14) + theme(legend.position="top",legend.title=element_text(size=18)) + 
  labs(x = "Labeled size",y = "Weight") + labs(group=expression(bold(theta)),color=expression(bold(theta)),shape=expression(bold(theta)),
                                               linetype=expression(bold(theta))) +
  guides(group = guide_legend(title.position = "left"),
         color = guide_legend(title.position = "left"),
         shape = guide_legend(title.position = "left"),
         linetype = guide_legend(title.position = "left")) 

dev.off()