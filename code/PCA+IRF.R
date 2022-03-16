
load("../Data/stock_returns_ind.RData") 
load("../Data/modelOutput3senti-Clim-1000.RData")

dtm <- dfm(dfm_speeches,verbose = F)
tcm <- Matrix::crossprod(sign(dtm))

result_co = sapply(seq(1:10),FUN=function(x) colMeans(coherence(as.matrix(top20words(results[[x]])), tcm, metrics = c("mean_logratio", "mean_pmi", "mean_npmi",
                                                                                                                      "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                                                                smooth = 1e-12, n_doc_tcm = N),na.rm=T))

rand_list  <- list()
clust_data <- list()

main_data_list   <- list()
k_index  <- seq(1,10,1)

#filter_n <- seq(0,20,10)
filter_n=20
n_senti=3

clust_stats_JST <- list()
for(ff in 1:length(filter_n)){
  rand_list[[ff]]        <- list()
  clust_data[[ff]]       <- list()
  
  print(ff)
  #for(k in 1:length(results)){
  for(k in c(2)){
    
    # extract parameters 
    theta <- get_parameter(results[[k]],"theta")
    phi   <- get_parameter(results[[k]],"phi")
    pi    <- get_parameter(results[[k]],"pi")
    
    
    
    
    #  concatenate sentiment and topic probabilities into one dataframe
    theta$sent1 <- pi$sent1
    theta$sent2 <- pi$sent2
    if(n_senti>2){
      theta$sent3 <- pi$sent3
    }
    
    
    
    
    
    cutoff<- kk[k]
    theta_est<-theta
    theta_est[,9:(9+cutoff)] <- theta[,9:(8+cutoff)]*theta$sent1
    theta_est[,(9+cutoff):(8+2.5*cutoff)] <- theta[,(9+cutoff):(8+2.5*cutoff)]*theta$sent2
    theta_est[,(9+2.5*cutoff):(8+3*cutoff)] <- theta[,(9+2.5*cutoff):(8+3*cutoff)]*theta$sent2
    
    if(n_senti==3){
      PCA_vecs_agg    <- theta%>% 
        group_by(org,handle)%>% 
        mutate(n_tweets = n()) %>%
        filter(n_tweets>filter_n[ff]) %>% 
        ungroup() %>%dplyr::select(org,handle,topic1sent1:sent3) %>% 
        group_by(org,handle)%>%
        summarise_all(list(mean))
    }
    
    if(n_senti==2){
      PCA_vecs_agg    <- theta%>% 
        group_by(org,handle)%>% 
        mutate(n_tweets = n()) %>%
        filter(n_tweets>filter_n[ff]) %>% 
        ungroup() %>%dplyr::select(org,handle,topic1sent1:sent2) %>% 
        group_by(org,handle)%>%
        summarise_all(list(mean))
    }
    
    
    PCA_vecs_agg$n_tweets <- NULL 
    
    #compute hellinger distance 
    
    
    if(n_senti==3){
      pca_mat <- as.matrix(PCA_vecs_agg%>%ungroup %>%dplyr::select(topic1sent1:sent3))
      pca_mat <- (pca_mat[,1:(ncol(pca_mat)-n_senti)])
    }
    
    if(n_senti==2){
      pca_mat <- as.matrix(PCA_vecs_agg%>%ungroup %>%
                             dplyr::select(topic1sent1:sent2))
      pca_mat <- (pca_mat[,1:(ncol(pca_mat))])
    }
    
    fit6  <-prcomp(pca_mat) 
    
    
    
    
    
    res.var <- get_pca_var(fit6)
    ind.var <- get_pca_ind(fit6)
    pc1 <- ind.var$coord[,1]
    pc2 <- ind.var$coord[,2]
    
    PCA_vecs_agg$dim1 <- pc1
    PCA_vecs_agg$dim2 <- pc2
    
    #stress_list[[ff]][[k]] <-fit4$copstress
    
    PCA_vecs_agg %>% filter(abs(dim2)<=0.3) %>% group_by(org) %>% 
      summarise(n(),mean(dim1),
                mean(dim2),sd(dim1),sd(dim2))%>%filter('n()'>filter_n[ff])
    g <- ggplot( PCA_vecs_agg %>%filter(abs(dim1)<=0.3),aes(dim1,dim2,colour=org))+
      geom_point() + 
      scale_size_manual(values=c(2,5))+theme_bw()+scale_color_manual(values=c("#2E74C0", "#CB454A","#228B22"))+
      ggtitle("Aggregate Message Positions-PCA")+
      labs(colour="Organization Type")
    plot(g)
    pdf(file=paste0("../plots/clim_total_JST_mespos_agg_",k,".pdf"))
    plot(g)
    dev.off()
    
    
    df.clust <- fit6$x[(fit6$x[,2])<(mean(fit6$x[,2]))+(2.5*sd(fit6$x[,2])) &
                         (fit6$x[,2])>(mean(fit6$x[,2]))-(2.5*sd(fit6$x[,2])) &
                         (fit6$x[,1])<(mean(fit6$x[,1]))+(2.5*sd(fit6$x[,1])) &
                         (fit6$x[,1])>(mean(fit6$x[,1]))-(2.5*sd(fit6$x[,1])) 
                       ,c(1,2)]
    km.res <- eclust(df.clust , "kmeans", k = 2, nstart = 25, graph = FALSE)
    org    <- as.numeric(factor(PCA_vecs_agg$org[(fit6$x[,2])<(mean(fit6$x[,2]))+(2.5*sd(fit6$x[,2])) &
                                                   (fit6$x[,2])>(mean(fit6$x[,2]))-(2.5*sd(fit6$x[,2])) &
                                                   (fit6$x[,1])<(mean(fit6$x[,1]))+(2.5*sd(fit6$x[,1])) &
                                                   (fit6$x[,1])>(mean(fit6$x[,1]))-(2.5*sd(fit6$x[,1]))]))
    
    clust_stats_JST[[k]] <- data.frame(vi=cluster.stats(d = dist(df.clust ), 
                                                        alt.clustering=org, clustering=km.res$cluster)$vi,
                                       rand=cluster.stats(d = dist(df.clust), alt.clustering=org, clustering=km.res$cluster)$corrected.rand,
                                       topic_n=k,model="JST") 
  }   
}   

clust_stats_JST <- data.table::rbindlist(clust_stats_JST)
clust_stats_JST$topic_n <- kk 



## time series

############
## VAR
theta <- get_parameter(results[[2]],"theta")
theta$Date <- as.POSIXct(
  substr(theta$created_at,1,10),format="%Y-%m-%d")

## VAR

stock_rtrn_vec <- temp_2021       %>%
  group_by(CALDT) %>%
  summarise(returns = mean(RET,na.rm=T)) %>%
  rename(Date=CALDT)

IGO_vec <-  theta %>% 
  group_by(Date,org)        %>% 
  filter(org=="IGO",year(Date)>=2014)        %>%
  summarise(dplyr::across(topic1sent1:topic10sent3,mean)) %>% 
  mutate(handle="IGO")      %>%
  dplyr::select(Date, handle,topic1sent1:topic10sent3)

NGO_vec <-  theta %>% 
  group_by(Date,org)        %>% 
  filter(org=="NGO",year(Date)>=2014)        %>%
  summarise(dplyr::across(topic1sent1:topic10sent3,mean)) %>% 
  mutate(handle="NGO")      %>%
  dplyr::select(Date, handle,topic1sent1:topic10sent3)

Ind_vec <-  theta %>% 
  group_by(Date,org)        %>% 
  filter(org=="Ind",year(Date)>=2014)        %>%
  summarise(dplyr::across(topic1sent1:topic10sent3,mean)) %>% 
  mutate(handle="Ind")      %>%
  dplyr::select(Date, handle,topic1sent1:topic10sent3)


Org_vec_all <- rbind(IGO_vec,NGO_vec,Ind_vec)

Org_vec_wide <- Org_vec_all%>% ungroup()        %>% 
  dplyr::select(Date, handle,topic1sent1:topic10sent3) %>%
  tidyr::pivot_wider(names_from=handle,
                     values_from=c(topic1sent1:topic10sent3)) 
Org_vec_wide <- Org_vec_wide %>% 
  na.omit()    %>%
  left_join(stock_rtrn_vec) %>%
  na.omit()


################################
#initialize lists




ts.mat   <- ts(Org_vec_wide[,2:(length(Org_vec_wide))])
ts.mat[,1:(ncol(ts.mat)-1)]   <- log(ts.mat[,1:(ncol(ts.mat)-1)] /(1-ts.mat[,1:(ncol(ts.mat)-1)] ))
var_aic  <- VAR(ts.mat, type = "none", lag.max = 7, ic = "AIC")
optimalP <- var_aic$p

if(run_var ==1){
  test <- VAR( ts.mat, optimalP, type = "const")
  
  
  
  
  var_irf_1 <- irf_corrected(test ,
                             n.ahead=5,
                             shock=1,
                             runs = 200,
                             ortho = T)
  
  
  save(var_irf_1,file = "../Data/IRF.save.RData")
  
}
load("../Data/IRF.save.RData")


topicLabels <- data.frame(label=c("Climate Action - COP","Protect Oceans", "Toxic Pollution Effects",
                                  "IPCC Report"         ,"Sustainable Development","Pass Legislation",
                                  "Job Offers"          ,"Pesticides/Food"        ,"Pruitt/Trump, EPA",
                                  "Reduce Carbon Emissions","Share a video"       ,"London Climate Protests",
                                  "Tackle Climate Change","Clean Energy/Renewables Jobs","Coal Pollution",
                                  "Extreme Weather","Low Carbon/Future of Gas","Trump Artic Drilling",
                                  "Twitter Engagement","Sustain the environment","Invest Fossil Fuel Industry",
                                  "Divest Fossil Fuels","Climate Justice/Crisis","Indigenous rights",
                                  "Solar Power","Mayors/Cities/C40","Stop Keystone",
                                  "Electric EV Business","Promoting STEM","Endangered Species"))
varLabels <- c(topicLabels[rep(seq_len(nrow(topicLabels)), 
                               each = 3), ],"Stock Returns")



temp    <- list()
irf_mat <- list()
idx     <- 1

for(i in 1:length(var_irf_1$irf)) {
  print(i)
  IRF<-DeltaLOtoDeltaProb(var_irf_1$irf[[idx]],
                          impulse=(ts.mat[,idx]),
                          response=(ts.mat),
                          delta=1)
  Lower<-DeltaLOtoDeltaProb(var_irf_1$Lower[[idx]],
                            impulse=(ts.mat[,idx]),
                            response=(ts.mat),
                            delta=1)
  Upper<-DeltaLOtoDeltaProb(var_irf_1$Upper[[idx]],
                            impulse=(ts.mat[,idx]),
                            response=(ts.mat),
                            delta=1)
  
  
  irf_mat[[idx]]        <- data.frame(t(IRF))  
  temp[[idx]]           <- data.frame(IRF = (IRF),
                                      Lower = (Lower),
                                      Upper = (Upper))
  rownames(temp[[idx]])    <- colnames(ts.mat)
  colnames(irf_mat[[idx]]) <- colnames(ts.mat)
  
  idx <-idx+1 
  
}




irf_mat <- data.table::rbindlist(irf_mat)
irf_mat <- as.matrix(irf_mat)
org  <- gsub("(.*)_","",colnames(irf_mat))
colnames(irf_mat)<- c(paste0(varLabels,": ",org))
rownames(irf_mat) <- colnames(irf_mat)


sig.coords <- lapply(1:length(temp),FUN = function(x){ 
  influencer <- x 
  influencee <- which(sign(temp[[x]]$Upper)==sign(temp[[x]]$Lower))
  return(data.frame(influencer,influencee,impulse = colnames(irf_mat)[x]))   }                 )

sig.coords <- data.table::rbindlist(sig.coords)
sig.coords <-sig.coords[which(sig.coords[,1]!=sig.coords[,2]),]


sig.coords$respondent <- NA
sig.coords$IRF   <- NA
sig.coords$Upper <- NA
sig.coords$Lower <- NA

for (ii in 1:nrow(sig.coords)){
  sig.coords$respondent[ii] <-colnames(irf_mat)[sig.coords$influencee[ii]] 
  sig.coords$IRF[ii]   <-temp[[sig.coords$influencer[ii]]]$IRF[sig.coords$influencee[ii]] 
  sig.coords$Upper[ii] <-temp[[sig.coords$influencer[ii]]]$Upper[sig.coords$influencee[ii]] 
  sig.coords$Lower[ii] <-temp[[sig.coords$influencer[ii]]]$Lower[sig.coords$influencee[ii]] 
}



### Stock Returns

dat.plot <- sig.coords %>%filter(gsub("(.*): ","",respondent) !="Ind",
                                 gsub("(.*): ","",impulse) == "returns",
                                 gsub("(.*): ","",respondent) !=gsub("(.*): ","",impulse),
)%>%
  arrange(IRF) %>%mutate(rank=1:n())


# Stock Returns

dat.plot <- dat.plot %>% ungroup()%>%
  mutate(respondent=factor(rank,
                           labels=(dat.plot$respondent)
  ))

pdf("../plots/fig 4n.pdf")
g<- ggplot(aes( IRF,
                respondent),
           data=dat.plot) + 
  geom_point(size=4,
             color="blue") +
  geom_errorbarh(aes(xmin = Lower, 
                     xmax = Upper),
                 height = 0.2, 
                 linetype="solid",
                 color="dodgerblue2") + 
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("IGO/NGO Response to Stock Market Returns") +
  theme(axis.text = element_text(size = 1))+
  theme_bw()
plot(g)
dev.off()





### Extreme weather


dat.plot <- sig.coords %>%filter(gsub("(.*): ","",respondent) =="Ind",
                                 gsub(": (.*)","",respondent) =="Extreme Weather",
                                 gsub("(.*): ","",respondent) !=gsub("(.*): ","",impulse),
                                 IRF >0)%>%
  arrange(IRF) %>%mutate(rank=1:n())


# Extreme Weather Diversion

dat.plot <- dat.plot %>% ungroup()%>%
  mutate(impulse=factor(rank,
                        labels=(dat.plot$impulse)
  ))

pdf("../plots/extremeWeather_ind.pdf")
g<- ggplot(aes( IRF,
                impulse),
           data=dat.plot) + 
  geom_point(size=1,
             color="blue") +
  geom_errorbarh(aes(xmin = Lower, 
                     xmax = Upper),
                 height = 1, 
                 linetype="solid",
                 color="dodgerblue2") + 
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Industry's Percentage Point Increase\n in Daily Propensity to Discuss\n Extreme Weather") +
  theme(axis.text = element_text(size = 1))+
  theme_bw()
plot(g)
dev.off()



dat.plot <- sig.coords %>%filter(gsub("(.*): ","",respondent) =="Ind",
                                 gsub(": (.*)","",respondent) =="Divest Fossil Fuels",
                                 gsub("(.*): ","",respondent) !=gsub("(.*): ","",impulse),
                                 IRF >0)%>%
  arrange(IRF) %>%mutate(rank=1:n())


# Divesting Diversion

dat.plot <- dat.plot %>% ungroup()%>%
  mutate(impulse=factor(rank,
                        labels=(dat.plot$impulse)
  ))

pdf("../plots/divest_IND.pdf")
g<- ggplot(aes( IRF,
                impulse),
           data=dat.plot) + 
  geom_point(size=1,
             color="blue") +
  geom_errorbarh(aes(xmin = Lower, 
                     xmax = Upper),
                 height = 1, 
                 linetype="solid",
                 color="dodgerblue2") + 
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Industry's Percentage Point Increase\n in Daily Propensity to Discuss\n Divesting") +
  theme(axis.text = element_text(size = 1))+
  theme_bw()
plot(g)
dev.off()

#### remainder


dat.plot <- sig.coords %>%filter(gsub("(.*): ","",respondent) =="Ind",
                                 gsub(": (.*)","",respondent) !="Extreme Weather",
                                 gsub("(.*): ","",respondent) !=gsub("(.*): ","",impulse),
                                 gsub(": (.*)","",respondent) !="Divest Fossil Fuels",
                                 IRF >0.75)%>%
  arrange(IRF) %>%mutate(rank=1:n())

dat.plot <- dat.plot %>% ungroup()%>%
  mutate(label=paste(impulse,gsub(": Ind","",respondent))) %>%
  mutate(label=factor(rank,
                      labels=(label),
  ))

pdf("../plots/remainder_IND.pdf")
g<- ggplot(aes( IRF,
                label),
           data=dat.plot) + 
  geom_point(size=1,
             color="blue") +
  geom_errorbarh(aes(xmin = Lower, 
                     xmax = Upper),
                 height = 1, 
                 linetype="solid",
                 color="dodgerblue2") + 
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Industry's Percentage Point Increase\n in Daily Propensity to Discuss\n Remaining Topics") +
  theme(axis.text = element_text(size = 1))+
  theme_bw()
plot(g)
dev.off()



## Negative Responds


dat.plot <- sig.coords %>%
  filter(gsub("(.*): ","",respondent) =="Ind",
         gsub("(.*): ","",respondent) !=gsub("(.*): ","",impulse),
         IRF < -0.5) %>%
  arrange(-IRF)%>% mutate(rank=1:n())

dat.plot <- dat.plot %>% ungroup()%>%
  mutate(label=paste0(impulse,": ",gsub(": Ind","",respondent))) %>%
  mutate(label=factor(rank,
                      labels=(label),
  ))


## Negative response

pdf("../plots/negative_IND.pdf")
g<- ggplot(aes( IRF,
                label),
           data=dat.plot) + 
  geom_point(size=1,
             color="blue") +
  geom_errorbarh(aes(xmin = Lower, 
                     xmax = Upper),
                 height = 1, 
                 linetype="solid",
                 color="dodgerblue2") + 
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Industry's Percentage Point Decrease\n in Daily Propensity to Discuss") +
  theme(axis.text = element_text(size = 1))+
  theme_bw()
plot(g)
dev.off()




