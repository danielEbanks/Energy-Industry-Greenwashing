
library(ggfortify)

# Load  NLP Libraries 

library(tidytext)
library(quanteda)
library(rJST)
library(stm)
library(text2vec)
library(cld3)
# Utilities 
library(dplyr)
library(parallel)
library(ggplot2)
library(lubridate)
library(utf8)
#cluster analysis 
library(factoextra)
library(fpc)
#time series
library(vars)
library(vegan)
library(xtable)
## Load in data

source("IRF_flexible_shock.R")

run_data <- 0

all.files  <- dir("../Dataset_2021/",recursive = T)

list.tweets <- list()
idx         <- 1
for (ff in all.files){
  temp    <- read.csv(paste0("../Dataset_2021/",ff))
  temp$id      <- NULL
  temp$`ï..id` <-NULL
  temp$org     <- substr(ff,1,3)
  list.tweets[[idx]] <- temp
  idx <- idx+1
}

textData <- data.table::rbindlist(list.tweets,use.names = T)
textData <- textData[!duplicated(textData$sentence),]
# create a week
textData$date <- as.POSIXct(
  substr(textData$created_at,1,10),format="%Y-%m-%d")
textData$week   <- isoweek(textData$date)
textData$text <- gsub(pattern = "[^[:alnum:][:space:]]", "",textData$sentence)
textData$text <-stringr::str_remove_all(textData$text, "[^[\\da-zA-Z ]]")
textData <- textData  %>% filter(substr(text,1,2)!="RT",
                                 utf8_valid(text),
                                 lengths(strsplit(text, "\\W+")) >3)



attach(textData)
llDisplay <- data.frame(text = paste(text),week=textData$week,handle=user_username,created_at=created_at,org=org,stringsAsFactors = F)
detach(textData)


stop_words_custom = c("family","get","adam","hear","de","la","los",
                      'el', 'para', 'en', 'que',"lo","RT","&","amp",
                      "amend","back","protect","commun","service","work","around","alway","november","august","january",
                      "happen","ive","hall","nation","work","service","this","discuss","community","learn","say","said","talk","congrats","congratulations","are","as","i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", 
                      "yours","he","her","him","she","hers","that","be","with","their","they're","is","was","been","not","they","it","have","will","has","by","for","Mister",   
                      "house" , "start" ,     
                      "one",   "thing"    ,"bring","put", "north","give","keep","pa","even","texa","year","join","well",
                      "call",  "learned"    ,"things" ,"things","can't","can","cant","will","go","going","let",
                      "lets","let's","say","says","know","talk","talked","talks","lady","dont","think","said","something",
                      "something","wont","people","make","want","went","goes","people","person","like","come","from",
                      "need","us",paste(textData$firstname),paste(tidytext::stop_words$word),stopwords())

toks1 <- tokens(corpus(llDisplay), remove_punct = TRUE)
toks2 <- tokens_remove(toks1, stop_words_custom)
toks3 <- tokens_ngrams(toks2, 1:3)
toks3 <- tokens_wordstem(toks3)


## Create Paradigm list
df_p       = readr::read_csv("../Data/paradigm.csv")




N = nrow(llDisplay)

# create a document feature matrix (quanteda style) to run JST
dfm_speeches <- dfm(toks3 , 
                    remove_punct = TRUE ,
                    remove_numbers = TRUE,
                    stem = T) %>% 
  dfm_trim(min_termfreq = 400,
           min_docfreq =0.002*N,
           max_docfreq = 0.4*N)


## top 100 sentiments
top_sents =df_p[ df_p$Token %in% names(topfeatures(dfm_speeches,n=1000)), ]

neutral_features = names(topfeatures(dfm_speeches,n=10000))[!(dfm_speeches@Dimnames$features
                                                              %in%top_sents$Token)]

paradigm_dict =dictionary(list(positive = top_sents$Token[top_sents$Positive==0.9],
                               negative = top_sents$Token[top_sents$Negative==0.9]
))


#optimize for topic number
kk = c(5,10,15,20,25,28,30,40,50,60)

if(run_data==1){

print(Sys.time())
results <- mclapply(kk,
                    FUN = function(kk) 
                      jst(dfm_speeches,
                          paradigm_dict,
                          numTopics = kk,
                          numSentiLabs = 3,
                          numIters = 2000),
                    mc.cores = getOption("mc.cores", 
                                         10L))  
print(Sys.time())

save(results,file="../Data/modelOutput3senti-Clim-1000.RData")

}

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




topwords.df <- top20words(results[[2]])
write.csv(topwords.df,file="../topwords.csv")




########################### SM


print(xtable::xtable(topicLabels,
             label    = "tab:TopicLabels",
             caption  = "Topic Labels",
             align    = "cc"),
      caption.placement = "top",
      include.rownames  = F,
      file      = "../tables/TopicLabels.tex") 


TopicTable <- data.frame(topNwords(results[[2]],N = 5))
colnames(TopicTable) <- topicLabels$label
topicLabels$name <- colnames(data.frame(topNwords(results[[2]],N = 5)))
print(xtable(t(TopicTable),
             label    = "tab:TopicWords",
             caption  = "Top 5 Topic Words",
             align    = "cccccc"),
      caption.placement = "top",
      include.rownames  = T,
      file      = "../tables/TopicWords.tex")



contribs <- res.var$contrib
  contribs <- as.data.frame(contribs)%>% mutate(Label=rownames(contribs),TopicTitle=topicLabels$label)
contribs <- contribs %>% 
  dplyr::select(TopicTitle,
                Dim.1,
                Dim.2)


contribs_strip<-contribs %>%
  arrange(-Dim.1) %>%
  rename_at(vars(TopicTitle,
                 Dim.1),
            ~c("Topic","Contribution")) %>%
  dplyr::select(Topic,Contribution) %>%
  na.omit()


print(xtable(contribs_strip %>%filter(Contribution>=1), 
             label    = "tab:TopicContribs",
             caption  = "Top PCA Topic Drivers of Seperation",
             align    = "cLc"),
      caption.placement= "top",
      include.rownames = F,
      file      = "../tables/TopicContribs_top.tex") 



print(xtable(contribs_strip, 
             label    = "tab:TopicContribs_all",
             caption  = "PCA Topic Contributions",
             align    = "cLc"),
      tabular.environment = "longtable",
      caption.placement = "top",
      floating = FALSE,
      include.rownames = FALSE, # because addtorow will substitute the default row names 
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after=c(-1),
      file      = "../tables/TopicContribs_all.tex") 







theta_summary <- theta %>% 
  tidyr::pivot_longer(cols=(topic1sent1:topic10sent3)) %>% 
  group_by(name) %>%
  mutate(Max=max(value),doc_id=as.numeric(docID)) %>%
  filter(Max==value) %>%
  dplyr::select(name,handle,docID)   

llDisplay<-llDisplay %>%mutate(docID=paste0("text",rownames(llDisplay)))

theta_emblematic_tweet <- theta_summary %>% 
  left_join(llDisplay %>%
              dplyr::select(text,docID),
            by="docID") %>%
  left_join(topicLabels,
            by=c("name")) %>%
  ungroup()%>%
  dplyr::select(handle,text,label) %>%
  na.omit() %>%
  distinct(label,.keep_all = T) %>%
  arrange(label)

stargazer(theta_emblematic_tweet,
          summary  = F,
          rownames = F,
          label    = "tab:EmbTweet",
          title    = "Emblematic Tweets",
          out      = "../tables/EmbTweet.tex")




#### summary stats




pdf("../plots/SM_Ind_extremeweather.pdf")
ggplot(Org_vec_wide,aes(x=Date,y=topic6sent1_Ind*100))+
  ylab("Fossil Fuel Firms' Propensity to\n Discuss Extreme Weather (%)")+
  xlab("")+
  ylim(c(0,20))+
  geom_line()+theme_classic()
dev.off()


pdf("../plots/SM_NGO_divest.pdf")
ggplot(Org_vec_wide,aes(x=Date,y=topic8sent1_NGO*100))+
  ylab("NGOs Propensity to\n Discuss Divestment (%)")+
  xlab("")+
  ylim(c(0,20))+
  geom_line()+theme_classic()
dev.off()

pdf("../plots/SM_IGO_divest.pdf")
ggplot(Org_vec_wide,aes(x=Date,y=topic8sent1_IGO*100))+
  ylab("IGOs Propensity to\n Discuss Divestment (%)")+
  xlab("")+
  ylim(c(0,20))+
  geom_line()+theme_classic()
dev.off()


pdf("../plots/SM_IGO_jobs.pdf")
ggplot(Org_vec_wide,aes(x=Date,y=topic5sent2_IGO*100))+
  ylab("IGOs Propensity to\n Renewable Jobs (%)")+
  xlab("")+
  ylim(c(0,20))+
  geom_line()+theme_classic()
dev.off()

topic5sent2

summary(Org_vec_wide)
pdf("../plots/SM_Returns.pdf")
ggplot(Org_vec_wide,aes(x=Date,y=returns*100))+
  ylab("Average Daily Stock Returns (%)")+
  xlab("")+
  ylim(c(-20,20))+
  geom_line()+theme_classic()
dev.off()


ret_summary <- Org_vec_wide%>%dplyr::select(returns) %>%summarise_each(funs(Mean = mean, 
                               St.dev = sd,
                               Min = min, 
                               Q25 = quantile(., 0.25), 
                               Median = median, 
                               Q75 = quantile(., 0.75), 
                               Max = max))

stargazer(ret_summary*100,
          summary  = F,
          digits = 2,
          rownames = F,
          label    = "tab:ReturnSummary",
          title    = "Stock Market Returns Data",
          out      = "../tables/returns.tex")

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\hline \n",
  "\\endhead \n",
  "\\hline \n",
  "{\\footnotesize Continued on next page} \n",
  "\\endfoot \n",
  "\\endlastfoot \n",
  sep=""))


captions_tabs <- c("Daily Propensity: Industry",
                   "Daily Propensity: NGOs",
                   "Daily Propensity: IGOs")
table_data_list <- list(Ind_vec,
                        NGO_vec,
                        IGO_vec)


file_tabs <-c("tab_DailyAttIND",
              "tab_DailyAttNGO",
              "tab_DailyAttIGO")

labels_tabs <-c("tab:DailyAttIND",
               "tab:DailyAttNGO",
               "tab:DailyAttIGO")


topicLabels$name <- names(data.frame(topNwords(results[[2]],N = 5)))

for(tt in 1:length(table_data_list)){
  print(xtable(table_data_list[[tt]]%>%ungroup()%>%
                 dplyr::select(starts_with("topic")) %>%
                 rename_at(vars(topicLabels$name),~topicLabels$label)%>%
                 summarise_each(funs(Mean = mean, 
                                     St.dev = sd,
                                     Min = min, 
                                     Q25 = quantile(., 0.25), 
                                     Median = median, 
                                     Q75 = quantile(., 0.75), 
                                     Max = max
                 )) %>% 
                 gather(stat, val) %>%
                 separate(stat, into = c("Topic", "Statistic"), sep = "_") %>%
                 spread(Statistic, val) %>%
                 mutate(Topic,
                        Mean=100*Mean, 
                        St.dev=100*St.dev, 
                        Min=Min*100,
                        Q25=Q25*100, 
                        Median=Median*100, 
                        Q75=Q75*100, 
                        Max=Max*100),
               caption = captions_tabs[tt],
               label   = labels_tabs[tt],
               align="lL|ccccccc"),
        tabular.environment = "longtable",
        caption.placement = "top",
        floating = FALSE,
        include.rownames = FALSE, # because addtorow will substitute the default row names 
        add.to.row = addtorow,     # this is where you actually make the substitution
        hline.after=c(-1),
        file=paste0("../tables/table_",file_tabs[tt],".tex")
  )
}


## Coherence 
pdf("../plots/SM-coherence.pdf")
g <- ggplot(data=t(result_co),aes(x=kk,y=mean_npmi_cosim))+
  geom_line()+xlab("Number of Topics") +
  ylab("Mean NPMI Cosine Similarity") +
  geom_vline(xintercept = 10,color="red")+
  theme_classic()
plot(g)
dev.off()


plot_freq <- theta  %>% group_by(Date) %>%summarise(Frequency=n())


pdf("../plots/SM-tweet_per_day.pdf")
g <- ggplot(data=plot_freq %>%filter(year(Date)>=2014),aes(x=Date,y=Frequency))+
  geom_line()+xlab("") +
  ylab("Tweets per Day (#)") +
  theme_classic()
plot(g)
dev.off()

