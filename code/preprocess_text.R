
## Preprocess data

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
