## Collect Tweets

install.packages("academictwitteR")
devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE) 
devtools::install_github("tidyverse/tidyverse")

library(academictwitteR)
library(sentimentr)
library(tidyr)
library(rtweet)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidytext)
library(reshape2)
library(dplyr)
library(viridis)
library(textdata)

# Insert bearer token
bearer_token <- "Insert your own token" 
set_bearer()
get_bearer()

#query using hashtags
tweets <-
  get_all_tweets(
    query = "emissions building",
    start_tweets = "20XX-01-01T00:00:00Z",
    end_tweets = "20XX-11-30T00:00:00Z",
    data_path = "data/",
    bind_tweets = FALSE,
    bearer_token = "Insert your own token",
    n= 10000000
  )

#query using username
tweets <-
  get_all_tweets(
    users = c("XXXXXXXXX"),
    start_tweets = "20XX-01-01T00:00:00Z",
    end_tweets = "20XX-11-01T00:00:00Z",
    data_path = "data/",
    bind_tweets = FALSE,
    bearer_token = "Insert your own token",
    n= 1000000
  )


#tweets <- bind_tweets(data_path = "data/")
df <- bind_tweets(data_path = "data/", output_format = "tidy")
head(df)
nrow(df)

#write to csv
write.csv(df, file ="Emi2021.csv")

#Calculate average sentiment score of each tweet
#note to rename the 'text' column to 'sentence' for using the package 'sentimentr'

dfs<- df %>%
  rename(
    sentence = text
  )
head(dfs)

df22 <- get_sentences(dfs)
sentiment_by(df22)

#append ave_sentiment score to the data frame
df$ave_sentiment <- sentiment_by(df1)$ave_sentiment 
head(df, n = 10)

write.csv(df, file ="exxon21.csv", row.names = FALSE)

#timeseries plot using ts_plot 

ts_plot(df, by = "weeks", dtname = "created_at", ticks=1.25)

#handling in POSIXc time series plot; similar to above step using ts_plot

elec <- as.data.frame(df)
head (elec,6)
nrow(elec)

elec$created_at <- ymd_hms(as.character(elec$created_at)) 
elec$Date <- as.Date(elec$created_at, format = "%Y-%m-%d  %H:%M")
elec$Year <- format(as.POSIXct(strptime(elec$created_at, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%Y:%m")
elec$Time <- format(as.POSIXct(strptime(elec$created_at, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
elec$Time <- as.POSIXct(elec$Time, format = "%H:%M")
head(elec, 6)
tail(elec, 6)

#plot volume of tweets over time 


#processing the dataframe 
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets1 <- elec %>% 
  filter(!str_detect(text, "^RT")) %>% 
  mutate(Text = str_replace_all(text, replace_reg, ""))

tidy_tweets <- tidy_tweets1 %>% unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

tidy_tweets %>% count(word, sort = TRUE)

most_common <- tidy_tweets %>% count(word, sort = TRUE)
head(most_common, n = 10)

freq_by_rank <- tidy_tweets %>% count(word, sort = TRUE) %>% mutate(rank = row_number())
freq_by_rank %>% mutate(rank=row_number())

ggplot(data = freq_by_rank) + geom_point(aes(x=rank, y=n))

ggplot(data = freq_by_rank) + geom_point(aes(x=rank, y=n)) + coord_trans(x="log10", y="log10")

rank_subset <- freq_by_rank %>% filter(rank < 50, rank > 10)

ggplot() + geom_point(data = rank_subset, aes(x = rank, y = n), color = "red") + geom_line(data = rank_subset, aes(x = rank, y = n), color = "blue")

daily_tweet <- tidy_tweets %>% mutate(tweetDay = floor_date(Date, "day"))
wordsPerDay <- count(daily_tweet, tweetDay)

#sentiments 

nrc <- get_sentiments("nrc")

tweet_sentiment <- tidy_tweets %>% inner_join(nrc) %>% count(index = Date, sentiment) %>%
  spread(sentiment, n, fill = 0)
head(tweet_sentiment, n = 10)

#write to csv
write.csv(tweet_sentiment, file ="be2021.csv")


