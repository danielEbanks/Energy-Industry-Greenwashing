install.packages("sentimentr")
install.packages("dplyr")
install.packages("rtweet")
install.packages("lubridate")
library(sentimentr)
library(dplyr)
library(rtweet)
library(ggplot2)
library(lubridate)


text_df <- data.frame(id = c("12345","23456","34567"),
                   sentence = c("@EcoStuWood Hi Stuart as part of our net zero ambition we see our investment in oil &amp gas scaling down &amp our investment in low carbon energy ramping up from $500m annually today to around $5 billion in 2030 https://t.co/BCSStAVEkB", 
                                "@richcalhoun Hi Rich, in the near &amp; medium term, gas can support the transition through the displacement of coal in the power mix &amp; as a back-up to renewables. Longer-term, we see decarbonized gas as a critical component of a net zero future https://t.co/BCSStAVEkB", 
                                "We‚Äôve started gas production off the coast of East India. It's the first of three new deep-water gas projects in the block ‚Äì and the deepest in Asia. Together, these projects are expected to meet 15% of India‚Äôs gas demand ‚Äì helping India move to a gas-based economy"),
                   stringsAsFactors = FALSE)
text <- get_sentences(text_df)
sentiment_by(text)

text_df$ave_sentiment <- sentiment_by(text)$ave_sentiment 
head(text_df)

## Read tweet per sentence into dataframe##
Exxon <- read.csv("Exxon_sent.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)[,c('created_at', 'ave_sentiment')]
head(Exxon)

#Calculate averegae sentiment score of each tweet
text <- get_sentences(BHP)
sentiment_by(text)

#append ave_sentiment score to the data frame
BHP$ave_sentiment <- sentiment_by(text)$ave_sentiment 
head(BHP)

#write it into a csv
write.csv(BHP, "BHP_sent.csv", row.names = FALSE)

#histogram
ggplot(BHP, aes(x = ave_sentiment)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density()

ggplot(BHP, aes(x = id, y = ave_sentiment)) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(BHP, aes(id, ave_sentiment, fill = "ave_sentiment")) +
  geom_col(show.legend = FALSE, fill = "orange4")

ts_plot(Exxon, by = "days", trim = 0L, tz = "UTC")


####IMPORTANT for handling posicX#####################
#############################################################################
################################################################################
install.packages("scales")
install.packages("extrafont")
library(lubridate)
library(ggplot2)
library(scales)
library(extrafont)


elec <- read.csv("Exxon_sent.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)[,c('created_at', 'ave_sentiment')]
head(elec,6)
nrow(elec)
elec$created_at <- ymd_hms(as.character(elec$created_at)) 
elec$Date <- as.Date(elec$created_at, format = "%Y-%m-%d  %H:%M")
elec$Year <- format(as.POSIXct(strptime(elec$created_at, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%Y:%m")
elec$Time <- format(as.POSIXct(strptime(elec$created_at, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
elec$Time <- as.POSIXct(elec$Time, format = "%H:%M")
head(elec, 6)
tail(elec, 6)

time.start=as.POSIXct("00:00",format="%H:%M")
time.end=as.POSIXct("23:00",format="%H:%M")


p <- ggplot(na.omit(elec), aes(x=Date, y=ave_sentiment, group=Date)) + 
  geom_line(alpha = 1, color ="deeppink4") +
  labs(x = "Month-Year", 
       y = "Average sentiment scores") 
p + theme(axis.title.y=element_text(size=12, vjust=2, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.5, angle = 90, colour="black")) + 
  scale_x_date(date_labels="%b-%Y", date_breaks = "3 month")

##############################################
##############################################

##Box plot## 

#multiple box plots

devtools::install_version("dplyr", version = "1.0.7")

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)

p <- read.csv("Box_plot.csv", header = TRUE, sep = ",")

data <- p %>%
  gather (key = "text", value = "value")
head(data)

data$text <- as.factor(data$text)
nd <- na.omit(data)


q <- ggplot (nd, aes(x = text, y = value)) +
  geom_boxplot() +
  ylab("Average_sentiment scores")
q

q + theme(axis.text.x = element_text(color= "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(color= "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_text(color= "black", size = 11, angle = 90, hjust = 0.5, vjust = 0.5),
          legend.position = "none") 


q <- ggsave("Boxplot_sent_Bw.jpg", units="in", width = 12, height = 7, dpi = 300)