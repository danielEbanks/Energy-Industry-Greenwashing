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



## Run analysis Scripts:


# Collect Tweet Data

source("collect_tweets.R")

#load stock data

source("load_stock_data.R")

# Preoprocess the data

source("preprocess_text.R")

# Run JST

source("JSTanalysis.R")

# PCA+IRF analysis

source("PCA+IRF.R")

# Create SM Tables
source("SM.R")

