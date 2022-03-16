## Load in stock data post 2010
library(dplyr)
library(lubridate)

f <- function(x,pos) x%>%  select(KYPERMNO,
                                  CALDT,
                                  PRC,
                                  RET,
                                  RETX,
                                  TCAP,
                                  VOL) %>%
  filter(year(as.Date(CALDT))>="2008")


temp_2021 <- readr::read_delim_chunked("../Data/fz_dp_dly.dat",
                                     col_names  = c("KYPERMNO",
                                                    "CALDT",
                                                    "PRC",
                                                    "RET",
                                                    "RETX",
                                                    "TCAP",
                                                    "VOL"),
                                     delim="|",  
                                     callback = readr::DataFrameCallback$new(f),
                                     chunk_size =5000000)
## Merge in ticker info
nm_temp_2021 <- readr::read_delim("sfz_nam.dat",delim="|",col_names=c("KYPERMNO",
                                                                      "NAMEDT",
                                                                      "NAMEENDDT",
                                                                      "NCUSIP",
                                                                      "NCUSIP9",
                                                                      "TICKER",
                                                                      "COMNAM",
                                                                      "SHRCLS",
                                                                      "SHRCD",
                                                                      "EXCHCD",
                                                                      "SICCD",
                                                                      "TSYMBOL",
                                                                      "SNAICS",
                                                                      "PRIMEXCH",
                                                                      "TRDSTAT",
                                                                      "SECSTAT")) %>%
  select(KYPERMNO,NAMEDT,NAMEENDDT,TICKER)%>% 
  filter(year(NAMEENDDT)>=2008, TICKER %in% c("RDS.A",
                                              "BP",
                                              "XOM",
                                              "CVX",
                                              "COP",
                                              "TTE",
                                              "BTU",
                                              "BHP"
                                              )) 


keep_keys <- unique(nm_temp_2021$KYPERMNO)
## subset on tickers
temp_2021 <- temp_2021 %>%filter(KYPERMNO %in% keep_keys)

save(temp_2021,"../Data/stock_returns_ind.RData") 
 
