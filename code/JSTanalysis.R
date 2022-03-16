






#optimize for topic number
kk = c(5,10,15,20,25,28,30,40,50,60)


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



