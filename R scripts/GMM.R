#model-based clustering
devtools::install_version("tidyr", version = "1.1.3")
library(factoextra)
library(mclust)
library(tidyverse)
library(ggplot2)
remove.packages("tidyr")

df <- read.csv("mfi.csv", header = TRUE, sep = ",")
df <- p[,-2]
head(df)
dfa <- na.omit(df)
head(dfa)
df.scaled <- scale(dfa)
df.scaled <- select(dfa, 5)
head(df.scaled)

#creating a dataframe
gmm <- data.frame(df.scaled)

#run the gmm
mc <- Mclust(gmm)
summary(mc)

#generate the cluster column in the data frame
gmm$CLUST <- mc$classification
head(gmm)

#export it as a csv file
write.csv(gmm, file = "MFIq.csv", row.names = FALSE)                    

#BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")

#classification plot showing the clustering

fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")

#Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco")

#scatter plot matrix
plot(mc, what=c("classification"))

plot(mc, what=c("density"))

plot(mc, what=c("uncertainty"))

plot(mc, what=c("BIC"))

#density estimates via GMM
dens <- densityMclust(dfa)
summary(dens)

plot(dens, what = "density", data = dfa, grid = 200, points.cex = 0.5,
     drawlabels = FALSE)
plot(dens, what = "density", type = "image", col = "firebrick", grid = 200)
plot(dens, what = "density", type = "persp", theta = -25, phi = 20)