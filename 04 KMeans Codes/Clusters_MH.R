
library(dplyr)
library(reshape2)
library(tidyverse)

features <- read.table('features.txt',sep = ',', header = TRUE)

breadth <- features %>% select(., secPerVisit, subsecPerVisit, contentPerVisit) # data frame for breadth clustering
temporal <- features %>% select(., avgTime,recency)  # data frame for temporal clustering

allaround <- features %>% select(., secPerVisit,subsecPerVisit, avgTime,recency, pct_home,pct_news,pct_sports, pct_story,pct_homefront,pct_section.front, page_views_per_visit, Typed.Bookmarked,Inside.Your.Site)

### CLUSTERING FUNCTIONS

## summary of k means
summary.kmeans = function(fit)
{
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  n = sum(fit$size)
  sse = sum(fit$withinss)
  xbar = t(fit$centers)%*%fit$size/n
  ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2)
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
  ))
  cat("SSE = ", sse, "; SSB = ", ssb, "\n")
  cat("R-Squared = ", ssb/(ssb+sse), "\n")
  cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
  invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}


## plot of k means
plot.kmeans = function(fit,boxplot=F)
{
  require(lattice)
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){
                  panel.dotplot(...)
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1),
                xlab="Cluster Mean"
  ))
  invisible(plotdat)
}

### BREADTH CLUSTERING
set.seed(123)
fitBreadth <- kmeans(breadth,3,nstart = 20, iter.max = 100)
summary.kmeans(fitBreadth)
plot.kmeans(fitBreadth)

#merge with customer id
breadthCluster <- as.data.frame(cbind(features$fire_fly_id, fitBreadth$cluster))
colnames(breadthCluster) <- c("fire_fly_id", "cluster")
breadthCluster$clusterName <- ifelse(breadthCluster$cluster == 1, "High Breadth All Around", ifelse(breadthCluster$cluster == 2,"Average Breadth","Low Subsections"))

### TEMPORAL CLUSTERING ###
set.seed(456)
fitTemporal <- kmeans(temporal,3,nstart = 20, iter.max = 100)
summary.kmeans(fitTemporal)
plot.kmeans(fitTemporal)

# merge with customer id
temporalCluster <- as.data.frame(cbind(features$fire_fly_id, fitTemporal$cluster))
colnames(temporalCluster) <- c("fire_fly_id", "cluster")
temporalCluster$clusterName <- ifelse(temporalCluster$cluster == 1, "Newer but Infrequent Visits", ifelse(temporalCluster$cluster == 2, "Older but Frequent Visits","Average Temporal"))

### WRITE TO CSV ###

Clusters <- inner_join(breadthCluster, temporalCluster, by = "fire_fly_id")
colnames(Clusters) <- c("fire_fly_id","Breadth Number", "Breadth Name", "Temporal Number", "Temporal Name")

write.csv(Clusters,"clusters_MH.csv")


###### EXPERIMENT WITH ALL AROUND CLUSTER #####

## standardize variables
allaroundScale <- scale(allaround)

## find number of clusters with Max F
F = double(7)
for(K in 2:8)
  F[K-1] = summary(
    kmeans(allaroundScale,K,nstart = 100))$F
plot(2:8, F, type = "b",xlab = "Number Clusters K")


fitAll <- kmeans(allaroundScale, 4, nstart = 100 )


