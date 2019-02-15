
features <- read.csv('/Users/mollysrour/Desktop/MSIA-421-DATA-MINING/03\ EDA/features.csv')
set.seed(12345)
fit = kmeans(features[,42:47], 10, 100, 100)
summary(fit)
table(fit$cluster)
fit$centers
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
    RMSE = round(sqrt(c(fit$withinss/(p*(fit$size-1)), sse/(p*(n-k)))), 4)
  ))
  cat("SSE = ", sse, "; SSB = ", ssb, "\n")
  cat("R-Squared = ", ssb/(ssb+sse), "\n")
  cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
  invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}

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
summary.kmeans(fit)
plot.kmeans(fit)


FINALREF_FIT <-  kmeans(features[,42:47], 4, 100, 100)
plot.kmean(FINALREF_FIT)
referral_cluster <- data.frame(features$fire_fly_id, FINALREF_FIT$cluster)
FINALCONTENT_FIT <- kmeans(features[,34:38], 4, 100, 100)
content_cluster <- data.frame(features$fire_fly_id, FINALCONTENT_FIT$cluster)
plot.kmeans(FINALCONTENT_FIT)



