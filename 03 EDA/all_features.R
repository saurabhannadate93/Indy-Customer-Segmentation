library(dplyr)
library(tidyverse)
# load csv's with features
setwd("~/Desktop/MSiA421/hw1")
f1 <- read.csv("Variables_MH.csv",header=T)
# remove index column
f1 <- f1[,-1]
f2 <- read.csv("Variables_TF.csv",header=T)
# remove column associated with Other
drops <- c("Other","pct_Other")
f2 <- f2[, !(names(f2) %in% drops)]
f3 <- read.csv("Variables_SA.csv",header=T)
# remove index column
f3 <- f3[,-1]
# rename column
names(f3)[names(f3)=="tot_visit"] <- "log_tot_visit"
f4 <- read.csv("percent_by_referral_type.csv",header=T)
# rearrange f4
f4 <- f4%>%
  spread(referrer_type_value, percentbyref)
# fill NA with 0 - this subscriber didnt use that referrer_type
f4[is.na(f4)] <- 0
f5 <- read.csv("percentWeekendPerCustomer.csv",header=T)
# remove index column
f5 <- f5[,-1]

# merge all dataframes
features <- merge(f1,f2,by="fire_fly_id")
features <- merge(features, f3, by="fire_fly_id")
features <- merge(features, f4, by="fire_fly_id")
features <- merge(features, f5, by="fire_fly_id")

# save current version
write.csv(features,file="features_v2.csv",row.names=F)
