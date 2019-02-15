### DATA MINING HW 1 ###
library(dplyr)
library(lubridate)

## read data

indy <- read.csv("Indy.csv")
indy$event_date <- as.Date(indy$event_date, "%Y-%m-%d")

##
summary(indy$referrer_type_value)

## avg time between visit ##

## create data frame of id and date sorted desc by date within in id
visit_times <- indy %>%  select(., fire_fly_id, event_date) %>% group_by(fire_fly_id,event_date) %>%  summarise() %>%
   group_by(fire_fly_id) %>% arrange(fire_fly_id,desc(event_date)) 

# calculate time difference in days betweeen consecutive visits
visit_times$tdiff <- unlist(tapply(visit_times$event_date, INDEX = visit_times$fire_fly_id,
                                          FUN = function(x) c(0, `units<-`(-diff(x), "days"))))

# create lookup list of id and avg time between visits (excludes 0)
time_bw_visit_lookup <- visit_times %>% filter(., tdiff > 0) %>% group_by(fire_fly_id) %>%
  summarise(., avgTime = mean(tdiff)) %>% select(., fire_fly_id,avgTime)
time_bw_visit_lookup$avgTime <- log(time_bw_visit_lookup$avgTime+1)

## IGNORE ###
visits_per_user <- indy %>% select(., fire_fly_id,event_date) %>% group_by(fire_fly_id) %>% summarise(.,visits = n_distinct(event_date)) %>%
  select(., fire_fly_id, visits)

table(visits_per_user$visits)

time_bw_visit_lookup_wt <- inner_join(time_bw_visit_lookup, visits_per_user, by = "fire_fly_id") %>% mutate(., avgTimeWt = avgTime*visits)
### IGNORE ###

hist(time_bw_visit_lookup$avgTime)
hist(log(time_bw_visit_lookup$avgTime+1))

### RECENCY ####

# most recent date
maxDate <- max(indy$event_date)

# create lookup table of days between most recent visit and max date
recency_lookup <- indy %>% select(.,fire_fly_id,event_date) %>% group_by(fire_fly_id) %>% summarise(., most_recent = max(event_date)) %>%
  select(., fire_fly_id,most_recent) %>% mutate(., recency = difftime(maxDate,most_recent,units = "days")) %>%
  select(.,fire_fly_id,recency)

recency_lookup$recency <- as.numeric(recency_lookup$recency)
recency_lookup$recency <- sqrt(recency_lookup$recency) ### SQRT transformation

hist(recency_lookup$recency)
hist(log(recency_lookup$recency+1))
hist(sqrt(recency_lookup$recency))
hist((recency_lookup$recency)^(1/3))
summary(recency_lookup$recency)
sd(recency_lookup$recency)

## SECTIONS AND SUBSECTIONS PER VISIT ##


sections_lookup <- indy %>% group_by(fire_fly_id,event_date) %>% summarise(., sections = n_distinct(section)) %>%
  select(., fire_fly_id,event_date, sections) %>% group_by(fire_fly_id) %>% summarise(., secPerVisit = sum(sections)/n_distinct(event_date)) %>%
  select(., fire_fly_id, secPerVisit)


hist(sections_lookup$secPerVisit)
hist(log(sections_lookup$secPerVisit+1))
hist(sqrt(sections_lookup$secPerVisit))

sections_lookup$secPerVisit <- log(sections_lookup$secPerVisit+1)


##subsections_lookup <- indy %>% group_by(fire_fly_id,visit_num) %>% summarise(., subsections = n_distinct(sub_section)) %>%
  ##select(., fire_fly_id,visit_num, subsections) %>% group_by(fire_fly_id) %>% summarise(., subsecPerVisit = sum(subsections)/n_distinct(visit_num)) %>%
  ##select(., fire_fly_id, subsecPerVisit)


subsections_lookup <- indy %>% group_by(fire_fly_id) %>% summarise(., visits = n_distinct(event_date), subsections = sum(grepl(":",sub_section))) %>%
  select(., fire_fly_id,visits, subsections) %>% mutate(., subsecPerVisit = subsections/visits) %>% select(.,fire_fly_id,subsecPerVisit)


hist(subsections_lookup$subsecPerVisit)
hist(log(subsections_lookup$subsecPerVisit+1))

subsections_lookup$subsecPerVisit <- log(subsections_lookup$subsecPerVisit+1)


### CONTENT TYPE PER VISIT ###

content_lookup <- indy %>% group_by(fire_fly_id,event_date) %>% summarise(., content = n_distinct(content_type)) %>%
  select(., fire_fly_id,event_date, content) %>% group_by(fire_fly_id) %>% summarise(., contentPerVisit = sum(content)/n_distinct(event_date)) %>%
  select(., fire_fly_id, contentPerVisit)

hist(content_lookup$contentPerVisit)
hist(log(content_lookup$contentPerVisit)+1)

content_lookup$contentPerVisit <- log(content_lookup$contentPerVisit+1)



### CREATE VARIABLES CSV WITH NEW FEATURES

Variables <- inner_join(content_lookup,subsections_lookup, by = "fire_fly_id") %>% inner_join(.,sections_lookup, by = "fire_fly_id") %>%
  inner_join(., recency_lookup, by = "fire_fly_id") %>% left_join(., time_bw_visit_lookup, by = "fire_fly_id")
Variables$avgTime <- ifelse(is.na(Variables$avgTime),0, Variables$avgTime)


write.csv(Variables,"Variables_MH.csv")

