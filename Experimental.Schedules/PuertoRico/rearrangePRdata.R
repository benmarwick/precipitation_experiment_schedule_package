## rearranging Puerto Rico data
## December 2013

library(reshape2)
library(lubridate)
library(plyr)

## read data
prdata <- read.csv("EVFSTowerMetDaily_2.csv")

## for each year, count Julian days

prdata <- ddply(prdata,.(YEAR),function(DF){
  ## make date object for first day
  jan1st <- paste0(unique(DF$YEAR),"-01-01")
  ## add julian days to this object (-1 because n days from Jan 1st)
  date <- ymd(jan1st)+days(DF$JULIAN-1)
  ## define interval for experiment
  exp_startdate <- ymd(paste0(unique(DF$YEAR),"-March-23"))
  ## interval of 60 days
  exp_interval <- new_interval(exp_startdate,exp_startdate+days(59))
  ## interval must be 60 days
  if (exp_interval/edays(1)!=59) stop("the interval is not 60 days!")
  in_experiment <- date%within%exp_interval
  cbind(DF,date,in_experiment)
})

## take only experimental days
pr_experimentdays <- subset(prdata,prdata$in_experiment)


## a quick check
ddply(pr_experimentdays,.(YEAR),summarize,
      start=min(JULIAN),
      isleap=leap_year(unique(YEAR))
      )


pr_experimentdays <- ddply(pr_experimentdays,.(YEAR),transform,ndays=seq_along(date))

pr_ppt <- dcast(pr_experimentdays,formula=ndays~YEAR,value.var="RAIN_MM")

## no NAs in years
nacol <- which(colSums(is.na(pr_ppt))>0)
pr_ppt <- pr_ppt[,-nacol]


write.csv(pr_ppt[,-1],"Ppt_PuertoRico.csv",row.names=FALSE)
