
scheduleread <- function(x){
  path <- file.path("../Experimental.Schedules",x)
  filepath <- list.files(path=path,pattern="schedule.csv",full.names=TRUE)
  read.csv(filepath,na.strings=c("NA","insects","sample","fill"))
}

analyzeNumbers <- function(x){
  numeric_names <- grepl(pattern=".[0-9]",x=names(x))
  mean_rain <- apply(X=x[,numeric_names],MARGIN=1,mean,na.rm=TRUE)
  data.frame(x[,!numeric_names],meanwater=mean_rain)
}

trt_summaries <- function(df){
  ddply(.data=df,.variables=.(trt.name),
        summarize,
        ## any summary statistic you like can go right here.
        meanrain=mean(value,na.rm=TRUE),
        n_zero=sum(value==0,na.rm=TRUE),
        n_overflow=sum(value>300,na.rm=TRUE)
  )
}
