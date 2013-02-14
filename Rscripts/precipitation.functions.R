################################################################################################################################################################
## functions used in the setup of the precipitation experiment
    
## written by Diane Srivastava and Andrew MacDonald

###########################################
## Method 1: "Srivastava's Integerization"


## the maximum likelihood method for estimating parameters from the
## negative binomial distribution
nbin.estimate <- function(ppt){
  model <-
    mle2(ppt~dnbinom(mu=mu,size=k),start=list(mu=9,k=0.7),data=list(ppt=round(ppt)))
  coef(model)
}


integerized <- function(...){
  ## run the algorithm
  data <- sriv.integer(...)
  ## save the output column
  output <- data$output
  ## save the original data, containing calculated and rounded freqs.
  original.data <- data$original.data
  ## repeat water amounts by their frequency
  water.amt <- rep(original.data$amts,original.data$integ.freqs)
  ## calculate the recovered water for each recovered day
  extra.water.amt <- sapply(output,wickwater)
  c(water.amt,extra.water.amt)
}


## called in 'integerized'
sriv.integer <- function(mean.dist=9.159,k=1.5,n.days=60){
  
  
  ## start with the vector of amounts of rainfall
  max.amt <- round(mean.dist*n.days)
  data <- data.frame(amts = 0:max.amt)
  
  ## calculate the probabilities.  I mean the *frequencies*
  data$freqs <- dnbinom(data$amts,mu=mean.dist,size=k)

  ## rescale the infinite distribution to be defined only on our
  ## little interval
  data$freqs <- data$freqs/sum(data$freqs)

  ## finally, transform the probabilites into frequencies
  data$freqs <- data$freqs*n.days

  ## round all the frequencies to integers
  data$integ.freqs <- round(data$freqs)        

  ## find the difference to get the 'wicks' on the candles
  data$wicks <- data$freqs-data$integ.freqs

  ## save the data in a separate object
  data2 <- data
  
  output <- list(NULL)
  misses <- numeric(0)
  threshs <- numeric(0)
  z <- 1
  ## this was tricky.  I have defined the loop to run while the sum of
  ## the remaining 'wicks' is greater than a very small number.  This
  ## is because, since the rounding is not perfect, the computer will
  ## try to carry out the operation on the very last few datapoints.
  ## It might be best to increase this 'stopping rule', if we find
  ## that for other parameters the algorithm fails.
  while(sum(data$wicks)>1e-10){
    #browser()
    sumwick <- cumsum(data$wicks)
    
    ## which valueS of this sum are no more than 2 abd **greater**
    ## than 1??
    ## Again, the value "very slightly greater than 1" is necessary,
    ## because the normal algorithm will continue to try to add too
    ## many values together.  They come out looking like "1" for
    ## which(sumwick>1), but they are actually increasing by a very
    ## slight amount.  basically, it defines a stopping point, again.
    ## If the sum of all the extras is only juuuust greater than 1 (in
    ## other words, if we have basically accounted for all the days)
    ## then we should just lump all the remaining days together in the
    ## final 'day'.
    ifelse(sum(data$wicks)>1.00001,
           threshold <- min(which(sumwick>1)),
           threshold <- length(data$wicks)
           )
    ## we use "min" because the
    ## minute it goes over, you've
    ## passed 1
      
    ## subtract from ONE the previous cumsum, to get the amount lacking
    ## from one.
    missing <- 1-sumwick[threshold-1]
    
    ## remove rows from *data* into *output*.
    output[[z]] <- data[1:threshold,]
    ## replace the *wicks* value in *output/data* into the value of
    ## missing
    output[[z]][threshold,"wicks"] <- missing
    
    
    ## take this much from threshold.  the threshold value now changes in *data*
    prop.remaining <- data[threshold,"wicks"]-missing
    ## is the same quantity as sumwick[threshold]-1
    ## this gets passed to the **next** group
    data[threshold,"wicks"] <- prop.remaining
    
    ## then you remove all those rows that were in the previous (minus the
    ## 'threshold' row, because we still need that)
    
    data <- data[-c(1:(threshold-1)),]
    threshs[z] <- threshold
    misses[z] <- missing
    z <- z+1
  }
  return(list(output=output,
              original.data=data2,
              thresholds=threshs,
              misses=misses))
}


## called in 'integerized'

wickwater <- function(dat){
  sum(dat["amts"]*dat["wicks"])
}


## this function returns the mean-sd as a function of window size.
## IT WORKS ON A LIST or, of course, a dataframe
mean.sd.fast <- function(rainfall.vector,all.windows=wins){


  ## go over all the shuffles and re-order them according to the
  ## windows
  list.window.shuffles <- lapply(rainfall.vector,rapply.windows,ss.list=all.windows)
  ## is a complicated list: each element represents the transformation
  ## of a single shuffle into little windowed chunks

  ## rapply into this list the **sd** function
  test <- rapply(list.window.shuffles,sd,how='list')

  ## the resulting list has three levels.  reduce this to two by
  ## unlisting the bottom level
  test2 <- lapply(test,function(z) lapply(z,unlist))

  ## using rapply, add the **mean** function.  reduce one more level
  ## of the list
  sapply(rapply(test2,mean,how='list'),unlist)
}



## This function quickly generates all the necessary 'windows', ie the
## subscripts of a rainfall vector which will be used to calculate 
windowmaker.l <- function(window.size,size=60){
  windows <- lapply(0:(size-1),function(w) (1:window.size)+w )
  ##browser()
  ## is there a 60 in any spot (other than the last!)
 fell.off <- sapply(windows,function(z) 60%in%z[1:length(z)-1])
  windows[!fell.off] #not the ones which fell off
}


## this function goes down all the all.windows subscripts, replacing
## them with the values from ***a single*** year of data
rapply.windows <- function(one.year,ss.list=all.windows){
  rapply(ss.list,f=function(b) one.year[b],how="list")
}


## produces a list as long as 'times' (columns)
## each element represents a shuffle of the dataset
## then we search over all of them to find the best one
shuffler.fast <- function(times,derived.data,mean.pattern,all.windows=wins) {
  #browser()
  ## max
  bigday <- max(derived.data)
  ## everything but the max
  derived.data2 <- derived.data[-which(derived.data==bigday)]
  shuffles <- replicate(times,c(bigday,sample(derived.data2)),simplify=FALSE)
  ## shuffles, a list of permutations of all the derived data
  ## put the max back
  
  shuffle.patterns <-  mean.sd.fast(shuffles,all.windows=all.windows)
  ## note that mean.sd.fast relies on the window sizes being already
  ## calculated and saved in an object called 'wins' (see
  ## "experimental calculation").  This saves time since it needs to
  ## be done only once.

  ## apply a function to the shuffles which gets us the 'best'
  criterion <- apply(shuffle.patterns,2,
                     function(z)
                     #cor(z,y=mean.patt)
                     sum((z-mean.pattern)^2)
                     )

  decent.ones <- which(criterion<quantile(criterion,probs=0.05))
                       
  winnah <- which.min(criterion)

  ## which of the 'decent.ones' has the biggest day closest to the start?
  big.start <- min(sapply(decent.ones,which.max))

  list(best.shuffle=shuffles[[winnah]],
       best.shuffle.pattern=shuffle.patterns[,winnah],
       bigstart.shuffle=shuffles[[big.start]],
       bigstart.shuffle.pattern=shuffle.patterns[,big.start],
       decent.shuffles=shuffles[decent.ones],
       all.shuffle.patterns=shuffle.patterns,
       crit=criterion,
       all.shuffles=do.call(cbind,shuffles),
       mean.pattern=mean.pattern
       )

}


### this function and its dependent variables are the only correct way
### to reorder the variables.
orderize <- function(z,new=order(best.sequence)){
	#browser()
  x <- numeric(length=60)
  x[new] <- sort(z)
  x 
}


## calculate the actual schedule
## produces a list with the schedule, the parameters and the water
## amounts of the different treatments, for use by graph.print
exp.trts <- function(params.rainfall,best.sequence){
  ## first make the vector of shifts
  ## then multiply them!  makes all the steps easier
  mu.shift <- c(0.1,0.2,0.4,0.6,0.8,1,1.5,2,2.5,3)
  k.shift <- c(0.5,1,2)
  param.space <- expand.grid(mu=mu.shift,k=k.shift)
  trt.name<-apply(param.space,1,function(param.vec){
    param.vec <- param.vec
    paste("mu",param.vec["mu"],"k",param.vec["k"],sep="")
  }
                  )
  param.space["mu"]<-param.space["mu"]*params.rainfall["mu"]
  param.space["k"]<-param.space["k"]*params.rainfall["k"]
  #browser()

  param.space.list <- split(param.space,list(1:nrow(param.space)))

  treatments <- lapply(param.space.list,FUN=function(param.vec)
                      integerized(
                        mean.dist=param.vec[["mu"]],
                        k=param.vec[["k"]]
                        )
                      )
  
  ## some of these are too long!  it seems to be the result of a weird
  ## quirk in the rounding when there is very high mu and very small k
  
  too.long <- which(sapply(treatments,length)>60)
  for(k in too.long){
    zero <- max(which(treatments[[k]]==0))
    treatments[[k]] <- treatments[[k]][-zero]
  }                    
  ## 'orderize' reorders all treatments according to best.sequence
  new.ord <- order(best.sequence)
  new.treatments <- lapply(treatments,function(y) orderize(z=y,new=new.ord))
  
  id.test<-identical(sapply(new.treatments,sum),sapply(treatments,sum))

  ## combine the treatments
  precip.amt <- do.call(rbind,new.treatments)
  precip.amt.round <- round(precip.amt,digits=2)
  ## generate a couple of random groups -- temporal groups of 3 days
  grps<-replicate(25,sample(rep(letters[1:3],10)),simplify=FALSE)
  ## ANOVA all of them for differences in mu and k
  aov.grp.mu<- lapply(grps,function(g) aov(param.space[["mu"]]~as.factor(g)))
  aov.grp.k <- lapply(grps,function(g) aov(param.space[["k"]]~as.factor(g)))
  
  Ps.mu <- sapply(aov.grp.mu,function(obj) summary(obj)[[1]]["Pr(>F)"][1,])
  Ps.k <- sapply(aov.grp.k,function(obj)
  summary(obj)[[1]]["Pr(>F)"][1,])

  ## pick one of them which has no sig. diff. in either.
  ## the first one is fine (since they are based on random shuffles
  ## their order is meaningless)
  grp <- grps[[min(which(Ps.mu>0.05&Ps.k>0.05))]]

  ## treatment medians
  trt.med <- apply(precip.amt,1,median)
  ## overall median
  overall <- trt.med[which(trt.name=="mu1k1")]
  ## one day grace
  preday <- matrix(data="sample",nrow=30,ncol=1)
  postday1 <- matrix(data="sample",nrow=30,ncol=1)
  postday2 <- matrix(data=NA,nrow=30,ncol=1)
  destruct.samp <-  matrix(data="insects",nrow=30,ncol=1)
  ## NOTE that precip.amt.round contains ALL THE NUMBERS prescribed by
  ## the distribution's parameters.  All other numbers are added to
  ## standardize experimental units (treatment medians, overall
  ## medians, etc)
  ## stick 'em all together
  new.trt <- cbind(overall,preday,precip.amt.round,
                   trt.med,postday1,postday2,destruct.samp)
  ## get rid of unwanted names
  dimnames(new.trt)[[2]] <- NULL

  ## a lookup list that transforms groups to numbers of days to shift
  ## in time
  lkup <- list(a=0,b=1,c=2)
  #look 'em up:
  shifts <- sapply(grp,function(s) lkup[[s]])
  ##PROBLEM
#browser()
  ## new.trt has both NA and numerical, so this is everything:
  ss <-  matrix(c(row(new.trt), col(new.trt)), ncol = 2)
  ## make a new matrix to put the shifted numbers in.
  ## use NA (default), since we want to separate days that have no
  ## treatment from those with zeros:
  final <- matrix(nrow=30,ncol=ncol(new.trt)+2)
  ## subscripts in this format are 1:nrow (first column) 65 times, and repeated
  ## numbers (second column: 30 1s, then 30 2s, etc) -- these
  ## represent the columns.  so
  ## we can repeat the shifts 65 times:
  ss.f <- ss
  ss.f[,2] <- ss[,2]+rep(shifts,ncol(new.trt))
  final[ss.f] <- new.trt[ss]

  ##too many digits in the estimated numbers! TMI
  param.space<-round(param.space,digits=2)

  ## now might be a convenient place for diagnostics, before bound in
  ## dataframe

  ## because of the addition of the NA days, and also the median
  ## treatments at the start and the end, before re-calculating the
  ## parameters we must excise all NAs and also remove the start and
  ## end numbers (the medians)
  act.param <- apply(precip.amt.round,1,function(x){
    x2 <- x[!is.na(x)]
    #x2[-c(1,length(x2))]
    nbin.estimate(x2[-c(1,length(x2))])
  }
                     )

  act.param <- data.frame(t(act.param))
  names(act.param) <- c("experiment.mu","experiment.k")

  names(param.space) <- c("intended.mu","intended.k")

  ## are the realized parameters close to the intended ones?
  parameters <- cbind(param.space,act.param)

  ## are the total water amounts close to the intended ones?
  water.amt <- cbind(experiment=apply(precip.amt.round,1,
                       function(x){
                         x2 <- x[!is.na(x)]
                         sum(x2[-c(1,length(x2))])
                       }
                       ),
                     intended=param.space[["intended.mu"]]*60
                     )
  ## 60 not na days
  ## total water = n.days*mu
  ## parameter restimates should be accurate
  
  sched <- cbind(trt.name,param.space,temporal.block=grp,final)

  list(schedule=sched,parameters=parameters,water.amt=water.amt)
}

## the wrapper function that calculates the precipitation amounts and
## orders them as best we can.
rainfall <- function(site,Times=50){
  ## site = the name of the folder in which the data is stored (not
  ## the directory, as the code will figure that out.
  ## Times = the number of permutations of rainfall days to be
  ## simulated.  The 'best' pemutation is selected from among them.

  ## build path name
  datapath <- file.path("../Experimental.Schedules/",site)

  ## you have to have the data in there first
  if(file.exists(datapath)==FALSE)
    stop("There is no folder with that name!")
  
  fieldsite.dir <- datapath

  ## get all the .csv files from that directory, and check that one
  ## starts with "Ppt"
  csvs <- list.files(pattern="*.csv",path=fieldsite.dir)
  rainfall.file <- pmatch("Ppt",csvs,nomatch=NA)
  if(length(rainfall.file)!=1)
    stop("make sure there is one and only one file with 'Ppt' in the name")
  ppt.file <- file.path(fieldsite.dir,csvs[rainfall.file])

  ## read in the data
  rainfall.data <- read.csv(file=ppt.file)
  
  ## add a stop line if less than 60!  or just a message li

  if(nrow(rainfall.data)!=60)
    stop("wrong number of rows in the input data!")
  
  ## estimate parameters for the negative binomial distribution for each
  ## year **independently**, then take the mean of all these.
  yearly.params <- sapply(rainfall.data[1:60,],nbin.estimate)
  params.rainfall <- rowMeans(yearly.params)
  
  ## use these average parameter estimates to calculate the 'new data',
  ## derived from the probability density function
  new.data <- integerized(mean.dist=params.rainfall["mu"],
                          k=params.rainfall["k"])
  ## Just a reminder: intergerized() produces a vector of 60 numbers,
  ## giving the 'integerized' numbers from the negative binomial
  ## distribution

  ## new.data is the control treatment 60 numbers, i.e. the ones which
  ## will be used to approximate the temporal pattern.

  ## ########################################
  ## TEMPORAL PATTERN APPROXIMATION
  ## ########################################
  
  ## calculate the window sizes -- only necessary once!
  wins <- lapply(2:59,function(z) windowmaker.l(window.size=z))
  ## a list 58 long -- the number of window sizes.
  ## each element is itself a list, giving the subscripts of all
  ## windows of that length
  ## the average pattern between the window size and the average
  ## standard deviation for the rainfall data.
  data.patterns <- mean.sd.fast(rainfall.vector=rainfall.data,
                                all.windows=wins)
  mean.patt <- rowMeans(data.patterns)
  ## the shuffler function permutes the derived data until it gets one
  ## that is close to the mean pattern.
  output <-shuffler.fast(times=Times,derived.data=new.data,
                         mean.pattern=mean.patt,
                         all.windows=wins)

  ## should you wish to find the best shuffle in the output matrix, it
  ## is here:
  ##which(apply(output[[3]],2,function(y)
  ##isTRUE(all.equal(y,output[[1]]))))
  ##
  out <- list(param=params.rainfall,         #average rainfall parameters
              yearly=yearly.params,          #parameters for each year
              data.patterns=data.patterns,   #patterns of sd~mean per yr
              shuffled=output)               #all shuffles generated.
  ## the 'output' object is a list.  it contains all the information
  ## about the permutations of the rainfall schedule.

  ## save the output in a file named with the computer that generated
  ## it !  this is useful for identifying the school's computer from
  ## my home one.
  out.file <- paste(site,"sim",Sys.info()[["nodename"]],"Rdata",sep='.')
  save(out,file=file.path(fieldsite.dir,out.file))
}
  

## this function generates graphs and the acutal schedule file itself
graph.print <- function(sim.data,site){
  ## sim.data is the name of the data file on disk
  ## site is the name of the folder that has data for that site.
  datapath <- file.path("../Experimental.Schedules",site)
  ## read in simulation data
  load(file.path(datapath,sim.data))
  
  ## extract the 'best' shuffle.  Also take the parameters.
  best.shuff <- out[["shuffled"]][["best.shuffle"]]
  parameters <- out[["param"]]

  ## exp.trts() is a big function for actually creating the schedule.
  ## it needs only the best shuffle and the parameters to be manipulated
  trts <- exp.trts(best.sequence=best.shuff,params.rainfall=parameters)
  
  ## Print the schedule:
  schedname <- paste(datapath,"/",site,"schedule.csv",sep="")
  write.csv(trts[["schedule"]],file=schedname,row.names=FALSE)

  ## the schedule goes in the main directory for this location.  the
  ## following diagnostic plots go in their own special subdirectory:

  diagnostic.dir <- file.path(datapath,"/diagnostics")
  dir.create(diagnostic.dir)
  
  ## graph all the schedules
  ## clever trick -- reading it in and calling the NA strings "NA",
  ## "sample" and "insects"
 
  rain.data <- read.csv(schedname,na.strings=c("NA","sample","insects"))

  pdf(file.path(diagnostic.dir,"treatments.over.time.pdf"))
  ymax <- max(rain.data[,paste("X",as.character(1:68),sep='')],na.rm=TRUE)
  for(i in 1:dim(rain.data)[1]){
    plot(unlist(rain.data[i,paste("X",as.character(1:68),sep='')]),
         ylab="rainfall",
         main=rain.data[i,"trt.name"],
         xlim=c(0,68),
         type='b',
         ylim=c(0,ymax)
         )
  }
  dev.off()
  
  
  ## a plot of the rainfall amounts, divided by k
  ## divide the dataframe by levels of k
  list.sched <-
    split(rain.data[,paste("X",as.character(1:68),sep='')],rain.data["intended.k"])

  ## plotting function
  plotr <- function(dat){
    plot(seq(1,ymax,length.out=62),type='n',ylim=c(0,ymax),ylab="rainfall")
    apply(dat,1,lines)
  }
  ## precipitation over time
  pdf(file.path(diagnostic.dir,"precip.time.pdf"))
  layout(matrix(1:3,nrow=3),widths=c(1),respect=TRUE)
  par(mar=rep(1,4))
  lapply(list.sched,plotr)
  dev.off()
  
  ## diagnostics
  
  pdf(file.path(diagnostic.dir,"param.result.pdf"))
  par(mfrow=c(1,2))
  with(trts[["parameters"]],plot(intended.mu,experiment.mu,main="mu"))
  abline(0,1)
  with(trts[["parameters"]],plot(intended.k,experiment.k,main="k"))
  abline(0,1)
  dev.off()
  
  pdf(file.path(diagnostic.dir,"water.amt.pdf"))
  with(data.frame(trts[["water.amt"]]),plot(intended,experiment,main="water"))
  abline(0,1)
  dev.off()
  
  ## fit of best.shuffle to data
  pdf(file.path(diagnostic.dir,"meansd.comp.pdf"))
  maxnum <- max(out[["shuffled"]][["best.shuffle.pattern"]],
                out[["shuffled"]][["mean.pattern"]])
  plot(out[["shuffled"]][["best.shuffle.pattern"]],pch=21,bg="white",
       ylab="mean sd",xlab="window size",
       main="black is from actual data",ylim=c(0,maxnum)
       )
  points(out[["shuffled"]][["mean.pattern"]],pch=21,bg="black")
  dev.off()


  ## fit of best.shuffle to data -- ALL DATA
  pdf(file.path(diagnostic.dir,"meansd.comp.all.data.pdf"))
  maxnum <- max(out[["shuffled"]][["best.shuffle.pattern"]],
                out[["shuffled"]][["mean.pattern"]],
                out[["data.patterns"]])
  plot(out[["shuffled"]][["best.shuffle.pattern"]],pch=21,bg="white",
       ylab="mean sd",xlab="window size",
       main="black is from actual data \n circles are mean",ylim=c(0,maxnum)
       )
  points(out[["shuffled"]][["mean.pattern"]],pch=21,bg="black")
  apply(out[["data.patterns"]],2,points,pch=23,bg="black")
  dev.off()

  readmename <- paste(datapath,"/",site,"readme.txt",sep="")
  nshuff <- dim(out[["shuffled"]][["all.shuffle.patterns"]])[2]
  
  sink(file=readmename)
  print(paste("readme file for the experimental schedule from",site))
  print(paste("calculated on",Sys.Date()))
  print("parameter estimates by year")
  print(out[["yearly"]])
  print(paste("it was selected from",nshuff,"simulations"))
  print("further diagnostics can be found in /diagnostics")
  sink()

  
}


site.parameter.comparison <- function() {
    calculated.sites <- list(CostaRica = "./CostaRica", FrenchGuiana = "./FrenchGuiana", 
        Cardoso = "./Cardoso",Colombia="./Colombia")
    scheds <- lapply(calculated.sites, function(x) list.files(x, 
        pattern = "*schedule.csv", full.names = TRUE))
    scheds <- lapply(scheds, read.csv)
    paramplot <- function(parm = "intended.mu") {
        vals <- lapply(scheds, function(z) z[[parm]])
        values <- lapply(vals, unique)
        rainest <- max(unlist(values))
        length(values[[1]])
        plot(c(1, length(values[[1]])), c(0, rainest), type = "n", 
            bty = "l", xlab = "rank of value", xaxt = "n", ylab = "value of parameter", 
            main = "comparison of parameters among CR and FG \n black  = CR, blue = Cardoso,red=Colombia")
        lapply(names(values), function(z) points(values[[z]], 
            pch = 21, bg = c("black", "white", "blue","red")[which(names(values) == 
                z)]))
        lapply(values, points)
    }
    pdf("parameter.comparison.pdf", height = 4)
    layout(matrix(1:2, nrow = 1))
    par(cex = 0.6)
    lapply(c("intended.mu", "intended.k"), function(y) paramplot(parm = y))
    dev.off()
}
