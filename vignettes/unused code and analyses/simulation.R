
################################################################################
#########################the following section is based on #####################
#####################################SIMULATION#################################
################################################################################

## get some nbinom random numbers
test <- rnbinom(n=92,mu=9.159,size=0.6)

hist(test)

  ## how does the annual variation change under the negative binomial
  ## model?
  hist(replicate(200,sum(rnbinom(n=92,mu=9.159,size=2))))


## this is a function that simulates the experiment.

nbinom.water <- function(mu=9.159,size=0.6){
  day.from.start <- 1:365
  water.days <- seq(from=1,length.out=92,by=4)
  #precip <- data.frame(day.from.start,water=0,cum.water=0)
  test <- rnbinom(n=92,mu=mu,size=size)
  water <- rep(0,365)
  cum.water <- rep(0,365)
  
  water[day.from.start%in%water.days] <- test
  cum.water[day.from.start%in%water.days] <- cumsum(test)



  ## every day begins with evaporation, followed by a refill
  ## how much should evaporate?  lets start with a constant
  
  total.water <- rep(0,365)
  total.water[1] <- water[1]
  measure <- rep(0,365)

  for (i in 2:365){
    ##first evaporation
    x <- total.water[i-1]*0.98
    ##then measurment -- as if we measure every day!
    measure[i] <- x
    ## then watering, if any
    x <- x+water[i]
    ## then overflow?
    ## then save the value
    total.water[i] <- x
  }
  meas <- measure[which(day.from.start%in%water.days)]
#  browser()
  cbind(total=sum(water),
        mean=mean(meas)
        )
 ## total.water
  ##mean(meas)
##  list(meas=meas,total.water=total.water)
}

## this code works when "total water" is turned on.  it draws the
## water change over time
pdf("singlebrom.pdf")
par(mfrow=c(3,1))
lapply(c(0.3,0.6,1.2),
       function(X)
       plot(nbinom.water(size=X),type='l',
            xlab="day of year",
            ylab="volume(ml)",
            main=paste("mu = 9.159, size =",X)
            )
       )
dev.off()

## this function works when "mean(meas)" is turned on.  it shows how
## the mean amount of water changes as mu goes from 7 to 11

par(mfrow=c(2,3))
lapply(c(0.2,0.6,1,2,3)*9.159,function(y) {
  waters <- replicate(20,nbinom.water(mu=y))
  title <- paste("mu = ",y,sep="")
  hist(waters,col="lightblue",xlab="mean bromeliad water content",main=title)
}
)
dev.off()




### this section uses the parameters from Diane's suggestion

mus <- c(0.1,0.2,0.4,0.6,0.8,1,1.5,2.5,3)*9.159
ks <- c(0.5,1,2)*0.6

## simulate a list of matricies using Bolker's handy function
diane.param <- lapply(1:30,function(...) apply2d(nbinom.water,mus,ks))

## make them into data frames; add mu alongside each while you're at it.
diane.param <- lapply(diane.param,data.frame,mu=mus)

## make one long dataframe
diane.param <- do.call(rbind,diane.param)

## rename columns
names(diane.param)[1:3] <- ks

## use melt! which I love. to make k into a column
diane.param <- melt(diane.param,id.var="mu")

## more renaming
names(diane.param)[2:3] <- c("k","mean")

## unfortunately k is perceived as a factor.  convert to numeric with
## the orignal vector.  a test with cbind shows that this maps them
## correctly.
#diane.param[["k"]] <- ks[diane.param[["k"]]]
## ** TOOK IT OUT ** because it is even more convenient to make it a factor!

str(diane.param)

pdf("meanannual.pdf")
densityplot(~mean|k,groups=mu,data=diane.param,layout=c(1,3),auto.key=TRUE)
dev.off()




## this provides the confidence intervals.  though working with them
## is a hassle.
sapply(7:11,function(y) {
  waters <- replicate(20,nbinom.water(mu=y))
  t.test(waters)[["conf.int"]][1:2]
}
)


## and this handy thing gives us the means as a function of 
outs <- sapply(7:11,function(y) { replicate(20,nbinom.water(mu=y)) } )
##apply(outs,2,function(z) t.test(z)[["conf.int"]][1:2])
plot(7:11,colMeans(outs))

colMeans(outs)/7:11

#how is the mean variation related to the total amount of rain?  By
#drawing vertical lines through this graph, we can see the variation
#in average water caused merely by sequence.

pdf("totalmean.pdf")
plot(do.call(rbind,lapply(1:500,function(...) nbinom.water())))
abline(v=850,col='lightgreen')
dev.off()


## of course this is not the only variable that matters.  early insect
## communities might be strongly affected by drought at the beginning,
## as ignacio suggests.

## an alternative might be to let them begin full.  that would mena
## programming in a maximum volume.

## i think the question is fundamentally about the level of variation
## we want to study.  There is variation among bromelaids, among
## years, and among sites. I would argue that we need to think
## carefully about the level of variation that we choose to
## study. fitting parameters to a distribution seems like a good way
## to capture variation among dates.  but, because it is variable, it
## is yet another source of noise.  Any other points could be captured
## in other models - but always preferring simpler ones. i like these
## simulations as capturing all the variation possible - i.e. without
## any realism of bromeliad varation etc.


##################################################################################

## in a response to the above, Diane pointed out that "There is no
## need to randomly select rainfall events from the negative binomial
## distribution. I started doing that too, before realising that I was
## just unnecessarily adding variation. The prob distribution is
## identical to the proportion of events expected in each category, so
## we can use it to generate EXACT rainfall frequencies for each of
## our treatments - so our scenarios exactly fit the negative binomial
## distribution (no noise)."
