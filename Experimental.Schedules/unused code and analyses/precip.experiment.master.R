##################################################
##  This is the master R file
## written by Andrew MacDonald
## September 2012

## the code below contains several lines of testing and checking
## various functions, as well as graphs of their performance
## for a single, streamlined workflow see experimental.calculation.R

## calculations of the number of days with set amounts of
## precipitation in the rainfall experiment
## first source in our functions
source("precipitation.functions.R")

## required packages
library("bbmle")
##library("multicore")
## read in data
cr <- read.csv("Ppt_Pitilla.csv")

## testing the function sriv.integer

test <- sriv.integer(k=4)

## sriv.integer produces a list which includes the results and some
## variables for checking the relationship.

output <- test$output 

extra.water <- sum(sapply(output,wickwater)) # how much water did we regain?
extra.days <- length(sapply(output,wickwater)) # how many days were regained?
extra.water.amt <- sapply(output,wickwater) # the most important:
                                        # amounts per day
extra.water
extra.days
extra.water.amt
##

original.data <- test$original.data
head(original.data)
sum(original.data$integ.freqs)+extra.days  ## should be 60!
sum(original.data$amts*original.data$integ.freqs)+extra.water ## should be mu*60

## repeat water amounts by their frequency
water.amt <- rep(original.data$amts,original.data$integ.freqs)

## histogram of all the days - original and recovered
hist(c(water.amt,extra.water.amt),col='lightgreen',breaks=30)

sum(water.amt,extra.water.amt)

#################################################
## a wrapper function that will run wickwater on the output of
## sriv.integer, and then produce the designated Days!

par(mfrow=c(3,1))
new.data <- round(integerized(k=0.6))
lapply(c(1.5,0.9,0.2),
       hist(integerized(k=1.5),col='lightgreen',breaks=30)
)

integerized()
##################################################
## check that the mean no longer depends on both k and mu

hist(integerized(mean.dist=9.159,k=4),col='lightgreen',breaks=30)

xs <- seq(0.3,1.2,by=0.01)
par(mfrow=c(3,1))
plot(xs,sapply(xs,tot.water.round),xlab="value of k (mu=9.159)",pch=21,bg='black',main="with rounding")
abline(h=9.159*60)
plot(xs,sapply(xs,function(x)
  sum(integerized(k=x))),pch=21,bg="red",ylim=c(0,700),main="with algorithm, on rounded numbers")
abline(h=9.159*60)
plot(xs,sapply(xs,function(x)
  sum(integerized(k=x,round=FALSE))),pch=21,bg="blue",main="with algorithm, on floored numbers")
abline(h=9.159*60)


#################################################
## A followup function will then back-calculate the negative binomial
## parameters from that distribution.

head(cr.ppt)

## estimate the actual parameters

round(cr.ppt)
varname <- names(cr.ppt)


params.rainfall <- rowMeans(sapply(cr,nbin.estimate))

## used for testing the parameter estimates of the experimental data
new.data <- round(integerized(mean.dist=params.rainfall["a"],
                              k=params.rainfall["k"]))

hist(new.data)

#results for costa rica dataset:

meansd(cr$yr2006) #max meansd of 11 at around 18 d
meansd(cr$yr2007) #max meansd of 14 at around 10 d, then a slow climb to 17 by 60 d
meansd(cr$yr2008) #max meansd of 12 by 10 d, then a climb to 14 by 30 d, then 14-16 increase in last 6 days 
meansd(cr$yr2009) #max meansd of 12 by 10 d or 13 by 30 d, then slow climb to 15 
meansd(cr$yr2010) #max of 16 by 20 d, constant thereafter
meansd(cr$yr2011) #max of 24 by 20 d, constant thereafter

#averaged over all six years:
plot(rowMeans(sapply(cr, meansd2)))



#### important series of test illustrating how to reorder all
#### subsequent vectors based on the control oneÇ

## this "unsorted" vector represents a sequence that we wish to emulate
unsorted.test <- c(1:5,1)
## if it were to be in order, we would select these elements in this
## sequence from left to right:
order(unsorted.test)

# empty vector
new.vec <- numeric(0)
## we can reverse the above process:  take a vector that is *already*
## in order, and place each element (from left to right) in the
## positions of the original vector
new.vec[order(unsorted.test)] <- 6:11

## we can see that the relative ranks of each vector are closely related.
new.vec
unsorted.test
plot(rank(new.vec),rank(unsorted.test))



### when we place these into time-blocks, we must be careful to have
### no difference in mu or k between blocks.  Is this actually common?


best.sequence <- output$best.shuffle

## should you wish to find the best shuffle in the output matrix, it
## is here:
#which(apply(output[[3]],2,function(y) isTRUE(all.equal(y,output[[1]]))))


## the experimental treatments vary mu and k:
trts<-exp.trts()


grps<-replicate(500,sample(rep(letters[1:3],10)),simplify=FALSE)


aov.grp<-lapply(grps,function(g) aov(trts$mu~as.factor(g)))
Fs.mu <- sapply(aov.grp,function(obj) summary(obj)[[1]]["F value"][1,])
## extract p-values and summarize in a histogram for Diane.
Ps.mu <- sapply(aov.grp,function(obj) summary(obj)[[1]]["Pr(>F)"][1,])

aov.grp<-lapply(grps,function(g) aov(trts$k~as.factor(g)))
Fs.k <- sapply(aov.grp,function(obj) summary(obj)[[1]]["F value"][1,])

Ps.k <- sapply(aov.grp,function(obj) summary(obj)[[1]]["Pr(>F)"][1,])

pdf("500 random groups.pdf")
par(mfrow=c(2,2))
hist(Fs.mu,col="lightblue",main="F-stastistic (mu)")
lapply(qf(c(0.025,0.975),2,27),function(x) abline(v=x,col="red",lwd=2))
hist(Ps.mu,col="lightgreen",main="P-values (mu)")
abline(v=0.05,col="red",lwd=2)
hist(Fs.k,col="lightblue",main="F-stastistic (k)")
lapply(qf(c(0.025,0.975),2,27),function(x) abline(v=x,col="red",lwd=2))
hist(Ps.k,col="lightgreen",main="P-values (k)")
abline(v=0.05,col="red",lwd=2)
dev.off()





### to do:
## read in data, make sure all the lines in calculations do the right
## thing

## confirm that the mle2 method works for all precipitation data.
## hold the ppt_trial file in reserve.

## run meansd on integerized, and on the mean cr data above, then
## attempt the analysis as suggested earlier:

## first calculate the sum of squared differences to the 1:1 line (ie
## sum of square differences to the observed rainfall).  Try this for
## many parameter values.








head(cr)

pdf("rain.pdf")
par(mfrow=c(2,2))
sapply(cr[1:60,],barplot,main="")
barplot(shuffles[,winnah])
dev.off()


############3

Rprof()
x <- shuffler.fast(times=50)
Rprof(NULL)
summaryRprof()

start.timer()
shuffler.fast(times=50)
stop.timer()



start.timer()
output <- shuffler.fast.2.CV(times=50)
stop.timer()

barplot(output)


new.data <- round(integerized(mean.dist=9,k=0.7))
new.data <- round(integerized(mean.dist=13.2,k=0.5))

mean.patt <- rowMeans(mean.sd.fast(cr))

output <-
  shuffler.fast(times=50,derived.data=new.data,mean.pattern=mean.patt)

pdf("ss.method.pdf")
barplot(output[[1]])
plot(mean.patt,pch=21,bg='black',ylim=c(0,20))
points(output[[2]])
plot(output[[2]],mean.patt)
abline(a=0,b=1)
dev.off()




output[[3]]



cor(output[[3]][,1],y=mean.patt)



plot(rowMeans(outs),ylim=c(0,25))
matpoints(outs)


