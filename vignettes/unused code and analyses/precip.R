
## a demonstration of the problem of rounding on the precipitation data
## written by Andrew summer 2012

library(lattice)
library(emdbook)
library(reshape)


## a graph of the probability distribution of the negative binomial
## points are used because it is a discrete probability distribution
## (ie integers only)
curve(dnbinom(x,mu=9.159,size=0.3),xlim=c(0,100),type='p')
curve(dnbinom(x,mu=9.159,size=0.6),xlim=c(0,100),type='p',add=TRUE,col='blue')
curve(dnbinom(x,mu=9.159,size=1.2),xlim=c(0,100),type='p',add=TRUE,col='red')

## It seems like we use the probability density
## function to get our rainy days

distrib <-dnbinom(0:80,mu=9.159,size=1.2)*60
barplot(distrib)

## this is a graph of the 'frequency' of rainy days.  i.e. it shows
## how many days (on the y-axis) have that amount of rain (on the
## x-axis)

## so, the thing to do would be to parameterize the distribution
## function, then multiply it by the number of DAYS, not the number of
## ml

seq.amounts <- seq(0,80,by=1)

#amounts <- 0:80 # the amounts of rainfall per day
days <- dnbinom(seq.amounts,mu=9.159,size=0.6)*60 #the frequency of
                                        #days with that much rainfall.

par(mfrow=c(1,1))
barplot(days,names=seq.amounts,xlab="rainfall amount",ylab="n days")
abline(h=0.5,col='red') ## the "Dectection Number", below which
## amounts of rain will be rounded down.

# how many such days will there be?
seq.amounts[which(days<0.5)]

## how much water is added to the plants
sum(seq.amounts*days)
## which is close to
60*9.159

## how many days does it calculate for?
sum(tapply(days,cut(seq.amounts,seq(0,80,by=5),include.lowest=TRUE),sum))


#####
## of course, all the above has A PROBLEM : the number of days with
## each amount are not integers.

## the sticky thing is that the frequency of each amount is not a
## round number.  

sum(days) #59 
sum(round(days)) #54
## rounding decreases the number of days.  What are we supposed to do
## with the other days?
sum(ceiling(days)) #121 clearly not that.
sum(floor(days)) # 40 nor that

days

## using the probability function WITHOUT rounding, we
## can see that we get a constant amount of water added at all levels
## of 'clumping', ie across a range of 'size' params
tot.water <- function(x){
  amounts <- 0:500
  days <- dnbinom(amounts,mu=9.159,size=x)*60
  ##sum(rep(amounts,days))
  sum(amounts*days)
}


xs <- seq(0.3,1.2,by=0.01)
plot(xs,sapply(xs,tot.water),xlab="value of k (mu=9.159)")

## This shows that the amount of water curves a tiiiiny bit with k,
## but is actually very small

## NOTE THE MEAN TIMES THE DAYS IS THE TOTAL WATER


## but, if you repeat the process with rounding, you get this: an
## increasing amount of rain (total rain) as the clumping parameter
## increases.  that's not supposed to happen: its supposed to be the
## *mean* that does that.  and in fact it is, if you don't round:
tot.water.round <- function(x){
  amounts <- 0:500
  days <- round(dnbinom(amounts,mu=9.159,size=x)*60)
  ##sum(rep(amounts,days))
  sum(amounts*days)
}


xs <- seq(0.3,1.2,by=0.01)
plot(xs,sapply(xs,tot.water.round),xlab="value of k (mu=9.159)")


## see them both together
plot(xs,sapply(xs,tot.water),xlab="value of k (mu=9.159)",ylim=c(200,600))
points(xs,sapply(xs,tot.water.round),xlab="value of k (mu=9.159)")

## can we correct this by varying the number of days which we use to
## multiply, thus returning a correct amount of output days?
tot.water.round.days <- function(x){
  amounts <- 0:500
  days <- round(dnbinom(amounts,mu=9.159,size=0.6)*x)
  ##sum(rep(amounts,days))
  sum(days)
}


xs<-seq(60,100,by=5)
sapply(xs,tot.water.round.days)
## yes, but this seems ***sketchy***


## but how can we avoid rounding?  we cannot water for a fraction of a
## day.  the trouble here is that probabilites are continuous, but
## days are discrete.

## we could simulate -- this would give discrete days -- but that
## would introduce variation that we are keen to avoid.

## we could 'fudge' the probabilies a bit -- not unlike my broken
## stick idea a while ago -- to always sum to one:

idea <- dnbinom(0:100,mu=10,size=0.8) #vector of probabilities of
                                      #rainfall amounts
sum(idea) # almost 1 == but not quite!!
new.idea <- idea/sum(idea) #similar-looking vector
barplot(new.idea*60) #multiply by the number of days.
sum(new.idea) #sums to 1
sum(round(new.idea*60))

#always generates an error.


## or maybe what we need is something simpler.  calculate the 'target'
## amount of water (ie the amount that would fall if there were smooth
## frequencies for the rainfall event) and then calculate the
## difference between that number and the realized one after
## rounding.  Take that difference, which will be an integer number,
## and just do one extra day of the watering of that amount.

## this seems a bit arbitrary, on second thought.


## what about 'binning'?  i.e. not watering in 1 ml increments (an
## implausible thing anyways) and instead lumping things into the
## groups of 5 ml.

## keeps the number of days OK

days
seq.amounts
amts.bins <- cut(seq.amounts,c(-1,seq(0,80,by=5)),include.lowest=TRUE)
# I include 0 as a separate category, because I want to preserve
# rainless days
## cut is kinda nutty.  I had to tweak include.lowest and the lower
## bound before arriving at a distribution of cutpoints that would
## preserve dry days.

data.frame(table(amts.bins)) #using dataframe just to force it into
                             #readability

data.frame(amts.bins,seq.amounts)  ## how do the 'bins' match with the
## original amounts?  tweaked CUT until this worked right

data.frame(amts.bins,days)  ## but the real thing is to see how the
## number of days in each category matches with the new category

n.days <- tapply(days,amts.bins,sum)
data.frame(n.days)
length(n.days)
midpts <- c(0,seq(from=2.5,length.out=length(n.days)-1))  # include 0
                                        # for dry days, and otherwise
                                        # take the midpoints of all
                                        # the categories.
# you can see that the bins are now 
barplot(n.days)
barplot(days)

sum(days)
sum(n.days)

sum(round(days))
sum(round(n.days))

sum(n.days*midpts)
sum(days*seq.amounts)


sum(round(n.days)*midpts)
sum(round(days)*seq.amounts)


days


freqs <- round(tapply(days,cut(amounts,seq(0,80,by=5)),sum))
sum(seq(2.5,77.5,by=5)*freqs)
sum(freqs)


## BTW it is totally possible to make a histogram where the size of
## the bins changes.
x <- runif(1000, 0, 40)
hist(x,breaks=c(0, 1, 3, 6, 10,40),col='blue')


qnbinom(p=0.28,mu=9.159,size=0.2)

curve(cumsum(dnbinom(x,mu=9.159,size=0.93)),ylim=c(0,1),xlim=c(0,50))

cumsum(dnbinom(0:50,mu=9.159,size=0.93))


x <- 0:600
y <- dnbinom(x,mu=9,size=0.3)
print(cbind(x,y*60))



6*1.64+7*1.44
6.5*3


## Is there a difference between the prob density of a sequence and a
## binned sequence?

mean.dist <- 9.159
k <- 0.7
n.days <- 60

amt.seq <- seq(from=0,to=mean.dist*n.days,by=5)

freqs.amt <- dnbinom(amt.seq,mu=mean.dist,size=k)
plot(amt.seq,freqs.amt)

sum(freqs.amt) # is not a

f.rescale <- freqs.amt/sum(freqs.amt)

sum(f.rescale)

plot(amt.seq,round(f.rescale*60))


### complete sequence
amt.lin <- seq(from=0,to=mean.dist*n.days,by=1)
freq.amt.lin <- dnbinom(amt.lin,mu=mean.dist,size=k)
plot(amt.lin,freq.amt.lin*60)
# definitely smoother?

sriv.integer()

