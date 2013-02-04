#code to examine precipitation frequency in oct, nov dec 2006 data
#see octdec2006.csv in work>brom>weather
cr<-read.table(file.choose(),sep=",", header=TRUE, na.strings="NA", fill=TRUE)

head(cr)

mean(cr$ppt)
sd(cr$ppt)
var(cr$ppt)
hist(cr$ppt, breaks=14)

p5<-sum(cr$freq1<-(cr$ppt>=0&cr$ppt<=10)*1)/length(cr$ppt)
p15<-sum(cr$freq1<-(cr$ppt>10&cr$ppt<=20)*1)/length(cr$ppt)
p25<-sum(cr$freq1<-(cr$ppt>20&cr$ppt<=30)*1)/length(cr$ppt)
p35<-sum(cr$freq1<-(cr$ppt>30&cr$ppt<=40)*1)/length(cr$ppt)
p65<-sum(cr$freq1<-(cr$ppt>60&cr$ppt<=70)*1)/length(cr$ppt)

#10.2 mm of rain a day on average
p5*5+p15*15+p25*25+p35*35+p65*65

#reduce all prob >0 by 50% to reduce mean by 50% by decreasing freq of ppt
(p5*5+p15*15+p25*25+p35*35+p65*65)/2

#increase ppt in each probability bin, this does not affect freq of zeroes
p5*10+p15*30+p25*50+p35*70+p65*130

#is it neg binomial? rough est of k is 0.90

mean(cr$ppt)^2/(var(cr$ppt) - mean(cr$ppt))


##make data into frequencies
cr$round.ppt<-round(cr$ppt)
freq<-table(cr$round.ppt)

#correct freq data
omits<-c(24,25,26,27,28,29)
best.freq<-c(freq[-omits],0,1,0,1,0,0,2,0,0,1,0,0,1,rep(0,25),1)

kfit(best.freq)
# better k est is 0.74 

#test for neg binomial from p 256 crawley

k.ppt<-kfit(best.freq)



nb<-length(cr$ppt)*(1+mean(cr$ppt)/k.ppt)^(-k.ppt)*factorial(k.ppt+(0:(length(best.freq)-1))-1)/(factorial(0:(length(best.freq)-1))*factorial(k.ppt-1))*(mean(cr$ppt)/(mean(cr$ppt)+k.ppt))^(0:(length(best.freq)-1))

both<-numeric(length(best.freq)*2)
both[1:26%%2!=0]<-best.freq
both[1:26%%2==0]<-nb
barplot(both,col=rep(c(1,0),13),ylab="frequency")

sum(((best.freq-nb)^2/nb)[nb>5])
sum(nb>5)
qchisq(0.95,sum(nb>5)-2-1)
qchisq(0.95,4)

#so sig diff from neg binomial, but this is prob as good as we'll get

k.ppt<-0.6
# hey, rerun with k=0.6 -iteratively determined- and the fit is better! 
#now it is borderline sig

  xf<-sapply(0:60,function(i) negbin(i,0.9159,0.6))
  barplot(xf, names=as.character(0:60),xlab="count",ylab="probability density")

#this seems to work!
hist(rnbinom(92,mu=9.159, size=0.6), breaks=14)
hist(cr$ppt, breaks=14)
hist(rnbinom(92,mu=9.159, size=0.74), breaks=14)

hist(rnbinom(92,mu=9.159, size=0.3), breaks=14)
hist(cr$ppt, breaks=14)

hist(rnbinom(92,mu=9.159, size=1.2), breaks=14)
hist(cr$ppt, breaks=14)

hist(rnbinom(92,mu=27.477, size=0.6), breaks=14)
hist(rnbinom(92,mu=27.477, size=0.3), breaks=14)
hist(rnbinom(92,mu=27.477, size=1.2), breaks=14)

hist(rnbinom(92,mu=1, size=0.6), breaks=14)
hist(rnbinom(92,mu=1, size=0.3), breaks=14)
hist(rnbinom(92,mu=1, size=1.2), breaks=14)

