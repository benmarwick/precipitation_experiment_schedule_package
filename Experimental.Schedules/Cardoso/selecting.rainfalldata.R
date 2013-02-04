## select and edit rainfall data

rain <- read.csv("cardoso.rain.csv")

str(rain)

rain.noday <- rain[,-1]
head(rain.noday)

write.csv(rain.noday,file="Ppt.cardoso.csv",row.names=FALSE)
