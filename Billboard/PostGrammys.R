require(reshape2)
require(ggplot2)

#make sure to run billboardfunc.R first

#create a data.frame for each artist
tswift<-billboardfunc("Taylor Swift")
kendrick <- billboardfunc("Kendrick Lamar")
alabamashakes <- billboardfunc("Alabama Shakes")
stapleton <-  billboardfunc("Chris Stapleton")
weeknd <-  billboardfunc("The Weeknd")

#inverts top 100 ranking
tswift$AUCcalc <- 101- tswift$Top100rank
alabamashakes$AUCcalc <- 101 - alabamashakes$Top100rank
stapleton$AUCcalc <- 101 - stapleton$Top100rank
weeknd$AUCcalc <- 101 - weeknd$Top100rank
kendrick$AUCcalc <- 101 - kendrick$Top100rank

#sum AUC for each week by artist
tagg <- aggregate(AUCcalc ~Date, data=tswift, sum)
kagg <- aggregate(AUCcalc ~ Date, data=kendrick, sum)
aagg <- aggregate(AUCcalc ~Date, data=alabamashakes, sum)
wagg <- aggregate(AUCcalc ~Date, data=weeknd, sum)
sagg <- aggregate(AUCcalc ~Date, data=stapleton, sum)

#combine into one data frame
mergedAUC <- merge(tagg, kagg, by.x="Date", by.y="Date", all=T)
mergedAUC <- merge(mergedAUC, wagg, by.x="Date", by.y="Date", all=T)
mergedAUC <- merge(mergedAUC, sagg, by.x="Date", by.y="Date", all=T)
colnames(mergedAUC) <- c("Date", "Taylor Swift", "Kendrick Lamar", "The Weeknd", "Chris Stapleton")

colSums(mergedAUC[,2:5], na.rm=T)

#Create long data for the purposes of graphing
grammymelt <- melt(mergedAUC, id.vars = "Date")

#this is the only way to replace NA with 0 to have it graph properly
temp.df <- data.frame(grammymelt)
temp.df[is.na(temp.df)] <-0
View(temp.df)
grammymelt <- temp.df
colnames(grammymelt)<- c("Date", "Artist", "value")
gg <- ( ggplot(grammymelt, aes(Date, value, color = Artist))
       + geom_line(size=1.1)
       + geom_ribbon(aes(ymin=0, ymax=value, fill=Artist, color = Artist), alpha=0.25) 
       + theme_bw()
       + theme(
         plot.margin = margin(20,10,20,20),
         plot.title = element_text(size = 18, face = "bold", margin = margin(0,0,20,0)),
         legend.title = element_text(size = 14, face = "bold"),
         axis.title.y = element_text(size = 14, face = "bold", margin = margin(0,20,0,0))
       )
       + ylim(0,510)
       + labs(list(
         title = "Grammy Arist Success on the Billboard Hot 100", x= NULL, y = "Total Weekly Point Value")
       )
)
gg