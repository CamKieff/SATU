require(ggplot2)

top100<- read.csv("billboardhot100.csv", header = T)

#convert dates to dates
top100$Date <- as.Date(top100$Date, "%m/%d/%Y")

#converts a wildcard expression into a regular expression. Only selects songs where Tswift is the main artist
grx <- glob2rx("Taylor Swift*")

#subsets top100 for Taylor Swift songs
tswift <- top100[grep(grx, top100$Top100artist), ]

#sorts by date and then by song name (the date might be redundant)
tswiftsort <- tswift[order(tswift$Date), ]
tswiftsort <- tswiftsort[order(tswiftsort$Top100song), ]

#This has something to do with getting it to graph properly.
id.table<-table(tswiftsort$Top100song)
tswiftlimit<-subset(tswiftsort, Top100song %in% names(id.table[id.table >= 3]))

#change the row names to their proper numbers.  I think this is important for getting the NA row generator to work.
row.names(tswiftlimit)<-seq(1,nrow(tswiftlimit),1)

#create new data frame to bind rows to
tswiftlimit.expand<-data.frame()

#Binds each row to the empty data frame and generates a row if there should be a gap
for(r in 1:nrow(tswiftlimit)){
  
  tswiftlimit.expand <- rbind(tswiftlimit.expand, tswiftlimit[r,]) #binds normal rows
  
  timediff<-as.numeric(tswiftlimit$Date[r+1]-tswiftlimit$Date[r]) #calculates the difference in time between two adjacent rows
  
  #if there is more than one week between rows, bind a blank row in between
  if(timediff > 8){
    tswiftlimit.expand <- rbind(tswiftlimit.expand, c(as.character(tswiftlimit$Top100song[r]), NA, NA, NA, NA, NA))
  }
}

#copies the date from the previous row and adds one day to rows with NA as the date
for(r in 1:nrow(tswiftlimit.expand)){

  if(is.na(tswiftlimit.expand$Date[r])){
    tswiftlimit.expand$Date[r] <- as.Date(tswiftlimit.expand$Date[r-1]+1)
  }
}

#change column name to properly name legend title
colnames(tswiftlimit.expand)[1]<- "Song"

#converts rank to a number for plotting purposes
tswiftlimit.expand$Top100rank<-as.numeric(tswiftlimit.expand$Top100rank)

# Selects songs from the 1989 album only based on its release date
NEN<-subset(tswiftlimit.expand, Date > "2014-09-01")

#plotting portion
p0<-(ggplot(NEN, aes(x = Date, y = Top100rank, color = Song)) 
     + geom_point() 
     + geom_line() 
     + scale_y_reverse(name = "Top 100 Ranking", breaks = c(1, seq(10, 100, 10))) 
     + scale_x_date(name = element_blank(), date_breaks = "2 months", date_labels = "%b%y")
     + labs(list(title = "Evolution of Taylor Swift's 1989", legend = "Song Title"))
     + theme(plot.title = element_text(size = 22, face = "bold", margin=margin(0,0,15,0)), 
             legend.title = element_text(size = 14, face = "bold"),
             axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,20,0,0)), 
             axis.text = element_text(size = 14),
             plot.margin = margin(20,10,20,20)
             )
)

p0

#calculate AUC
NEN$AUCcalc <- 101 - NEN$Top100rank
Tswiftagg <- aggregate(AUCcalc ~ Song, data = NEN, sum)
Tswiftagg$Views <- c(736426012, 1437592992, NA, 44872910, 1299792051, 374772444, NA, 354629255, NA, NA) #as of 2/1/2016
p2 <- (ggplot(data=Tswiftagg)
     + geom_point(aes(x = Views, y = AUCcalc))
      )

p2

#plot of TSwifts 1989 formatted for fucking tumblr with strange seetings
p3<-(ggplot(NEN, aes(x = Date, y = Top100rank, color = Song)) 
     + geom_point(size=4) 
     + geom_line() 
     + scale_y_reverse(name = "Top 100 Ranking", breaks = c(1, seq(10, 100, 10))) 
     + scale_x_date(name = element_blank(), date_breaks = "2 months", date_labels = "%b%y")
     + labs(list(title = "Evolution of Taylor Swift's 1989", legend = "Song Title"))
     + theme(plot.title = element_text(size = 40, face = "bold", margin=margin(0,0,15,0)),
             legend.text = element_text(size=26),
             legend.title = element_text(size = 26, face = "bold"),
             axis.title.y = element_text(size = 26, face = "bold", margin=margin(0,20,0,0)), 
             axis.text = element_text(size = 26),
             plot.margin = margin(20,10,20,20)
     )
)

p3