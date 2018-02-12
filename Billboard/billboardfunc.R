require(ggplot2)

top100 <- read.csv("GitHub/SATU/Billboard/billboardhot100.csv", header = T)

top100$Date <-
  as.Date(top100$Date, "%m/%d/%Y") #format dates as dates

billboardfunc <-
  function(artistname, limitdate = "2014-09-01", countlimit = 1) {
    #converts a wildcard expression into a regular expression. Only selects songs where artist name is the main artist
    grx <- glob2rx(paste0(artistname, "*"))
    
    #subsets top100 for artist songs to create an initial dataframe
    dfinit <- top100[grep(grx, top100[,3]),]
    
    dfinit <- dfinit[order(dfinit$Date),] #sort by date (redundant?)
    dfinit <- dfinit[order(dfinit[, 1]),] #sort by song name
    
    #This has something to do with getting it to graph properly.
    id.table <- table(dfinit[, 1])
    dflimit <-
      subset(dfinit, Top100song %in% names(id.table[id.table >= countlimit]))
    
    #change the row names to their proper numbers. (important generating NA rows?)
    row.names(dflimit) <- seq(1,nrow(dflimit),1)
    
    dfexpand <- data.frame() #create new data frame to bind rows to
    
    #Binds each row to the empty data frame and generates a row if there should be a gap
    for (r in 1:nrow(dflimit)) {
      dfexpand <- rbind(dfexpand, dflimit[r,]) #binds normal rows
      
      timediff <-
        as.numeric(dflimit$Date[r + 1] - dflimit$Date[r]) #calculates the difference in time between two adjacent rows
      
      #if there is more than one week between rows, bind a blank row in between
      if (timediff > 8 || is.na(timediff)) {
        dfexpand <-
          rbind(dfexpand, c(as.character(dflimit[r, 1]), rep(NA, times = (ncol(dflimit)-1))))
      }
    }
    
    #copies the date from the previous row and adds one day to rows with NA as the date
    for (r in 1:nrow(dfexpand)) {
      if (is.na(dfexpand$Date[r])) {
        dfexpand$Date[r] <- as.Date(dfexpand$Date[r - 1] + 1)
      }
    }
    
    colnames(dfexpand)[1] <-
      "Song" #change column name for legend title
    
    #converts rank to a number for plotting purposes
    dfexpand[,4] <- as.numeric(dfexpand[,4])
    
    dffinal <-
      subset(dfexpand, Date > limitdate) #Subset songs based on date
    
    return(dffinal)
  }

#plotting function
artistplotbasic <- function(artistdf) {
  p <- (
    ggplot(artistdf, aes(
      x = Date, y = Top100rank, color = Song
    ))
    + geom_point()
    + geom_line()
    + scale_y_reverse(name = "Position", breaks = c(1, seq(10, 100, 10)))
    + scale_x_date(
      name = element_blank(), date_breaks = "2 months", date_labels = "%b%y"
    )
    + labs(list(
      title = paste("Evolution of", artistdf[1,1]), legend = "Song Title"
    ))
    + theme(
      plot.title = element_text(
        size = 22, face = "bold", margin = margin(0,0,15,0)
      ),
      legend.title = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(
        size = 14, face = "bold", margin = margin(0,20,0,0)
      ),
      axis.text = element_text(size = 14),
      plot.margin = margin(20,10,20,20)
    )
  )
}

#calculate AUC and generate new dataframe
artistAUC <- function (artistdf) {
  artistdf$AUCcalc <- 101 - artistdf$Top100rank
  artistaggdf <- aggregate(AUCcalc ~ Song, data = artistdf, sum)
  return (artistaggdf)
}

#TSWIFT EXAMPLES
test1 <- billboardfunc("Taylor Swift")

tswifttest <- artistplotbasic(test1)
tswifttest

Tswiftagg <- artistAUC(test1)
Tswiftagg$Views <-
  c(736426012, 1437592992, NA, 44872910, 1299792051, 374772444, NA, 354629255, NA, NA) #as of 2/1/2016
TSfit <- lm(AUCcalc ~ Views, data = Tswiftagg)
p2 <- (ggplot(data = Tswiftagg)
       + geom_point(aes(x = Views, y = AUCcalc)))

p2

#SOME JUSTIN BIEBER EXAMPLES
JBtest <-
  billboardfunc("Justin Bieber", limitdate = "2014-08-28", countlimit = 2)
JBplot <- artistplotbasic(JBtest)
JBplot
JBagg <- artistAUC(JBtest)
JBagg$Views <-
  c(
    14957118, 145944134, 17126724, 250544268, 23179692, 9723278, 17049444, 36111551, 743466730, 30869719, 697902059
  ) #as of 2/2/2016
JBfit <- lm(AUCcalc ~ Views, data = JBagg)
summary(JBfit)

#ONE DIRECTION EXAMPLES PER MELANIES REQUEST
oneDtest <- billboardfunc("One Direction", limitdate = "2015-07-15", countlimit = 1)
oneDplot <- artistplotbasic(oneDtest)
oneDplot

#AWOLNATION
AWN <- billboardfunc("AWOLNATION", limitdate="2011-03-15", countlimit = 2)