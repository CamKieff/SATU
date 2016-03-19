require(reshape2)
require(ggplot2)

moped2<-read.csv('mopedreport4.csv', header=TRUE)

#use reshpae2 to convert long data into column data
mopeddata2<-dcast(moped2, fun.aggregate=mean, MONTH ~ YEAR, value.var="MOPED")

#use data from the previous december as starting data for January
tempvec<-c(NA, subset(mopeddata2[12,], select=c('MONTH', '2008','2009','2010','2011','2012','2013','2014')))
mopeddata2<-rbind(as.numeric(tempvec), mopeddata2)


#I do not know why I need these but when I switched from mopedreport2 to mopedreport3 it added an extra column and row (11/9/15)
mopeddata2$'NA'<-NULL
mopeddata2<-mopeddata2[-14,]

#Remove partial 2008 data and take means on all pre-2015 data
mopeddata2$`2008`<-NULL
mopeddata2$MEAN<-apply(subset(mopeddata2, select=c('2009','2010','2011','2012','2013','2014')), 1, function(x) mean(x,na.rm=T))

#find column means and set to a vector.  These are not used again.
septmeans<-colMeans(mopeddata2[1:9,], na.rm=T)

#change months to month names. These will be offset by 1 month to make the graph look correct
mopeddata2$MONTH<-c(month.abb, ' ')

#ensures that months are plotted in the correct order rather than alphabetically
mopeddata2$MONTH<-as.character(mopeddata2$MONTH)
mopeddata2$MONTH<-factor(mopeddata2$MONTH, levels=unique(mopeddata2$MONTH))

#Create graph
mopedplot2<-(ggplot()
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$MEAN, group=1),size=1.5, alpha=0.5)
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$'2009', group=1),alpha=0.25)
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$'2010', group=1), alpha=0.25)
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$'2011', group=1), alpha=0.25)
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$'2012', group=1), alpha=0.25)
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$'2013', group=1), alpha=0.25)
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$'2014', group=1), alpha=0.25)
             +geom_line(aes(x=mopeddata2$MONTH, y=mopeddata2$'2015', group=1), size=1.5, alpha=0.5, color="red")
             +geom_vline(xintercept=8.75, linetype="longdash", alpha=0.5)
             +labs(list(title="Moped Search Traffic", x=NULL, y="Relative Search Traffic"))
             +theme(title=element_text(size=18, face="bold"),axis.title=element_text(size=14, face="bold"),axis.text=element_text(size=14))
             +annotate("text", x=7.35, y=31, label="Downtown Drops", size=5,alpha=0.75,face="bold")
)

#plot graph
mopedplot2

#save as SVG
mysvg<-grid.export("mopedsvg.svg", addClasses=TRUE)