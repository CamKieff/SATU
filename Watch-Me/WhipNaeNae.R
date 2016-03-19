#create vectors of data based on number of "whips" or "nae naes" per verse
ndata<-c(2,4,6,8,10,12)
wdata<-c(3,6,9,12,15,18)

#create plot
whipnaenae<-(ggplot()
+geom_point(aes(x=ndata, y=wdata), size=6, alpha=0.5, color="#800000")
+ylab("WHIP")
+xlab("NAE NAE")
+theme_bw()
+theme(legend.position="none", title=element_text(size=20, face="bold"),axis.title=element_text(size=16, face="bold"),axis.text=element_text(size=14))
+ggtitle("WATCH ME")
+geom_abline(slope=3/2, size=1.5, alpha=0.75, color="red")
+annotate("text", x=5, y=15,label="R^2 = 1", size=6))

#plot graph
whipnaenae