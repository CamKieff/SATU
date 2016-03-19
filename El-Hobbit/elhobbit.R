require(ggplot2)
require(gridSVG)

hobbit <- read.csv("ElHobbit.csv", header = T)

#creates a new data frame with the average words per page by chapter
hobbit.agg <- aggregate(hobbit, by = list(factor(hobbit$CHAPTER)), mean)

#another potentially useful way to aggregate future me
#aggregate(WORDS~CHAPTER,data=hobbit, summary)

#this is the linear regression part. It is searchable for the coefficients
hobreg <- with(hobbit, summary(lm(WORDS ~ CHAPTER)))

h0 <- (ggplot()
  +geom_point(aes(x = hobbit$CHAPTER, y = hobbit$WORDS), size = 4, alpha = 0.25)  
  +geom_point(aes(x = hobbit.agg$CHAPTER, y = hobbit.agg$WORDS), color = "red", size = 4, alpha = 0.75)
  +geom_abline(intercept = hobreg$coefficients[1], slope = hobreg$coefficients[2], size=1.1, alpha=0.75)
  +xlab("Chapter")
  +ylab("Number of Searched Words")
  +ggtitle("El Hobbit as a Learning Tool")
  +theme(axis.title=element_text(size=16), title=element_text(size=20))
  +scale_x_continuous(breaks=seq(1:19))
  +scale_y_continuous(breaks=seq(0,45,5))
)
h0

#export file as an svg
mysvg<-grid.export("elhobbit.svg", addClasses=TRUE)