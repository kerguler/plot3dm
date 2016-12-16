source("plot3dm.R")

# -------------------------------------------------------------

d<-read.csv("../data/data.csv",header=TRUE)
d<-d[d$lat<50,]

scl<-rainbow(11)
cl.scl<-function(v) sapply(v,function(vv) scl[max(-5,min(log2(vv),5))+6])

zlabel<-"Critical temperature (°C)"

# -------------------------------------------------------------

m<-plot3dm(d,zlabel)
points(m$x,m$y,col=cl.scl(m$z),pch=15,cex=0.5)

