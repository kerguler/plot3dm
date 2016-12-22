# plot3dm
R script to overlay data onto a map.

    source("plot3dm.R")

    scl<-rainbow(11)
    cl.scl<-function(v) sapply(v,function(vv) scl[max(-5,min(log2(vv),5))+6])

    d<-read.csv("../data/data.csv",header=TRUE)
    p<-persp3dm(d,"Parameter axis label")
    points(trans3d(d$par,d$lon,d$lat,p),col=cl.scl(d$val))

data.csv

    lon,lat,par,val
    -88.625,13.125,0,3.24535
    -88.625,13.125,0.5,3.24535
    -88.625,13.125,1,3.24535
    ...
    -88.625,13.625,0,4.95867
    -88.625,13.625,0.5,4.95867
    -88.625,13.625,1,4.95867
    ...
