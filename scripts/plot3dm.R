library(maps)
library(mapdata)

trans<-function(m,l){
    return(data.frame(x=m$x-0.25*(m$x-l$x),
                      y=(m$y-l$y)*(1+0.01*(m$x-l$x))+l$y))
}

re.scale<-function(y,to=c(0,1),from=range(y,na.rm=TRUE))
{
  return(to[1]+(to[2]-to[1])*(y-from[1])/(from[2]-from[1]))
}

plot3dm<-function(d,zlabel="",reverse=TRUE) {
    if (!all(diff(d$lon)==0)) {
        warning("Current version requires a constant longitude!")
        return(NULL)
    }

    lat.range<-range(d$lat)
    lon<-d$lon[1]
    lon.extend<-(lat.range[2]-lat.range[1])*c(-0.25,0.5)
    par.range<-range(d$par)
    width<-1.45*lon.extend[2]
    flp<-if (reverse) {lon+c(0,-width)} else {lon+c(0,width)}
    x<-re.scale(d$par,flp)
    y<-d$lat
    
    m<-maps::map("worldHires",xlim=lon+lon.extend,ylim=lat.range,plot=FALSE)
    lat.cap<-0.1*(m$range[4]-m$range[3])
    
    mm<-m
    mm$x[m$x<m$range[1] | m$x>m$range[2]]<-NA
    mm$y[m$y<m$range[3] | m$y>m$range[4]]<-NA
    lonxy<-data.frame(x=lon,
                      y=mean(m$range[3:4]))
    tr<-trans(mm,lonxy)
    mm$x<-tr$x
    mm$y<-tr$y
    a<-min(which(!is.na(mm$x))[1],which(!is.na(mm$y))[1])
    if (a>1) {
        mm$x<-mm$x[-c(1:a)]
        mm$y<-mm$y[-c(1:a)]
    }
    
    maps::map(mm,xlim=c(m$range[1]-lon.extend[2],m$range[2]),ylim=c(m$range[3]-lat.cap,m$range[4]+lat.cap))
    xyr<-data.frame(x=c(m$range[1],m$range[1],m$range[2],m$range[2],m$range[1]),
                    y=c(m$range[3],m$range[4],m$range[4],m$range[3],m$range[3]))
    xyr.t<-trans(xyr,lonxy)
    lines(xyr.t)
    a<-seq(xyr.t[3,2],xyr.t[4,2],length.out=5)
    axis(4,
         mgp=c(0,0.5,0),
         at=a,
         labels=sprintf("%g°",a),
         pos=xyr.t[3,1])
    mtext("Latitude",4)
    if (reverse){
        rect(flp[1],lat.range[1],flp[2]-0.5,lat.range[2],col=rgb(1,1,1,alpha=0.75),border="black")
    } else {
        rect(flp[1],lat.range[1],flp[2]+0.5,lat.range[2],col=rgb(1,1,1,alpha=0.75),border="black")
    }
    if (reverse){
        lbl<-seq(lon,lon-width,length.out=5)
    } else {
        lbl<-seq(lon,lon+width,length.out=5)
    }
    axis(3,
         at=lbl,
         labels=seq(par.range[1],par.range[2],length.out=5),
         mgp=c(0,0.5,0),
         pos=lat.range[2])
    mtext(zlabel,3,at=if (reverse){lon-0.5*width}else{lon+0.5*width})
    return(data.frame(x=x,
                      y=y,
                      z=d$val))
}
