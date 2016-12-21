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

persp3dm<-function(d,zlabel="",reverse=TRUE) {
    if (!all(diff(d$lon)==0)) {
        warning("Current version requires a constant longitude!")
        return(NULL)
    }

    lat.range<-range(d$lat)
    par.range<-range(d$par)
    lon<-d$lon[1]
    lon.range<-range(d$lon)
    lon.extend<-(lat.range[2]-lat.range[1])*c(-0.5,0.5)
    
    m<-maps::map("worldHires",xlim=lon+lon.extend,ylim=lat.range,plot=FALSE)
    mm<-m
    mm$x[m$x<m$range[1] | m$x>m$range[2]]<-NA
    mm$y[m$y<m$range[3] | m$y>m$range[4]]<-NA
    a<-min(which(!is.na(mm$x))[1],which(!is.na(mm$y))[1])
    if (a>1) {
        mm$x<-mm$x[-c(1:a)]
        mm$y<-mm$y[-c(1:a)]
    }
    xyr<-data.frame(x=c(par.range[1],par.range[1],par.range[1],par.range[1],par.range[1]),
                    z=c(lat.range[1],lat.range[2],lat.range[2],lat.range[1],lat.range[1]),
                    y=c(lon+lon.extend[1],lon+lon.extend[1],lon+lon.extend[2],lon+lon.extend[2],lon+lon.extend[1]))
    
    par(mar=c(1.5,1.5,1.5,1.5)+0.1,mgp=c(0,0,0))
    p<-persp(par.range,
             lon+lon.extend,
             matrix(NA,nrow=2,ncol=2),
             zlim=lat.range,
             theta=if (reverse) 180 else 0,
             phi=0,
             r=1,
             d=1,
             axes=FALSE,
             box=FALSE,
             scale=TRUE)
    
    lxt<-if (reverse) lon+lon.extend[2] else lon+lon.extend[1]
    a<-seq(lat.range[1],lat.range[2],length.out=5)
    tick.start <- trans3d(par.range[1], lxt, a, p)
    tick.end   <- trans3d(par.range[1]-0.25, lxt, a, p)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    label.pos <- trans3d(par.range[1]-1.0, lxt, a, p)
    text(label.pos$x, label.pos$y, labels=sprintf("%g°",a), adj=0.5, srt=90, cex=0.75)
    mtext("Latitude",if (reverse) 4 else 2)
    
    a<-seq(par.range[1],par.range[2],length.out=5)
    if (reverse) a<-rev(a)
    tick.start <- trans3d(a, lon, lat.range[2]+0.5, p)
    tick.end   <- trans3d(a, lon, lat.range[2]+0.75, p)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    l.start <- trans3d(par.range[1], lon, lat.range[2]+0.5, p)
    l.end   <- trans3d(par.range[2], lon, lat.range[2]+0.5, p)
    segments(l.start$x, l.start$y, l.end$x, l.end$y)
    label.pos <- trans3d(a, lon, lat.range[2]+1.5, p)
    text(label.pos$x, label.pos$y, labels=sprintf("%g°",a), adj=0.5, srt=0, cex=0.75)
    text(trans3d(mean(par.range),lon,lat.range[2]+4.5,p),zlabel)
    
    lines(trans3d(par.range[1],mm$x,mm$y,p))
    lines(trans3d(xyr$x,xyr$y,xyr$z,p))
    
    return(p)
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
