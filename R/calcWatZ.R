calcWatZ<-function(mc,showPlot=TRUE){
    if (mc$type=='KC'){
        W_xmsz <- calcWatZ.gmacs(mc,showPlot=showPlot)
    } else if (mc$type=='TC'){
        cat('TODO: calcWatZ() not implemented for TC yet!\n')
        stop();
    } else {
        throwModelTypeError(mc$type,'calcWatZ()');
    }
    return(W_xmsz);    
}

calcWatZ.gmacs<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$wAtZ;
    
    W_xmsz <- dimArray(mc,'x.m.s.z',val=0);
    for (x in d$x$nms){
        for (m in d$m$nms){
            for (s in d$s$nms){
                lnA<-log(p$a_xz[x,]);#no dependence on shell condition
                W_xmsz[x,m,s,]<-exp(lnA+p$b_xz[x,]*log(d$z$vls));
            }
        }
    }
    if (showPlot){
        
    }
    
    return(W_xmsz);
}