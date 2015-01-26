#'
#'@title Calculate probability of molting at size for sex, maturity state, shell condition.
#'
#'@title Function to calculate probability of molting at size for sex, maturity state, shell condition.
#'
#'@param mc - model configuration object
#'@param showPlot - flag to show plots
#'
#'@return prMolt_xmsz
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcPrMolt<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$molting;
    
    if (mc$type=='KC'){
        tiny <- 0.001;
        
        prMolt_xmsz <- dimArray(mc,'x.m.s.z',val=0);    
        for (x in d$x$nms){
            mu <- p$mu[x]
            sd <- p$cv[x] * mu;
            mp <- 1.0 - ((1.0-2.*tiny)*plogis(d$z$vls,mu,sd) + tiny)
            for (m in d$m$nms){
                for (s in d$s$nms) prMolt_xmsz[x,m,s,]<-mp;
            }            
        }
    } else if (mc$type=='TC') {
        prMolt_xmsz <- p$prMolt_xmsz;
    } else {
        throwModelTypeError(mc$type,'calcPrMolt()');
    }
    
    if (showPlot){
        mdfr<-melt(prMolt_xmsz,value.name='val');
        mdfr$facs <- paste(mdfr$m,mdfr$s,mdfr$x)
        p <- ggplot(aes(x=z,y=val,color=facs),data=mdfr)
        p <- p + geom_line()
        p <- p + labs(x='size (mm)',y='pr(molt|sex,maturity state,shell condition,size)')
        p <- p + guides(color=guide_legend('category'))
        print(p);
    }
    
    return(prMolt_xmsz);
}