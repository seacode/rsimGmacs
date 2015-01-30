#'
#'@title Calculate probability of molting at size for sex, maturity state, shell condition.
#'
#'@title Function to calculate probability of molting at size for sex, maturity state, shell condition.
#'
#'@param mc - model configuration object
#'@param showPlot - flag to show plots
#'
#'@return prMolt_yxmsz
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
        
        prMolt_yxmsz <- dimArray(mc,'y.x.m.s.z',val=0);    
        mdfr<-NULL;
        for (t in names(p$blocks)){
            tb<-p$blocks[[t]];
            yrs<-as.character(tb$years);
            for (x in d$x$nms){
                mu <- tb$mu[x]
                sd <- tb$cv[x] * mu;
                mp_z<-dimArray(mc,'z');
                mp_z[] <- 1.0 - ((1.0-2.*tiny)*plogis(d$z$vls,mu,sd) + tiny);
                mdfrp<-melt(mp_z,value.name='val');
                mdfrp$x<-x;
                mdfrp$t<-t;
                mdfr<-rbind(mdfr,mdfrp);
                for (m in d$m$nms){
                    for (s in d$s$nms) {
                        for (y in yrs) prMolt_yxmsz[y,x,m,s,]<-mp_z;
                    }#s
                }#m      
            }#x
        }#t
    } else {
        throwModelTypeError(mc$type,'calcPrMolt()');
    }
    
    if (showPlot){
        p <- ggplot(aes(x=z,y=val,color=x),data=mdfr)
        p <- p + geom_line()
        p <- p + labs(x='size (mm)',y='pr(molt|sex,size)')
        p <- p + guides(color=guide_legend('sex'));
        p <- p + facet_wrap(~t,ncol=1);
        print(p);
    }
    
    return(prMolt_yxmsz);
}