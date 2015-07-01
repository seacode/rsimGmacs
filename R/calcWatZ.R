#'
#'@title Calculate weight-at-size by year, sex
#'
#'@param mc - model configuration object
#'
#'@return W_yxmsz: 5d array with weight-at-size by year/sex/maturity state/shell condition
#'
#'@import ggplot2
#'@import reshape2
#'
#'@export
#'
calcWatZ<-function(mc,showPlot=TRUE){
    if (mc$type=='KC'){
        W_yxmsz <- calcWatZ.gmacs(mc,showPlot=showPlot)
    } else {
        throwModelTypeError(mc$type,'calcWatZ()');
    }
    return(W_yxmsz);    
}

#'
#'@title Calculate weight-at-size by year, sex
#'
#'@param mc - model configuration object
#'
#'@return W_yxmsz: 5d array with weight-at-size by year/sex/maturity state/shell condition
#'
#'@import ggplot2
#'@import reshape2
#'
#'@export
#'
calcWatZ.gmacs<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$wAtZ;
    
    W_yxmsz <- dimArray(mc,'y.x.m.s.z',val=0);
    mdfr<-NULL;
    for (t in names(p$blocks)){
        tb<-p$blocks[[t]];
        yrs<-as.character(tb$years);
        W_xz<-dimArray(mc,'x.z');
        for (x in d$x$nms){
            lnA<-log(tb$a_xz[x,]);#no dependence on maturity/shell condition
            B  <-tb$b_xz[x,];
            W_xz[x,] <- exp(lnA+B*log(d$z$vls));
            for (m in d$m$nms){
                for (s in d$s$nms){
                    for (y in yrs) {W_yxmsz[y,x,m,s,]<-W_xz[x,]}
                }
            }
        }
        mdfrp<-melt(W_xz,value.name='val');
        mdfrp$t<-t;
        mdfr<-rbind(mdfr,mdfrp);
    }
    if (showPlot){
        pz <- ggplot(mapping=aes(x=z,y=val,color=x),data=mdfr)
        pz <- pz + geom_line();
        pz <- pz + ylim(0,NA)
        pz <- pz + labs(x='size (mm)',y='weight (kg)',title='')
        pz <- pz + guides(color=guide_legend('sex'));
        pz <- pz + facet_wrap(~t,ncol=1);
        print(pz)
    }
    
    return(W_yxmsz);
}