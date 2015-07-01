#'
#'@title Calculate survey numbers at size through the model time interval
#'
#'@description Function to calculate survey numbers at size through the model time interval
#'
#'@param mc - model configuration list object
#'@param mp - model processes list object
#'@param N_yxmsz - initial numbers at size array
#'
#'@return N_vyxmsz: 6-d array of survey numbers by year/sex/maturity/shell condition/size
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcNatZ.Surveys<-function(mc,mp,N_yxmsz,showPlot=TRUE){
    #calculate time series of survey abundance
    d<-mc$dims;
    Q_vyxmsz <- mp$S_list$Q_vyxmsz;
    N_vyxmsz <- dimArray(mc,'v.y.x.m.s.z');
    for (v in d$v$nms){
        N_vyxmsz[v,,,,,]<-Q_vyxmsz[v,,,,,]*N_yxmsz[,,,,];
    }#v
    if (showPlot){
        mdfr<-melt(N_vyxmsz,value.name='val');
        ddfr<-dcast(mdfr,v+x+y~.,fun.aggregate=sum,value.var='val');
        p <- ggplot(aes(x=y,y=`.`,color=v,linetype=x,shape=x),data=ddfr);
        p <- p + geom_line(alpha=0.8,width=2);
        p <- p + geom_point(alpha=0.8);
        p <- p + ylim(0,NA)
        p <- p + labs(x='year',y='Survey Abundance (millions)');
        p <- p + guides(color=guide_legend('',order=1,alpha=1),
                        linetype=guide_legend('',order=2),
                        shape=guide_legend('',order=3));
        print(p);
        
        #size comps
        p <- ggplot(aes(x=y,y=z,fill=val,size=val),data=mdfr);
        p <- p + geom_point(alpha=0.6,shape=21);
        p <- p + scale_size_area(max_size=10);
        p <- p + scale_fill_gradient();
        p <- p + geom_abline(intercept=0,slope=1,linetype=3,color='black');
        p <- p + labs(x='year',y='size (mm)',title='Survey Abundance');
        p <- p + guides(fill=guide_colorbar('Abundance\n(millions)',order=1,alpha=1),
                        size=guide_legend('',order=2));
        if (mc$dims$m$n==1){
            p <- p + facet_grid(v + s ~ x);#only 1 maturity state
        } else {
            p <- p + facet_grid(v + m + s ~ x);
        }
        print(p);
    }
    
    return(N_vyxmsz)
}