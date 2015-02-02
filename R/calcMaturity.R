#'
#'@title Calculate maturity from model parameters.
#'
#'@description Function to calculate maturity from model parameters.
#'
#'@details Interpretation differs between king and Tanner crab models. For king crab, the
#'results represent sex-specific maturity ogives (by size). For Tanner crab, the results
#'represent the sex-specific probability that an immature crab will molt to maturity (by size).
#'
#'@param mc  model configuration list object
#'@param showPlot - flag (T/F) to plot results
#'
#'@return mat_yxz
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcMaturity<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$maturity;
    
    mat_yxz<-dimArray(mc,'y.x.z');
    if (mc$type=='KC'){
        mdfr<-NULL;
        for (t in names(p$blocks)){
            tb<-p$blocks[[t]];
            yrs<-as.character(tb$years);
            for (y in yrs) {mat_yxz[y,,] <- tb$mat_xz;}
            mdfrp<-melt(tb$mat_xz,value.name='val');
            mdfrp$tb<-t;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else {
        throwModelTypeError(mc$type,'calcMaturity()');
    }
    
    if (showPlot){
        p <- ggplot(aes(x=z,y=val,color=x),data=mdfr)
        p <- p + geom_line()
        p <- p + labs(x='size (mm)',y='pr(maturity|size)')
        p <- p + guides(color=guide_legend('sex'))
        p <- p + facet_wrap(~tb,ncol=1)
        print(p);
    }
    
    return(mat_yxz);
}
