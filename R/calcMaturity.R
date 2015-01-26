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
#'@return mat_xz
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcMaturity<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$maturity;
    
    if (mc$type=='KC'){
        mat_xz <- p$mat_xz;
    } else if (mc$type=='TC'){
        mat_xz <- p$mat_xz;
    } else {
        throwModelTypeError(mc$type,'calcMaturity()');
    }
    
    if (showPlot){
        mdfr<-melt(mat_xz,value.name='val');
        p <- ggplot(aes(x=z,y=val,color=x),data=mdfr)
        p <- p + geom_line()
        p <- p + labs(x='size (mm)',y='pr(maturity|size)')
        p <- p + guides(color=guide_legend('sex'))
        print(p);
    }
    
    return(mat_xz);
}