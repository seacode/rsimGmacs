#'
#'@title Calculate size-specific survey catchabilities.
#'
#'@description Function to calculate size-specific survey catchabilities.
#'
#'@param mc - model configuration object
#'@param showPlot - flag to show plots
#'
#'@return list with the following elements:
#'sel_vyxmsz - size-specific selectivity
#'Q_vyxms - fully-selected survey catchability
#'Q_vyxmsz - size-specific survey catchability
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcSurveyCatchabilities<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    vs<-mc$params$surveys
    
    Q_vyxms   <-dimArray(mc,'v.y.x.m.s');
    sel_vyxmsz<-dimArray(mc,'v.y.x.m.s.z')
    Q_vyxmsz  <-dimArray(mc,'v.y.x.m.s.z');
    
    for (v in names(vs)){
        Q_yx   <-dimArray(mc,'y.x');  #sex-specific capture rates by year for survey v
        sel_yxz<-dimArray(mc,'y.x.z');#sex-specific capture selectivity by year for survey v
        blocks<-vs[[v]]$blocks;
        for (t in names(blocks)){
            b<-blocks[[t]];
            yrs<-as.character(b$years);
            Q_b<-b$mnQ*exp(-(b$sdQ^2)/2+rnorm(length(yrs),mean=0,sd=b$sdQ));#annual Q's in time block for males
            sel_xz<-dimArray(mc,'x.z');#sex-specific capture selectivity for time block
            for (x in d$x$nms){
                #set catchabilities
                fac<-1;
                if (x=='female') fac<-b$offQX;
                Q_yx[yrs,x]<-fac*Q_b;
                
                #calc selectivity/retention curves
                si<-b$sel[[x]];#selectivity info
                sel_xz[x,]<-calcSelectivity(si$type,d$z$vls,si$params);
                for (y in yrs) {sel_yxz[y,x,]<-sel_xz[x,];}
            }#x
            if (showPlot){
                #plot selectivity functions
                mdfr<-melt(sel_xz,value.name='val');
                p <- ggplot(aes(x=z,y=val,color=x),data=mdfr);
                p <- p + geom_point(size=6);
                p <- p + geom_line();
                p <- p + ylim(0,1.05)
                p <- p + labs(x='size (mm)',y='survey selectivity',title=paste(v,", ",t,sep=''))
                p <- p + guides(color=guide_legend(''))
                print(p)
            }
            for (y in yrs){
                for (x in d$x$nms){
                    for (m in d$m$nms){
                        for (s in d$s$nms) {
                            #no dependence on m,s assumed
                            Q_vyxms[v,y,x,m,s]     <- Q_yx[y,x];
                            sel_vyxmsz[v,y,x,m,s,] <- sel_yxz[y,x,];
                            Q_vyxmsz[v,y,x,m,s,]   <- Q_yx[y,x]*sel_yxz[y,x,];
                        }#s
                    }#m
                }#x
            }#y
        }#t
        if (showPlot){
            #plot fully-selected catchabilities
            mdfr<-melt(Q_yx,value.name='val');
            p <- ggplot(aes(x=y,y=val,color=x),data=mdfr);
            p <- p + geom_line();
            p <- p + ylim(0,NA)
            p <- p + labs(x='year',y='Catchability',title=v)
            p <- p + guides(color=guide_legend(''))
            print(p)
        }
    }#v
    
    return(list(sel_vyxmsz=sel_vyxmsz,Q_vyxms=Q_vyxms,Q_vyxmsz=Q_vyxmsz))
}