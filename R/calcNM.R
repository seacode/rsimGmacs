#'
#'@title Calculate natural mortality.
#'
#'@param mc - model configuration object
#'
#'@return array M_yxmsz
#'
#'@import ggplot2
#'
#'@export
#'
calcNaturalMortality<-function(mc,showPlot=TRUE){
    if (mc$type=='KC'){
        M_yxmsz <- calcNM.gmacs(mc,showPlot=showPlot)
    } else {
        throwModelTypeError(mc$type,'calcNaturalMortality');
    }
    return(M_yxmsz);
}

#'
#'@title Calculate natural mortality for gmacs models.
#'
#'@param mc - model configuration object
#'
#'@return array M_yxmsz
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcNM.gmacs<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$nm;
    M_yxmsz <- dimArray(mc,'y.x.m.s.z');
    mdfr<-NULL;
    for (t in names(p$blocks)){
        tb<-p$blocks[[t]];
        yrs<-as.character(tb$years);
        M_x <- dimArray(mc,'x');
        M_x[]<-tb$mnM;
        for (x in d$x$nms) {M_yxmsz[yrs,x,,,] <- M_x[x];}
        mdfrp<-melt(M_x,value.name='val');
        mdfrp$tb<-t;
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (showPlot){
        p <- ggplot(aes(x=tb,y=val,fill=x),data=mdfr)
        p <- p + geom_bar(stat='identity',position='dodge')
        p <- p + labs(x='time block',y='M')
        p <- p + guides(fill=guide_legend('sex'))
        print(p);
    }
    return(M_yxmsz);
}

#'
#'@title Calculate natural mortality for tcsam models.
#'
#'@param mc - model configuration object
#'
#'@return array M_yxmsz
#'
#'@import ggplot2
#'
#'@export
#'
calcNM.tcsam<-function(mc,showPlot=TRUE){
    p<-mc$params$nm;
    
    M_yxmsz <- dimArray(mc,'y.x.m.s.z',val=p$LnM);
    #TODO: add in other factors
    
    M_yxmsz <- exp(M_yxmsz);
    
    #TODO: add in plot
    return(M_yxmsz);
}