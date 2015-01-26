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
#'@import ggplot2
#'
#'@export
#'
calcNM.gmacs<-function(mc,showPlot=TRUE){
    p<-mc$params$nm;
    M_yxmsz <- dimArray(mc,'y.x.m.s.z',val=p$M0);
    #TODO: add in time-varying natural mortality
    #TODO: add in plot
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