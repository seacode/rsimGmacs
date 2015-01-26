#'
#'@title Get an array
#'
#'@param mc - model configurqtion object
#'@param str - string describing array indices
#'
#'@return array of appropriate dimensions filled with default value(s)
#'
#'@export
#'
dimArray<-function(mc,str,val=0){
    d<-mc$dims;
    strp<-strsplit(str,'.',fixed=TRUE);
    ns<-length(strp[[1]]);#number of dimensions
    dms<-0*(1:ns);        #dimensions sizes
    dmnms<-list();        #list of dimensions (names) with index values
    for (i in 1:ns){
        idx <- strp[[1]][i];
        dms[i]<-d[[idx]]$n;
        dmnms[[idx]]<-d[[idx]]$nms;
    }
    A<-array(data=val,dim=dms,dimnames=dmnms);
    return(A);
}