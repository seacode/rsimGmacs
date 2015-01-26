#'
#'@title Calculate fishery catches, mortality at size through the model time interval
#'
#'@description Function to calculate fishery catches, mortality at size through the model time interval
#'
#'@param mc - model configuration list object
#'@param mp - model processes list object
#'@param N_yxmsz - numbers at size array
#'
#'@return list with the following elements:
#'NC_fyxmsz: 6-d array of fishery catches (NOT MORTALITY) by year/sex/maturity/shell condition/size
#'NM_fyxmsz: 6-d array of fishery mortality as numbers by year/sex/maturity/shell condition/size
#'NR_fyxmsz: 6-d array of retention mortality by year/sex/maturity/shell condition/size
#'ND_fyxmsz: 6-d array of discard mortality by year/sex/maturity/shell condition/size
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcNatZ.Fisheries<-function(mc,mp,N_yxmsz,showPlot=TRUE){
    #calculate time series of fisheries catches
    d<-mc$dims;
    S_yxmsz   <- mp$S_yxmsz;#survival
    Z_yxmsz   <- mp$Z_yxmsz;#total mortality rate
    FC_fyxmsz <- mp$F_list$FC_fyxmsz;#capture rates
    FM_fyxmsz <- mp$F_list$FM_fyxmsz;#mortality rates
    RM_fyxmsz <- mp$F_list$RM_fyxmsz;#retention mortality rates
    DM_fyxmsz <- mp$F_list$DM_fyxmsz;#discard mortality rates
    
    fac_yxmsz  <- dimArray(mc, 'y.x.m.s.z');
    fac_yxmsz[,,,,]<-((1-S_yxmsz[,,,,])/Z_yxmsz[,,,,])*N_yxmsz[,,,,];
    
    NC_fyxmsz <- dimArray(mc,'f.y.x.m.s.z');#numbers captured
    NM_fyxmsz <- dimArray(mc,'f.y.x.m.s.z');#numbers killed
    NR_fyxmsz <- dimArray(mc,'f.y.x.m.s.z');#numbers retained
    ND_fyxmsz <- dimArray(mc,'f.y.x.m.s.z');#numbers discarded and killed
    for (f in d$f$nms){
        NC_fyxmsz[f,,,,,]<-FC_fyxmsz[f,,,,,]*fac_yxmsz[,,,,];#numbers captured-at-size
        NM_fyxmsz[f,,,,,]<-FM_fyxmsz[f,,,,,]*fac_yxmsz[,,,,];#total mortality-at-size
        NR_fyxmsz[f,,,,,]<-RM_fyxmsz[f,,,,,]*fac_yxmsz[,,,,];#retained mortality-at-size
        ND_fyxmsz[f,,,,,]<-DM_fyxmsz[f,,,,,]*fac_yxmsz[,,,,];#discard mortality-at-size
    }#f
    if (showPlot){
        ncdfr<-melt(NC_fyxmsz,value.name='val'); ncdfr$type<-'captured';
        nmdfr<-melt(NM_fyxmsz,value.name='val'); nmdfr$type<-'total mortality';
        rmdfr<-melt(NR_fyxmsz,value.name='val'); rmdfr$type<-'retained mortality';
        dmdfr<-melt(ND_fyxmsz,value.name='val'); dmdfr$type<-'discard mortality';
        mdfr<-rbind(ncdfr,nmdfr,rmdfr,dmdfr);
        ddfr<-dcast(mdfr,f+type+x+y~.,fun.aggregate=sum,value.var='val');
        p <- ggplot(aes(x=y,y=`.`,color=f,linetype=type,shape=type),data=ddfr);
        p <- p + geom_line(alpha=0.8,width=2);
        p <- p + geom_point(alpha=0.8);
        p <- p + labs(x='year',y='Fishery Catch/Mortality');
        p <- p + guides(color=guide_legend('fishery',order=1,alpha=1),
                        linetype=guide_legend('type',order=2),
                        shape=guide_legend('type',order=2));
        p <- p + facet_wrap(~ x, ncol=1);#only 1 maturity state
        print(p);
        
        #size comps
#         p <- ggplot(aes(x=y,y=z,fill=val,size=val),data=mdfr);
#         p <- p + geom_point(alpha=0.6,shape=21);
#         p <- p + scale_size_area(max_size=10);
#         p <- p + scale_fill_gradient();
#         p <- p + geom_abline(intercept=0,slope=1,linetype=3,color='black');
#         p <- p + labs(x='year',y='size (mm)',title='Survey Abundance');
#         p <- p + guides(fill=guide_colorbar('Abundance',order=1,alpha=1),
#                         size=guide_legend('',order=2));
#         if (mc$type=='KC'){
#             p <- p + facet_wrap(~ v + x + s, ncol=1);#only 1 maturity state
#         } else {
#             p <- p + facet_wrap(~ m + s + x, ncol=1);
#         }
#         print(p);
    }
    
    return(list(NC_fyxmsz=NC_fyxmsz,NM_fyxmsz=NM_fyxmsz,
                NR_fyxmsz=NR_fyxmsz,ND_fyxmsz=ND_fyxmsz));
}