#'
#'@title Calculate size-specific fishery capture and retention rates.
#'
#'@description Function to calculate size-specific fishery capture and retention rates.
#'
#'@param mc - model configuration object
#'@param showPlot - flag to show plots
#'
#'@return list with the following elements
#'sel_fyxmsz - size-specific selectivity
#'ret_fyxmsz - size-specific retention
#'hm_fy - handling mortality rate
#'F_fyxms - fully-selected fishing capture rate
#'FC_fyxmsz - size-specific fishing capture rate
#'FM_fyxmsz - size-specific fishing mortality rate
#'RM_fyxmsz - size-specific retention mortality rate
#'DM_fyxmsz - size-specific discard mortality rate
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcFishingMortalities<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    fs<-mc$params$fisheries;
    
    hm_fy     <-dimArray(mc,'f.y');
    F_fyxms   <-dimArray(mc,'f.y.x.m.s');
    sel_fyxmsz<-dimArray(mc,'f.y.x.m.s.z')
    ret_fyxmsz<-dimArray(mc,'f.y.x.m.s.z');
    FC_fyxmsz <-dimArray(mc,'f.y.x.m.s.z');
    FM_fyxmsz <-dimArray(mc,'f.y.x.m.s.z');
    RM_fyxmsz <-dimArray(mc,'f.y.x.m.s.z');
    DM_fyxmsz <-dimArray(mc,'f.y.x.m.s.z');
    
    for (f in names(fs)){
        F_yx   <-dimArray(mc,'y.x');  #sex-specific capture rates by year for fishery f
        sel_yxz<-dimArray(mc,'y.x.z');#sex-specific capture selectivity by year for fishery f
        ret_yxz<-dimArray(mc,'y.x.z');#sex-specific retention by year for fishery f
        blocks<-fs[[f]]$blocks;
        for (t in names(blocks)){
            b<-blocks[[t]];
            yrs<-as.character(b$years);
            hm_fy[f,yrs]<-b$hm;
            F_b<-b$mnF*exp(-(b$sdF^2)/2+rnorm(length(yrs),mean=0,sd=b$sdF));#annual F's in time block for males
            sel_xz<-dimArray(mc,'x.z');#sex-specific capture selectivity for time block
            ret_xz<-dimArray(mc,'x.z');#sex-specific retention for time block
            for (x in d$x$nms){
                #set capture rates
                fac<-1;
                if (x=='female') fac<-b$offFX;
                F_yx[yrs,x]<-fac*F_b;
                
                #calc selectivity/retention curves
                si<-b$sel[[x]];#selectivity info
                ri<-b$ret[[x]];#retention info
                sel_xz[x,]<-calcSelectivity(si$type,d$z$vls,si$params);
                ret_xz[x,]<-0*sel_xz[x,];
                if (!is.null(ri)) ret_xz[x,]<-calcSelectivity(ri$type,d$z$vls,ri$params);
                for (y in yrs) {sel_yxz[y,x,]<-sel_xz[x,];}
                for (y in yrs) {ret_yxz[y,x,]<-ret_xz[x,];}
            }#x
            if (showPlot){
                sdfr<-melt(sel_xz,value.name='val');
                sdfr$type<-'selectivity';
                rdfr<-melt(ret_xz,value.name='val');
                rdfr$type<-'retention';
                mdfr<-rbind(sdfr,rdfr)
                p <- ggplot(aes(x=z,y=val,color=x,shape=type),data=mdfr);
                p <- p + geom_point(size=6);
                p <- p + geom_line();
                p <- p + labs(x='size (mm)',y='selectivity/retention',title=paste(f,", ",t,sep=''))
                p <- p + guides(color=guide_legend(''),shape=guide_legend(''))
                print(p)
            }
            #print(dimnames(F_fyxms))
            for (y in yrs){
                for (x in d$x$nms){
                    for (m in d$m$nms){
                        for (s in d$s$nms) {
                            #no dependence on m,s assumed
                            #cat(f,y,x,m,s,'\n')
                            F_fyxms[f,y,x,m,s]     <- F_yx[y,x];
                            sel_fyxmsz[f,y,x,m,s,] <- sel_yxz[y,x,];
                            ret_fyxmsz[f,y,x,m,s,] <- ret_yxz[y,x,];
                            #fishing capture rates
                            FC_fyxmsz[f,y,x,m,s,]  <- F_fyxms[f,y,x,m,s]*sel_fyxmsz[f,y,x,m,s,];
                            #retention mortality rates
                            RM_fyxmsz[f,y,x,m,s,]  <- FC_fyxmsz[f,y,x,m,s,]*(ret_fyxmsz[f,y,x,m,s,]);
                            #discard mortality rates
                            DM_fyxmsz[f,y,x,m,s,]  <- FC_fyxmsz[f,y,x,m,s,]*((1-ret_fyxmsz[f,y,x,m,s,])*b$hm);
                            #total fishing mortality rates
                            FM_fyxmsz[f,y,x,m,s,]  <- RM_fyxmsz[f,y,x,m,s,]+DM_fyxmsz[f,y,x,m,s,];
                        }#s
                    }#m
                }#x
            }#y
        }#b
        if (showPlot){
            mdfr<-melt(F_yx,value.name='val');
            p <- ggplot(aes(x=y,y=val,color=x),data=mdfr);
            p <- p + geom_line();
            p <- p + labs(x='year',y='Capture Rate',title=f)
            p <- p + guides(color=guide_legend(''))
            print(p)
        }
    }#f
    
    return(list(sel_fyxmsz=sel_fyxmsz,ret_fyxmsz=ret_fyxmsz,hm_fy=hm_fy,F_fyxms=F_fyxms,
                FC_fyxmsz=FC_fyxmsz,FM_fyxmsz=FM_fyxmsz,RM_fyxmsz=RM_fyxmsz,DM_fyxmsz=DM_fyxmsz))
}
