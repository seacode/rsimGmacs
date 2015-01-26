#'
#'@title Project population numbers at size forward through the model time interval
#'
#'@description Function to project population numbers at size forward through the model time interval
#'
#'@param mc - model configuration list object
#'@param mp - model processes list object
#'@param iN_xmsz - initial numbers at size array
#'@param R_yxz - recruitment at size array
#'
#'@return N_yxmsz: 5-d array of population numbers by year/sex/maturity/shell condition/size
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcNatZ<-function(mc,mp,iN_xmsz,R_yxz,showPlot=TRUE){
    #calculate time series of population abundance
    d<-mc$dims;
    N_yxmsz <- dimArray(mc,'y.x.m.s.z');
    N_yxmsz[1,,,,] <- iN_xmsz;
    for (y in (1:(d$y$n-1))){#note that y index is an integer, not a string
        for (x in d$x$nms){
            N_msz <- dimArray(mc,'m.s.z');
            N_msz[,,] <- N_yxmsz[y,x,,,];#sex-specifc abundance at start of year y
            if (mc$type=='KC'){
                T_zz <- mp$T_xmszz[x,1,1,,]; #indep of m,s
                S_z  <- mp$S_yxmsz[y,x,1,1,];#indep of m,s
                #one maturity state
                if (d$s$n==1){ #one shell condition
                    n_z <- as.vector(N_msz[1,1,]);
                    np_z <- (n_z %*% T_zz) * S_z; #growth BEFORE survival
                    N_yxmsz[y+1,x,1,1,] <- np_z + R_yxz[y,x,];
                } else if (d$s$n==2){ #two shell conditions
                    prMolt_z<-mp$prMolt_xmsz[x,1,1,];#indep of m,s
                    n_z <- as.vector(N_msz[1,'new shell',]);#new shell
                    o_z <- as.vector(N_msz[1,'old shell',]);#old shell
                    np_z <- (((n_z+o_z) * prMolt_z) %*% T_zz)*S_z;# new shell, old shell that molt, grow survive
                    op_z <- ((n_z+o_z) * (1-prMolt_z))*S_z;
                    N_yxmsz[y+1,x,1,'new shell',] <- np_z + R_yxz[y,x,];
                    N_yxmsz[y+1,x,1,'old shell',] <- op_z;
                }
            } else if (mc$type=='TC'){
                #TODO: implement!!
            }
        }#x
    }#y
    if (showPlot){
        mdfr<-melt(N_yxmsz,value.name='val');
        ddfr<-dcast(mdfr,x+y~.,fun.aggregate=sum,value.var='val');
        p <- ggplot(aes(x=y,y=`.`,color=x,shape=x),data=ddfr);
        p <- p + geom_line(alpha=0.8,width=2);
        p <- p + geom_point(alpha=0.8);
        p <- p + labs(x='year',y='Population Abundance');
        p <- p + guides(color=guide_legend('',order=1,alpha=1),
                        shape=guide_legend('',order=3));
        print(p);
        
        #size comps
        p <- ggplot(aes(x=y,y=z,fill=val,size=val),data=mdfr);
        p <- p + geom_point(alpha=0.6,shape=21);
        p <- p + scale_size_area(max_size=10);
        p <- p + scale_fill_gradient();
        p <- p + geom_abline(intercept=0,slope=1,linetype=3,color='black');
        p <- p + labs(x='year',y='size (mm)',title='Population Abundance');
        p <- p + guides(fill=guide_colorbar('Abundance',order=1,alpha=1),
                        size=guide_legend('',order=2));
        if (mc$type=='KC'){
            p <- p + facet_wrap(~ x + s, ncol=1);#only 1 maturity state
        } else {
            p <- p + facet_wrap(~ m + s + x, ncol=1);
        }
        print(p);
    }
    
    return(N_yxmsz)
}