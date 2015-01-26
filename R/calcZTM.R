calcZTM<-function(mc,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$growth;
    
    #calc. molt increment
    mi_xz <- dimArray(mc,'x.z');
    for (x in d$x$nms){
        mi_xz[x,]<-p$a_x[x]-p$b_x[x]*d$z$vls;#molt increment for MIDPOINTS
    }
    
    mnZAM_xz    <- dimArray(mc,'x.z')
    prZAM_xzz   <- dimArray(mc,'x.z.zp')
    prZAM_xmszz <- dimArray(mc,'x.m.s.z.zp')
    for (x in d$x$nms){
        mnZAM_xz[x,]<-d$z$vls+mi_xz[x,]
        for (z in 1:d$z$n){ #looping over size bins
            idx<-z:d$zc$n;#index over CUTPOINTS
            cumZAM<-pgamma(d$zc$vls[idx],shape=mnZAM_xz[x,z]/p$scale[x],scale=p$scale[x]);#integrated up each cutpoint
            prZAM<-first_difference(cumZAM);#contribution to each bin
            #TODO: no truncation here!!
            prZAM<-prZAM/sum(prZAM);
            prZAM_xzz[x,z,z:d$zp$n]<-prZAM;
        }
        for (m in d$m$nms){
            for (s in d$s$nms){
                prZAM_xmszz[x,m,s,,]<-prZAM_xzz[x,,];#indep of maturity state, shell condition
            }
        }
    }
    if (showPlot){
        mdfr<-melt(mnZAM_xz,value.name='val');
        p <- ggplot(aes(x=z,y=val,color=x,shape=x),data=mdfr);
        p <- p + geom_abline(intercept=0,slope=1,linetype=3,color='black')
        p <- p + geom_line(width=2);
        p <- p + geom_point(size=5);
        p <- p + guides(color=guide_legend(''),shape=guide_legend(''));
        p <- p + labs(x='pre-molt size (mm)',y='mean post-molt size (mm)');
        print(p);
        mdfr<-melt(prZAM_xzz,value.name='val');
        p <- ggplot(aes(x=z,y=zp,fill=val,size=val),data=mdfr);
        p <- p + geom_point(alpha=0.6,shape=21);
        p <- p + scale_size_area(max_size=10);
        p <- p + scale_fill_gradient()
        p <- p + geom_abline(intercept=0,slope=1,linetype=3,color='black')
        p <- p + labs(x='pre-molt size (mm)',y='post-molt size (mm)',title='Growth Transition Matrices');
        p <- p + guides(fill=guide_colorbar(expression(pr*bgroup("(",paste(Z[post],"|",Z[pre]),")")),order=1,alpha=1),
                        size=guide_legend('',order=2));
        p <- p + facet_wrap(~ x, ncol=1);
        print(p);
    }
    return(prZAM_xmszz)
}