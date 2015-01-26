#'
#'@title Calculate recruitment-at-size by year, sex
#'
#'@param mc - model configuration object
#'
#'@return R_yxz: 3d array with numbers  of crab recruiting by year/sex/size
#'
#'@import ggplot2
#'
#'@export
#'
calcRecruitment<-function(mc,showPlot=TRUE){
    d <- mc$dims;      #model dimensions info
    p <- mc$params$rec;#recruitment parameters
    
    #calc size distribution
    R_z  <- calcRatZ(mc,showPlot=showPlot)
    
    #calc total recruitment by year
    R_y  <- dimArray(mc,'y');
    ndvs <- (d$y$n-1);
    sigR <- exp(p$lnSigR);
    devs <- rnorm(ndvs,mean=0, sd=sigR);
    R_y[1+(1:ndvs)] <- exp(p$lnR+devs-mean(devs)-(sigR^2)/2);
    
    #calc sex-specific recruitment by year
    R_yx  <- dimArray(mc,'y.x');
    if (d$x$n==1){
        sigXR <- 0;
        R_yx[1+(1:ndvs),1] <- 1;
    } else {
        ndvs  <- (d$y$n-1);
        if (is.na(p$lnSigXR)){
            sigXR <- 0;
            devs  <- 0*(1:ndvs)
        } else {
            sigXR <- exp(p$lnSigXR);
            devs  <- rnorm(ndvs,mean=0, sd=sigXR);
        }
        mXR <- 1/(1+exp(-(p$lnXR+devs+(sigXR^2)/2)));
        R_yx[1+(1:ndvs),1] <- mXR;
        R_yx[1+(1:ndvs),2] <- 1-mXR;
    }
    
    #calc year/sex/size-specific recruimtent
    R_yxz <- dimArray(mc,'y.x.z');
    for (y in d$y$nms){
        #cat('y =',y,'\n')
        for (x in d$x$nms){
            #cat('x =',x,'\n')
            R_yxz[y,x,] <- R_y[y]*R_yx[y,x]*R_z;
        }
    }
    
    if (showPlot){
        mdfr<-melt(R_y,value.name='n')
        py <- ggplot(mapping=aes(x=y,y=n),data=mdfr)
        py <- py + geom_line();
        py <- py + labs(x='year',y='Total Annual Recruitment',title='Recruitment')
        print(py)
        mdfr<-melt(R_yx,value.name='p')
        px <- ggplot(mapping=aes(x=y,y=p),data=mdfr[mdfr$x=='male',])
        px <- px + geom_line();
        px <- px + labs(x='year',y='fraction male',title='sex ratio')
        px <- px + ylim(c(0,1))
        print(px)
    }
    return(R_yxz)
}
#---------------------------------------------------------------------
#'
#'@title Calculate proportions recruiting-at-size
#'
#'@param mc - model configuration object
#'
#'@return R_z: d array with proportions of crab recruiting by size
#'
#'@import ggplot2
#'
#'@export
#'
calcRatZ<-function(mc,showPlot=TRUE){
    d <- mc$dims;      #model dimensions info
    p <- mc$params$rec;#recruitment parameters
    
    #calc size distribution
    R_z  <- dimArray(mc,'z');
    alpha <- exp(p$lnAlphaZ);
    beta  <- exp(p$lnBetaZ);
    zcs <- d$zc$vls;
    print(zcs)
	cum <- pgamma(zcs,shape=alpha/beta,scale=beta);
	R_z[] <- first_difference(cum)[];
	R_z <- R_z/sum(R_z);   # Standardize so sums to 1.0
    
    if (showPlot){
        mdfr<-melt(R_z,value.name='n');
        pz <- ggplot(mapping=aes(x=z,y=n),data=mdfr)
        pz <- pz + geom_line();
        pz <- pz + labs(x='size (mm)',y='pr(Z)',title='size distribution')
        print(pz)
    }
    return(R_z)
}

#R_yxz <- calcRecruitment(mc)