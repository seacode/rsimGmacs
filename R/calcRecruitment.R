#'
#'@title Calculate recruitment-at-size by year, sex
#'
#'@param mc - model configuration object
#'
#'@return R_yxz: 3d array with numbers  of crab recruiting by year/sex/size
#'
#'@details R_yxz has units of exp(lnR). If lnR is in log(millions), then R_yxz
#'has units of millions of individuals.
#'
#'@import ggplot2
#'@import reshape2
#'
#'@export
#'
calcRecruitment<-function(mc,showPlot=TRUE){
    d <- mc$dims;      #model dimensions info
    p <- mc$params$rec;#recruitment parameters
    
    #calc annual size distribution
    R_yz  <- calcRatZ(mc,showPlot=showPlot)
    
    #calc total recruitment by year
    R_y   <- dimArray(mc,'y');
    dims  <- dim(R_y);
    dmnms <- dimnames(R_y);
    for (t in names(p$blocks)){
        tb<-p$blocks[[t]];
        yrs<-as.character(tb$years);
        ndvs <- length(yrs);
        sdR <- sqrt(log(1+(tb$cvR)^2));
        devs <- rnorm(ndvs,mean=0, sd=sdR);
        r_y <- exp(tb$lnR+devs-mean(devs)-(sdR^2)/2);
        names(r_y) <- yrs;
        R_y[yrs] <- r_y;#changes R_y from array to vector for some reason
    }
    R_y<-as.array(R_y,dim=dims,dimnames=dmnms);#change back to array
    dimnames(R_y)<-dmnms;#make sure names of dimnames are correct
    
    #calc sex-specific recruitment by year
    R_yx  <- dimArray(mc,'y.x');
    if (d$x$n==1){
        sdXR <- 0;
        R_yx[1+(1:ndvs),1] <- 1;
    } else {
        for (t in names(p$blocks)){
            tb<-p$blocks[[t]];
            yrs <-as.character(tb$years);
            ndvs<- length(yrs);
            devs<- rnorm(ndvs,mean=0, sd=tb$sdXR);
            mXR <- 1/(1+exp(-(tb$lnXR+devs+(tb$sdXR^2)/2)));
            R_yx[yrs,'male']   <- mXR;
            R_yx[yrs,'female'] <- 1-mXR;
        }
    }
    
    #calc year/sex/size-specific recruimtent
    R_yxz <- dimArray(mc,'y.x.z');
    for (y in d$y$nms){
        #cat('y =',y,'\n')
        for (x in d$x$nms){
            #cat('x =',x,'\n')
            R_yxz[y,x,] <- R_y[y]*R_yx[y,x]*R_yz[y,];
        }
    }
    
    if (showPlot){
        mdfr<-melt(R_y,value.name='n')
        py <- ggplot(mapping=aes(x=y,y=n),data=mdfr)
        py <- py + geom_line();
        py <- py + ylim(0,NA)
        py <- py + labs(x='year',y='Annual Total Recruitment (millions)',title='Recruitment')
        print(py)
        mdfr<-melt(R_yx,value.name='p')
        px <- ggplot(mapping=aes(x=y,y=p),data=mdfr[mdfr$x=='male',])
        px <- px + geom_line();
        px <- px + ylim(0,1)
        px <- px + labs(x='year',y='fraction male',title='sex ratio')
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
#'@return R_yz: d array with annual proportions of crab recruiting by size
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcRatZ<-function(mc,showPlot=TRUE){
    d <- mc$dims;      #model dimensions info
    p <- mc$params$rec;#recruitment parameters
    
    #calc size distribution
    R_yz  <- dimArray(mc,'y.z');
    mdfr<-NULL;
    for (t in names(p$blocks)){
        tb<-p$blocks[[t]];
        yrs<-as.character(tb$years);
        alpha <- exp(tb$lnAlphaZ);
        beta  <- exp(tb$lnBetaZ);
        zcs <- d$zc$vls;
        #print(zcs)
	    cum <- pgamma(zcs,shape=alpha/beta,scale=beta);
        prs<-dimArray(mc,'z');
        prs[] <- first_difference(cum);
        prs <- prs/sum(prs);#standardized to sum to 1
	    for (y in yrs) {R_yz[y,] <- prs;}
        mdfrp<-melt(prs,value.name='val');
        mdfrp$t<-t;
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (showPlot){
        pz <- ggplot(mapping=aes(x=z,y=val,color=t),data=mdfr)
        pz <- pz + geom_line();
        pz <- pz + labs(x='size (mm)',y='pr(Z)',title='size distribution')
        pz <- pz + guides(color=guide_legend('time block'));
        print(pz)
    }
    return(R_yz)
}

#---------------------------------------------------------------------
#'
#'@title Calculate initial proportions recruiting-at-size
#'
#'@param mc - model configuration object
#'
#'@return R_z: 1d array with initial proportions of crab recruiting by size
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcRatZ.init<-function(mc,showPlot=TRUE){
    d <- mc$dims;      #model dimensions info
    p <- mc$params$rec$inits;#recruitment parameters
    
    #calc size distribution
    R_z  <- dimArray(mc,'z');
    mdfr<-NULL;
        alpha <- exp(p$lnAlphaZ);
        beta  <- exp(p$lnBetaZ);
        zcs <- d$zc$vls;
        #print(zcs)
        cum <- pgamma(zcs,shape=alpha/beta,scale=beta);
        prs<-dimArray(mc,'z');
        prs[] <- first_difference(cum);
        prs <- prs/sum(prs);#standardized to sum to 1
        R_z[] <- prs;
        mdfr<-melt(prs,value.name='val');
    
    if (showPlot){
        pz <- ggplot(mapping=aes(x=z,y=val),data=mdfr)
        pz <- pz + geom_line();
        pz <- pz + labs(x='size (mm)',y='pr(Z)',title='initial size distribution')
        pz <- pz + guides(color=guide_legend(''));
        print(pz)
    }
    return(R_z)
}

