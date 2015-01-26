#'
#'@title Calculate initial size distributions.
#'
#'@description Function to calculate initial size distributions.
#'
#'@param mc - model configuration list object
#'@param mp - model processes list object
#'
#'@return n_xmsz: 4-d array of initial population abundance by sex/maturity/shell condition/size
#'
#'@import reshape2
#'@import ggplot2
#'
#'@export
#'
calcInitSizeDist<-function(mc,mp,showPlot=TRUE){
    d<-mc$dims;
    p<-mc$params$rec;
    
    n_xmsz<-dimArray(mc,'x.m.s.z');
    
    sigR<- exp(p$lnSigR);
    R0  <- exp(p$lnR-(sigR^2)/2);
    R_z <- calcRatZ(mc,showPlot=FALSE)
    if (mc$type=='KC'){
        r<-0.5*R0*R_z;#assumes equal size ratio
        for (x in d$x$nms){
            T_zz<-mp$T_xmszz[x,1,1,,];#indep of m,s
            S_z<-mp$S_yxmsz[1,x,1,1,];#indep of m,s
            if ((d$s$n==1)&(d$m$n==1)){
                #one shell condition, one maturity state
                n_xmsz[x,1,1,]<-calcEquilSizeDist.11(r,T_zz,S_z);
            } else if ((d$s$n==2)&(d$m$n==1)) {
                #two shell conditions, one maturity state
                prMolt_z<-mp$prMolt_xmsz[x,1,1,];#indep of m,s
                res_sz<-calcEquilSizeDist.21(r,T_zz,S_z,prMolt_z);
                n_xmsz[x,1,,]<-res_sz;
            } else {
                cat("Error in calcEqSizeDist(...). Invalid combination of shell conditions/maturity states for king crab models.\n")
                cat("number of shell conditions:",d$s$n,"\n")
                cat("number of maturity states:",d$m$n,"\n")
                cat("Aborting...\n")
                stop();
            }
        }#x
    } else if (mc$type=='TC'){
        cat('TODO: calcInitSizeDist() not implemented for TC yet!\n')
        stop();
    } else {
        throwModelTypeError(mc$type,'calcInitSizeDist()');
    }
    if (showPlot){
        mdfr<-melt(n_xmsz,value.name='val')
        p <- ggplot(aes(x=z,y=val,fill=s),data=mdfr);
        p <- p + geom_bar(alpha=0.5,stat='identity',position='dodge');
        p <- p + labs(x='size (mm)',y='initial abundance')
        p <- p + guides(fill=guide_legend('shell condition'))
        p <- p + facet_wrap(~x+m,ncol=1);
        print(p)
    }
    return(n_xmsz);
}

#'
#'@title Calculate the equilibrium size distribution for a model with 
#'1 shell condition and 1 maturity state.
#'
#'@description Function to calculate the equilibrium size distribution for a model with 
#'1 shell conditions and 1 maturity state.
#'
#'@param r - vector of equilibrium recruits-at-size
#'@param A - size transition matrix
#'@param S - vector or diagonal matrix of survival by size class
#'
#'@return list with equilibrium numbers-at-size vectors for new shell and old shell crab
#'
calcEquilSizeDist.11<-function(r,A,S){
    nz<-length(r);
    I <- diag(nz);
    if (is.vector(S)) S <- diag(S);
    
    At <- t(A%*%S);
    
    n <- -solve(At-I,r);
    return(n);
}

#'
#'@title Calculate the equilibrium size distribution for a model with 
#'2 shell conditions and 1 maturity state.
#'
#'@description Function to calculate the equilibrium size distribution for a model with 
#'2 shell conditions and 1 maturity state.
#'
#'@param r - vector of equilibrium recruits-at-size
#'@param A - size transition matrix
#'@param S - vector or diagonal matrix of survival by size class
#'@param P - vector or diagonal matrix of molting probability by size class
#'
#'@return list with equilibrium numbers-at-size vectors for new shell and old shell crab
#'
calcEquilSizeDist.21<-function(r,A,S,P){
    nz<-length(r);
    I <- diag(nz);
    if (is.vector(S)) S <- diag(S);
    if (is.vector(P)) P <- diag(P);
    
    B <- solve(I - (I-P) %*% S);#inverse of square matrix
    C <- P %*% S %*% A;
    D <- t(I - C - (I-P) %*% S %*% B %*% C);
    
    n <- solve(D,r);                #new shell
    o <- n %*% ((I-P) %*% S %*% B); #old shell
    
    n_xz <- t(array(data=c(n,o),dim=c(length(n),2)));
    return(n_xz);
}

