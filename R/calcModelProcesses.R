#'
#'@title Calculate model processes
#'
calcModelProcesses<-function(mc,showPlot=TRUE){
    #weight-at-size
    W_xmsz <- calcWatZ(mc,showPlot=showPlot);
    
    #calculate time-varying natural mortality
    M_yxmsz <- calcNaturalMortality(mc,showPlot=showPlot);
    
    #calculate molting probabilities
    prMolt_xmsz<-calcPrMolt(mc,showPlot=showPlot);
    
    #calculate size transition matrix
    T_xmszz <- calcZTM(mc,showPlot=showPlot);
    
    #calculate pr(maturity|size) [interpretations differ for KC, TC]
    prMat_xz <- calcMaturity(mc,showPlot=showPlot);
    
    #calculate fishing mortalities
    F_list <- calcFishingMortalities(mc,showPlot=showPlot);
    
    #calculate survey catchabilities (TODO: implement this)
    S_list <- calcSurveyCatchabilities(mc,showPlot=showPlot);
    
    #calculate time-varying total mortality
    Z_yxmsz <- M_yxmsz;
    for (f in mc$dims$fisheries$nms){
        Z_yxmsz[,,,,] <- Z_yxmsz[,,,,] + (F_list$F_fyxmsz)[f,,,,,];
    }
    
    #calculate survival
    S_yxmsz <- exp(-Z_yxmsz);
    
    mp <- list(W_xmsz=W_xmsz,
               M_yxmsz=M_yxmsz,
               Z_yxmsz=Z_yxmsz,
               S_yxmsz=S_yxmsz,
               prMolt_xmsz=prMolt_xmsz,
               T_xmszz=T_xmszz,
               prMat_xz=prMat_xz,
               F_list=F_list,
               S_list=S_list)
    
    return(mp)
}
