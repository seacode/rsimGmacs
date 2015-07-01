#'
#'@title Calculate model processes
#'
#'@description Function to calculate model processes prior to running the model
#'
#'@param mc - model configuration list object
#'@param showPlot - flag (T/F) to show plots
#'
#'@return mp - list object for model processes
#'
#'@export
#'
calcModelProcesses<-function(mc,showPlot=TRUE){
    #weight-at-size
    W_yxmsz <- calcWatZ(mc,showPlot=showPlot);
    
    #calculate time-varying natural mortality
    M_yxmsz <- calcNaturalMortality(mc,showPlot=showPlot);
    
    #calculate molting probabilities
    prMolt_yxmsz<-calcPrMolt(mc,showPlot=showPlot);
    
    #calculate size transition matrix
    G_list <- calcZTM(mc,showPlot=showPlot);
    
    #calculate pr(maturity|size) [interpretations differ for KC, TC]
    prMat_yxz <- calcMaturity(mc,showPlot=showPlot);
    
    #calculate fishing mortalities
    F_list <- calcFishingMortalities(mc,showPlot=showPlot);
    
    #calculate survey catchabilities
    S_list <- calcSurveyCatchabilities(mc,showPlot=showPlot);
    
    #calculate time-varying total mortality
    Z_yxmsz <- M_yxmsz;
    for (f in mc$dims$f$nms){
        cat("adding F to Z for",f,"\n")
        for (y in mc$dims$y$nms){
            for (x in mc$dims$x$nms){
                for (m in mc$dims$m$nms){
                    for (s in mc$dims$s$nms){
                        Z_yxmsz[y,x,m,s,] <- Z_yxmsz[y,x,m,s,] + (F_list$FM_fyxmsz)[f,y,x,m,s,];
                    }
                }
            }
        }
    }
    cat(dim(Z_yxmsz),'\n\n')
    
    #calculate survival
    S_yxmsz <- exp(-Z_yxmsz);
    
    mp <- list(W_yxmsz=W_yxmsz,
               M_yxmsz=M_yxmsz,
               Z_yxmsz=Z_yxmsz,
               S_yxmsz=S_yxmsz,
               prMolt_yxmsz=prMolt_yxmsz,
               T_yxmszz=G_list$prZAM_yxmszz,
               mnMI_yxz=G_list$mnMI_yxz,
               prMat_yxz=prMat_yxz,
               F_list=F_list,
               S_list=S_list)
    
    return(mp)
}
