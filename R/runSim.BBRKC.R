#'
#'@title Run the BBRKC simulation model to produce a Gmacs input file.
#'
#'@description Function to run the BBRKC simulation model to produce a Gmacs input file.
#'
#'@param fn - filename for output file (input file to gmacs)
#'@param showPlot - flag (T/F) to show plots
#'
#'@return list with elements:
#'mc - model configuration list object
#'mp - model processes list object
#'mr - model results list object
#'
#'@export
#'
runSim.BBRKC<-function(fn='gmacs.input.dat',showPlot=TRUE){
    #get model configuration for BBRKC
    mc <- readModelConfiguration();
    
    #calculate model processes
    mp <- calcModelProcesses(mc,showPlot=showPlot);
    
    #run the model
    mr <- runModel(mc,mp,showPlot=showPlot);
    
    #output results to model files
    writeSim.gmacs(mc,mp,mr,fn=fn,showPlot=showPlot);
    
    return(invisible(list(mp=mp,mc=mc,mr=mr)));
}