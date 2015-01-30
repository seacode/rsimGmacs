#'
#'@title Run the BBRKC simulation model
#'
#'@description Function to run the BBRKC simulation model
#'
#'@param showPlot - flag (T/F) to show plots
#'
#'@export
#'
runSim.BBRKC<-function(showPlot=TRUE){
    #get model configuration for BBRKC
    mc <- readModelConfiguration();
    
    #calculate model processes
    mp <- calcModelProcesses(mc,showPlot=showPlot);
    
    #run the model
    mr <- runModel(mc,mp,showPlot=showPlot)
    
    #output results to model files
    #TODO: implement!
}