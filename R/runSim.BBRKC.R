#'
#'@title Run the BBRKC simulation model to produce a Gmacs input file.
#'
#'@description Function to run the BBRKC simulation model to produce a Gmacs input file.
#'
#'@param inpfn - input filename for rsim model configuration
#'@param outfn - filename for output file (input file to gmacs)
#'@param seed - random number generator seed
#'@param showPlot - flag (T/F) to show plots
#'@param pdf - name of pdf file to save plots to (if not NULL)
#'@param width - page width for pdf
#'@param height - page height for pdf
#'
#'@return list with elements:
#'mc - model configuration list object
#'mp - model processes list object
#'mr - model results list object
#'
#'@export
#'
runSim.BBRKC<-function(inpfn=NULL,
                       outfn='gmacs.input.dat',
                       seed=NULL,
                       showPlot=TRUE,
                       pdf=NULL,
                       width=8,
                       height=6){
    #set RNG seed
    set.seed(seed,kind='default',normal.kind='default');
    
    #create pdf for plot output, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,onefile=TRUE,width=width,height=height);
        on.exit(dev.off());
    }
                       
    #get model configuration for BBRKC
    cat("Reading model configuration file\n")
    mc <- readModelConfiguration(fn=inpfn);
    
    #calculate model processes
    cat("\n\nCalculating model processes\n")
    mp <- calcModelProcesses(mc,showPlot=showPlot);
    
    #run the model
    cat("\n\nRunning model\n")
    mr <- runModel(mc,mp,showPlot=showPlot);
    
    #output results to model files
    cat("\n\nWriting gmacs input file\n")
    writeSim.gmacs(mc,mp,mr,fn=outfn,showPlot=showPlot);
    
    return(invisible(list(mc=mc,mp=mp,mr=mr)));
}