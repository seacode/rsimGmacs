#'
#'@title Make multiple simulation runs.
#'
#'@description Funtion to make multiple simulation runs, saving the Gmacs input file and simulation output from each in a separate folder.
#'
#'@param nSim - number of simulations to run
#'@param seeds - vector (or NA or NULL) of random number generator seeds
#'@param inpfn - input filename for rsim model configuration
#'@param outfn - filename for output file (input file to gmacs)
#'@param topDir - top level directory/folder for saving results
#'@param subDirBase - base name for sequentially-numbered sub-directories/folders
#'@param showPlot - flag (T/F) to show/print plots
#'@param pdf - name of pdf file in each subfolder to save plots to (if not NULL)
#'@param width - page width for pdf
#'@param height - page height for pdf
#'
#'@return vector of seed values used (or NULL, if specified as such).
#'
#'@details Results from each model run is written to a separate (sequentially-numbered) subfolder
#'below that specified by topDir. If showPlot=TRUE, plots for a model run are saved to a pdf file
#'if 'pdf' is not NULL, otherwise plots are shown on the screen device.
#'
#'If seeds is NA, then nSim seeds are drawn from a uniform distribution [1,100000000].
#'If seeds is NULL, then each seed is based on the system time.
#'
#'@export
#'
makeMultipleRuns.BBRKC<-function(nSim=1,
                                 seeds=NA,
                                 inpfn=NULL,
                                 outfn='gmacs.input.dat',
                                 topDir='.',
                                 subDirBase="BBRKC",
                                 showPlot=FALSE,
                                 pdf=NULL,
                                 width=8,
                                 height=6){
    if (is.na(seeds)) {
        #create seeds using a uniform random number generator
        seeds<-floor(runif(nSim,min=1,max=100000000));
    } else if (!is.null(seeds)){
        #extend seeds to required length by replication
        seeds<-rep(seeds,nSim);
    }
    ndig<-ceiling(log10(nSim))+1;
    for (i in 1:nSim){
        seed<-ifelse(is.null(seeds),NULL,seeds[i]);
        sub<-paste(subDirBase,formatC(i,format='d',width=ndig,flag='0'),sep='');
        sub<-file.path(topDir,sub);
        cat("Creating subdir '",sub,"'. Using seed ",seed,".\n",sep='');
        dir.create(sub,showWarnings=FALSE);
        outfnp<-file.path(sub,outfn);
        pdfp<-ifelse(is.null(pdf),NULL,file.path(sub,pdf));
        rsim<-runSim.BBRKC(inpfn=inpfn,outfn=outfnp,seed=seed,
                           showPlot=showPlot,pdf=pdfp,width=width,height=height);
        save(rsim,file=file.path(sub,'rsim.RData'))
    }
    return(seeds);
}