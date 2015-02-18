# rsimGmacs
R-based simulator for testing gmacs.

The function 'runSim.BBRKC(...)' runs the R-based gmacs simulation model in a simple demonstration mode based on the model configuration file ModelConfiguration.BBRKC.dat [2 fisheries, 2 surveys, multiple time blocks]. Plots are produced of various population quantities if the runSim.BBRKC function parameter 'showPlot' is TRUE (the default). An input data file to gmacs is produced (based on the filename specified by the runSim.BBRKC function parameter 'fn'; default is 'gmacs.input.dat').

Comments from gmacs developers/users are welcome and invited.

## Required packages
The following packages are required to run rsimGmacs:
    ggplot2 (version >= 1.0.0)
    reshape2 (version >= 1.4.1)
    tcltk (version >= 3.1.2)
