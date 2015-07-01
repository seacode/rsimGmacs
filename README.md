# rsimGmacs
R-based simulator for testing gmacs.

* The function 'runSim.BBRKC(...)' runs the R-based gmacs simulation model.

* The function 'makeMultipleRuns.BBRKC(...)' allows the user to make multiple runs and save the output (i.e., the input file for a gmacs model run) in separate, sequentially numbered subfolders.

The simulation is based on a user-specified "model configuration"" input file (the 'inpfn' parameter for the function runSim.BBRKC, or one selected by the user via a file chooser GUI). When running the simulation, plots can be produced of various population quantities if the runSim.BBRKC parameter 'showPlot' is TRUE (the default), and optionally saved as a pdf file if the filename is specified using the 'pdf' parameter. An input data file to gmacs is produced (based on the filename specified by the runSim.BBRKC function parameter 'outfn'; the default is 'gmacs.input.dat').

Two example input configuration files are provided to create simple demonstration models (2 fisheries, 2 surveys, multiple time blocks) are provided ('ModelConfiguration.BBRKC.dat', 'ModelConfiguration.BBRKC.1.dat'), as well as the resulting output files ('gmacs.input.dat', 'gmacs.input.1.dat'). 

Comments from gmacs developers/users are welcome and invited.

NOTE: Units for recruitment are assumed to be in millions of individuals and units for individual weights-at-size in kg, so that output units are in millions of individuals for abundance-related quantities and in 1000's t for biomass-related quantities.

## Required packages
The following packages are required to run rsimGmacs:  

* ggplot2 (version >= 1.0.0)  
* reshape2 (version >= 1.4.1)  
* tcltk (version >= 3.1.2)  
