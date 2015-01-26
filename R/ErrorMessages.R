#'
#'@title Throw an unrecognized model type error.
#'
#'@param type - model type
#'@param str - string to print (name of calling function)
#'
#'@details Stops the R run.
#'
throwModelTypeError<-function(type,str){
    cat('Model type not recognized in ',str,'.\n',sep='')
    cat("Type was '",type,"'.\n",sep='')
    cat("Aborting...\n");
    stop();
}