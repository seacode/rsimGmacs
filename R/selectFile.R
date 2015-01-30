#' @title Select a file.
#'  
#' @description Function allows the user to select a file using a gui interface.
#' 
#' @param ext - extension for files to choose from
#' @param caption - caption for file dialog (if file name not provided)
#' 
#' @return Selected file name. Returns NULL if the user canceled selection using the file dialog.
#' 
#' @importFrom tcltk tk_choose.files
#' 
#' @export
#' 
selectFile<-function(ext='*',caption=paste("Select .",ext," file to import",sep='')){
    Filters<-addFilter(ext,paste(ext,"files (*.",ext,")",sep=''),paste("*.",ext,sep=''));
    file<-tcltk::tk_choose.files(caption=caption,
                             multi=FALSE,filters=matrix(Filters[ext,],1,2,byrow=TRUE));
    if (length(file)==0) return(NULL);
    return(file)
}

#file<-selectFile();