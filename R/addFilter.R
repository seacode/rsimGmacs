#' @title Add a file type to file extensions
#'  
#' @description Add a filter to the file extension matrix 
#' (if it's not there already) used in file dialogs.
#'  
#' @param rowname - row name to add in filters matrix
#' @param desc - brief description of the file types
#' @param filter - extension filter ("*.ext")
#' @param Filters - matrix of filters to add to
#'  
#' @return augmented Filters matrix
#' 
#' @export
#'  
#------------------------------------------------
addFilter<-function(rowname='csv',
                    desc="csv files (*.csv)",
                    filter="*.csv",
                    Filters=NULL){
    if (is.null(Filters)){
        Filters<-matrix(c(desc,filter),1,2,byrow=TRUE);
        rownames(Filters)[1]<-rowname;
    }
    if (!any(rownames(Filters)==rowname)){
            Filters<-rbind(Filters,c(desc,filter));
            rownames(Filters)[nrow(Filters)]<-rowname;
    }
    return(Filters)
}

