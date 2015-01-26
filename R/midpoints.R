#'
#'@title Calculate the midpoints of a vector.
#'
#'@description Function to calculate the midpoints of a vector.
#'
#'@param x - the vector to calculate the midpoints for
#'
#'@return the vector of midpoints
#'
#'@export
#'
midpoints<-function(x){
    n<-length(x)-1;
    d<-0.5*(x[1+(1:n)]+x[1:n]);
    names(d)<-names(d);
    return(d);
}