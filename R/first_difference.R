#'
#'@title Calculate the first difference of a vector.
#'
#'@description Function to calculate the first difference of a vector.
#'
#'@param x - the vector to calculate the first difference for
#'
#'@return the vector of first differences
#'
#'@export
#'
first_difference<-function(x){
    n<-length(x)-1;
    d<-x[1+(1:n)]-x[1:n];
    names(d)<-names(x)[1+(1:n)];
    return(d);
}