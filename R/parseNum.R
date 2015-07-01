#'
#'@title Parse numeric expressions from a character vector.
#'
#'@title Function to parse numeric expressions from a character vector.
#'
#'@param str - character vector to parse
#'
#'@return parsed and evaluated numerical expressions as a vector
#'
#'@export
#'
parseNum<-function(str){
    n<-length(str);
    res<-list();
    for (i in 1:n){
        expr<-parse(text=paste('tmp<-',str[i]))
        eval(expr);
        res[[i]]<-tmp;
    }
    return(unlist(res));
}
