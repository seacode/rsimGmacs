#'
#'@title Check string against keyword and stop if unequal.
#'
#'@description Function to check string against a keyword and stop if unequal.
#'
#'@param chk - string to check
#'@param key - key word to check against
#'
#'@export
#'
checkKeyword<-function(chk,key){
    if (chk!=key){
        cat('Expected key word "',key,'" but got "',chk,'"\n');
        cat("Aborting...");
        stop();
    }
}