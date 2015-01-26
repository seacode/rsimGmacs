#'
#'@title Calculate values for a selectivity curve
#'
#'@param type - the type of selectivity function to calculate
#'@param z - vector of values at which to calculate the function
#'@param params - the selectivity function parameters, as a list
#'
#'@return vector matching size of z, with names given by elements of z
#'
#'@export
#'
calcSelectivity<-function(type,z,params){
    if (tolower(type)=='logistic'){
        res<-plogis(z,params$mu,params$sd);
        res<-res/max(res);
    }
    return(res)
}
#-----------------------------------------------------------------------------------
#'
#'@title Calculate the logistic function
#'
#'@param z - vector of sizes at which to compute selectivities
#'@param mu - size at which selectivity  = 50% (logit-scale mean)
#'@param sd - standard deviation in selectivity (logit-scale standard deviation)
#'
#'@return vector with selectivity values at the elements of z
plogis<-function(z,mu,sd){
    #cat(z,'\n')
    #cat('mu, sd = ',mu,sd,'\n')
    res<-1.0/(1.0+exp(-(z-mu)/sd));
    #print(res);
    names(res)<-as.character(z);
    #print(res)
    return(res)
}
#-----------------------------------------------------------------------------------
