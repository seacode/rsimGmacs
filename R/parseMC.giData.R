#'
#'@title Parse growth increment data section of a model configuration file.
#'
#'@description Function to parse the growth increment data ('giData') section of a model configuration file.
#'
#'@param rsp - parsed text list from model configuration file
#'@param i - index to start of surveys section in rsp
#'@param dims - model configuration dims list
#'
#'@return list with
#'i - index starting next section
#'giData - list object with giData info
#'
#'@export
#'
parseMC.giData<-function(rsp,i,dims){
    n <- dims$f$n;
    mny <- dims$y$mny;
    mxy <- dims$y$mxy;
    asy <- dims$y$asy;
    
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'GrowthIncrementData');
    
    giData<-list();
    nt<-parseNum(rsp[[i]][1]); i<-i+1;
    if (nt>0){
        for (it in 1:nt){
            t<-rsp[[i]][1];
            eval(parse(text=paste('years<-',t)));
            cv       <-parseNum(rsp[[i]][2]);
            addObsErr<-as.logical(rsp[[i]][3]);
            i<-i+1;
            block<-list(years=years,cv=cv,addObsErr=addObsErr);
            giData[[t]]<-block;
        }#t
    }#nt>0
    return(list(i=i,giData=giData));
}
