#'
#'@title Parse surveys section of a model configuration file.
#'
#'@description Function to parse the 'surveys' section of a model configuration file.
#'
#'@param rsp - parsed text list from model configuration file
#'@param i - index to start of surveys section in rsp
#'@param dims - model configuration dims list
#'
#'@return list with
#'i - index starting next section
#'surveys - list object with surveys info
#'
#'@export
#'
parseMC.Surveys<-function(rsp,i,dims){
    n <- dims$f$n;
    mny <- dims$y$mny;
    mxy <- dims$y$mxy;
    
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'Surveys');
    surveys<-list();
    for (vp in 1:n){
        v<-rsp[[i]][1]; i<-i+1;#survey name
        blocks<-list();
        nt<-parseNum(rsp[[i]][1]); i<-i+1;
        for (tp in 1:nt){
            t<-rsp[[i]][1];
            eval(parse(text=paste('years<-',t)));
            mnQ  <-parseNum(rsp[[i]][2]);
            sdQ  <-parseNum(rsp[[i]][3]);
            offQX<-parseNum(rsp[[i]][4]);
            cv   <-parseNum(rsp[[i]][5]);
            addObsErr<-as.logical(rsp[[i]][6]);
            i<-i+1;
            sel<-list();
            nc<-parseNum(rsp[[i]][1]); i<-i+1;
            for (ic in 1:nc){
                x <-rsp[[i]][1];               #sex
                ct<-rsp[[i]][2];               #curve type
                ft<-rsp[[i]][3];               #function type
                np<-parseNum(rsp[[i]][4]);     #number of parameters for function
                ps<-parseNum(rsp[[i]][4+1:np]);#parameter values
                if (ct=='selectivity'){
                    sel[[x]]<-list(type=ft,params=ps);
                } else {
                    cat('Unrecognized curve type "',ct,'"\n');
                    cat('Should be "selectivity"\n');
                    cat('Aborting...');
                    stop();
                }
                i<-i+1;
            }#ic
            block<-list(years=years,
                        mnQ=mnQ,sdQ=sdQ,offQX=offQX,cv=cv,addObsErr=addObsErr,
                        sel=sel);
            blocks[[t]]<-block;
        }#t
        surveys[[v]]<-list(name=v,blocks=blocks);
    }#vp
    return(list(i=i,surveys=surveys));
}
