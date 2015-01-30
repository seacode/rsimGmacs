#'
#'@title Select and read a model configuration file.
#'
#'@description Function to select and read a model configuration file.
#'
#'@param fn - the file to read (NULL brings up a file chooser dialog)
#'@param ext - extension of type of file to select (if fn is NULL)
#'
#'@return model configuration list object
#'
#'@export
#'
readModelConfiguration<-function(fn=NULL,ext='*'){
    #get file name to read
    if (is.null(fn)){
        fn<-selectFile(ext)
    }
    
    conn<-file(fn,open='r');
    res<-readLines(con=conn);    
    close(conn);
    
    #parse res to remove comments and collect quoted and unquoted text elements
    rsp<-parseText(res,stripComments=TRUE);
    
    cat('Parsed file:\n')
    for (i in 1:length(rsp)){cat(rsp[[i]],'\n');}
    cat('\n\n')
    
    i<-1;
    chk<-rsp[[i]][1];i<-i+1;
    if (chk!='ModelConfiguration') {
        cat("First non-comment line should begin with keyword 'ModelConfiguration'.\n")
        cat('First word is "',chk,'".\n',sep='');
        cat('Aborting...\n');
        stop();
    }    
    
    #model type
    modelType<-rsp[[i]][1]; i<-i+1;
    cat("ModelType = '",modelType,"'\n",sep='');
    
    #parse model dimensions
    lst.dims<-parseDims(rsp,i);  
    dims<-lst.dims$dims;
    i<-lst.dims$i;
    
    zbs<-dims$z$vls; #size bins
    
    #--parse model parameters    
    params <- list(wAtZ=NULL,
                   nm=NULL,
                   molting=NULL,
                   growth=NULL,
                   maturity=NULL,
                   rec=NULL,
                   fisheries=NULL,
                   surveys=NULL);
    
    #weight at size
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'WatZ');
    blocks<-list();
    nt<-parseNum(rsp[[i]][1]); i<-i+1;
    for (tp in 1:nt){
        t<-rsp[[i]][1]; i<-i+1;
        eval(parse(text=paste('years<-',t)));
        a<-dimArray(list(dims=dims),'x.z');
        b<-dimArray(list(dims=dims),'x.z');
        nc<-parseNum(rsp[[i]][1]); i<-i+1;
        for (ic in 1:nc){
            xp<-rsp[[i]][1]; 
            eval(parse(text=paste('zs<-',rsp[[i]][2]))); 
            av<-parseNum(rsp[[i]][3]); 
            bv<-parseNum(rsp[[i]][4]);
            a[xp,zs]<-av;
            b[xp,zs]<-bv;
            i<-i+1;
        }
        blocks[[t]]<-list(years=years,
                          a_xz=a,
                          b_xz=b
                         );
    }#blocks
    params$wAtZ<-list(blocks=blocks);
    
    #natural mortality
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'M');
    blocks<-list();
    nt<-parseNum(rsp[[i]][1]); i<-i+1;
    for (tp in 1:nt){
        t<-rsp[[i]][1]; i<-i+1;
        eval(parse(text=paste('years<-',t)));
        mnM<-dimArray(list(dims=dims),'x');
        cvM<-dimArray(list(dims=dims),'x');
        for (xp in 1:dims$x$n){
            x<-rsp[[i]][1]; 
            mnM[x]<-parseNum(rsp[[i]][2]);;
            cvM[x]<-parseNum(rsp[[i]][3]);;
            i<-i+1;
        }
        blocks[[t]]<-list(years=years,
                          mnM=mnM,
                          cvM=cvM
                         );
    }#blocks
    params$nm<-list(blocks=blocks);
    
    #molting
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'Molting');
    blocks<-list();
    nt<-parseNum(rsp[[i]][1]); i<-i+1;
    for (tp in 1:nt){
        t<-rsp[[i]][1]; i<-i+1;
        eval(parse(text=paste('years<-',t)));
        mu<-dimArray(list(dims=dims),'x')
        cv<-dimArray(list(dims=dims),'x')
        for (xp in 1:dims$x$n){
            x<-rsp[[i]][1]; 
            mu[x]<-parseNum(rsp[[i]][2]); 
            cv[x]<-parseNum(rsp[[i]][3]); 
            i<-i+1;
        }#xp
        blocks[[t]]<-list(years=years,
                          mu=mu,
                          cv=cv
                          );
    }#blocks
    params$molting<-list(blocks=blocks);
    
    #growth
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'Growth');
    blocks<-list();
    nt<-parseNum(rsp[[i]][1]); i<-i+1;
    for (tp in 1:nt){
        t<-rsp[[i]][1]; i<-i+1;
        eval(parse(text=paste('years<-',t)));
        a<-dimArray(list(dims=dims),'x');
        b<-dimArray(list(dims=dims),'x');
        s<-dimArray(list(dims=dims),'x');
        for (xp in 1:dims$x$n){
            x<-rsp[[i]][1]; 
            a[x]<-parseNum(rsp[[i]][2]); 
            b[x]<-parseNum(rsp[[i]][3]); 
            s[x]<-parseNum(rsp[[i]][4]); 
            i<-i+1;
        }#xp
        blocks[[t]]<-list(years=years,
                          a_x=a,
                          b_x=b,
                          s_x=s
                         );
    }#blocks
    params$growth<-list(blocks=blocks);
    
    #maturity
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'Maturity');
    blocks<-list();
    nt<-parseNum(rsp[[i]][1]); i<-i+1;
    for (tp in 1:nt){
        t<-rsp[[i]][1]; i<-i+1;
        eval(parse(text=paste('years<-',t)));
        mat_xz<-dimArray(list(dims=dims),'x.z');
        for (xp in 1:dims$x$n){
            x<-rsp[[i]][1]; 
            mat_xz[x,]<-parseNum(rsp[[i]][1+(1:dims$z$n)]); 
            i<-i+1;
        }
        blocks[[t]]<-list(years=years,mat_xz=mat_xz);
    }#blocks
    params$maturity<-list(blocks=blocks);
    
    #recruitment
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'Recruitment');
    #read initialization section
    lnR     = parseNum(rsp[[i]][1]); #ln-scale mean recruitment
    cvR     = parseNum(rsp[[i]][2]); #ln-scale value for ln-scale recruitment standard deviation
    lnXR    = parseNum(rsp[[i]][3]); #ln-scale nominal sex ratio
    sdXR    = parseNum(rsp[[i]][4]); #ln-scale standard deviation for sex ratio deviations
    lnAlphaZ= parseNum(rsp[[i]][5]); #ln-scale alpha parameter for rec. size distribution
    lnBetaZ = parseNum(rsp[[i]][6]); #ln-scale beta parameter for rec. size distribution
    i<-i+1;
    inits<-list(lnR     =lnR,     #ln-scale mean recruitment
                cvR     =cvR,     #recruitment cv
                lnXR    =lnXR,    #ln-scale nominal sex ratio
                sdXR    =sdXR,    #ln-scale standard deviation for sex ratio deviations
                lnAlphaZ=lnAlphaZ,#ln-scale alpha parameter for rec. size distribution
                lnBetaZ =lnBetaZ  #ln-scale beta parameter for rec. size distribution
               );

    #read time blocks
    blocks<-list();
    nt<-parseNum(rsp[[i]][1]); i<-i+1;
    for (tp in 1:nt){
        t<-rsp[[i]][1];
        eval(parse(text=paste('years<-',t)));
        lnR     = parseNum(rsp[[i]][2]); #ln-scale mean recruitment
        cvR     = parseNum(rsp[[i]][3]); #ln-scale value for ln-scale recruitment standard deviation
        lnXR    = parseNum(rsp[[i]][4]); #ln-scale nominal sex ratio
        sdXR    = parseNum(rsp[[i]][5]); #ln-scale standard deviation for sex ratio deviations
        lnAlphaZ= parseNum(rsp[[i]][6]); #ln-scale alpha parameter for rec. size distribution
        lnBetaZ = parseNum(rsp[[i]][7]); #ln-scale beta parameter for rec. size distribution
        blocks[[t]]<-list(years=years,
                          lnR     =lnR,     #ln-scale mean recruitment
                          cvR     =cvR,     #recruitment cv
                          lnXR    =lnXR,    #ln-scale nominal sex ratio
                          sdXR    =sdXR,    #ln-scale standard deviation for sex ratio deviations
                          lnAlphaZ=lnAlphaZ,#ln-scale alpha parameter for rec. size distribution
                          lnBetaZ =lnBetaZ  #ln-scale beta parameter for rec. size distribution
                         )
        i<-i+1;
    }#blocks
    params$rec<-list(inits=inits,
                     blocks=blocks);
    
    #fisheries
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'Fisheries');
    fisheries<-list();
    for (fp in 1:dims$f$n){
        f<-rsp[[i]][1]; i<-i+1;
        blocks<-list();
        nt<-parseNum(rsp[[i]][1]); i<-i+1;
        for (tp in 1:nt){
            t<-rsp[[i]][1];
            eval(parse(text=paste('years<-',t)));
            hm   <-parseNum(rsp[[i]][2]);
            mnF  <-parseNum(rsp[[i]][3]);
            sdF  <-parseNum(rsp[[i]][4]);
            offFX<-parseNum(rsp[[i]][5]);
            i<-i+1;
            sel<-list();
            ret<-list();
            nc<-parseNum(rsp[[i]][1]); i<-i+1;
            for (ic in 1:nc){
                x <-rsp[[i]][1];               #sex
                ct<-rsp[[i]][2];               #curve type (sel or ret)
                ft<-rsp[[i]][3];               #function type
                np<-parseNum(rsp[[i]][4]);     #number of parameters for function
                ps<-parseNum(rsp[[i]][4+1:np]);#parameter values
                if (ct=='selectivity'){
                    sel[[x]]<-list(type=ft,params=ps);
                } else if (ct=='retention'){
                    ret[[x]]<-list(type=ft,params=ps);
                } else {
                    cat('Unrecognized curve type "',ct,'"\n');
                    cat('Should be "selectivity" or "retention" \n');
                    cat('Aborting...');
                    stop();
                }
                i<-i+1;
            }#ic
            block<-list(years=years,hm=hm,mnF=mnF,sdF=sdF,offFX=offFX,
                        sel=sel,ret=ret);
            blocks[[t]]<-block;
        }#t
        fisheries[[f]]<-list(name=f,blocks=blocks);
    }#fp
    params$fisheries<-fisheries;
    
    #surveys
    chk<-rsp[[i]][1]; i<-i+1;
    checkKeyword(chk,'Surveys');
    surveys<-list();
    for (vp in 1:dims$v$n){
        v<-rsp[[i]][1]; i<-i+1;
        blocks<-list();
        nt<-parseNum(rsp[[i]][1]); i<-i+1;
        for (tp in 1:nt){
            t<-rsp[[i]][1];
            eval(parse(text=paste('years<-',t)));
            mnQ  <-parseNum(rsp[[i]][2]);
            sdQ  <-parseNum(rsp[[i]][3]);
            offQX<-parseNum(rsp[[i]][4]);
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
            block<-list(years=years,mnQ=mnQ,sdQ=sdQ,offQX=offQX,
                        sel=sel);
            blocks[[t]]<-block;
        }#t
        surveys[[v]]<-list(name=v,blocks=blocks);
    }#vp
    params$surveys<-surveys
    
    #-----model configuration
    mc<-list(type=modelType,dims=dims,params=params)
    return(mc);
}

#'
#'@title Parse model dimensions information from a character string vector.
#'
#'@param rz - character string vector from which to extract model dimensions info
#'@param i - index into character string vector at which to start parsing
#'
#'@return a list with elements
#'dims : model dimensions list object
#'i    : final index + 1 into rz at end of parsing dims info
#'
#'@export
#'
parseDims<-function(rz,i){
    
    #create dims list object
    dims<-list(y=list(n=0,nms=NULL,vls=NULL,mny=0,mxy=0),
               x=list(n=0,nms=NULL),
               m=list(n=0,nms=NULL),
               s=list(n=0,nms=NULL),
               z=list(n=0,nms=NULL,vls=NULL),
               zp=list(n=0,nms=NULL,vls=NULL),
               zc=list(n=0,nms=NULL,vls=NULL),
               f=list(n=0,nms=NULL),
               v=list(n=0,nms=NULL)
               );
    
    #set text row counter
    j<-0;
    
    #years
    mny<-parseNum(rz[[i+j]][1]); j<-j+1;
    mxy<-parseNum(rz[[i+j]][1]); j<-j+1;
    cat('model years = ',mny,":",mxy,'\n',sep='')
    dims$y$mny<-mny;
    dims$y$mxy<-mxy
    dims$y$n<-mxy-mny+1;
    dims$y$vls<-mny:mxy;
    dims$y$nms<-as.character(mny:mxy);
    
    #size bins and cutpoints
    mnZC<-parseNum(rz[[i+j]][1]); j<-j+1;
    mxZC<-parseNum(rz[[i+j]][1]); j<-j+1;
    delZ<-parseNum(rz[[i+j]][1]); j<-j+1;
    dims$zc$vls<-seq(from=mnZC,to=mxZC,by=delZ);
    dims$zc$nms<-as.character(dims$zc$vls);
    dims$zc$n<-length(dims$zc$vls);
    dims$z$vls<-dims$zc$vls[(2:dims$zc$n)-1]+0.5*first_difference(dims$zc$vls);
    dims$z$nms<-as.character(dims$z$vls);
    dims$z$n<-length(dims$z$vls);
    dims$zp$vls<-dims$z$vls
    dims$zp$nms<-dims$z$nms;
    dims$zp$n<-dims$z$n;
    cat('size bins =',dims$z$nms,'\n');
    
    #sexes
    dims$x$n<-parseNum(rz[[i+j]][1]); j<-j+1;
    dims$x$nms<-rz[[i+j]][1:dims$x$n]; j<-j+1;
    cat("sexes = ",addQuotes(dims$x$nms),'\n')
    
    #maturity states
    dims$m$n<-parseNum(rz[[i+j]][1]); j<-j+1;
    dims$m$nms<-rz[[i+j]][1:dims$m$n]; j<-j+1;
    cat("maturity states = ",addQuotes(dims$m$nms),'\n')
    
    #shell condition
    dims$s$n<-parseNum(rz[[i+j]][1]); j<-j+1;
    dims$s$nms<-rz[[i+j]][1:dims$s$n]; j<-j+1;
    cat("shell condition = ",addQuotes(dims$s$nms),'\n')
    
    #fisheries
    dims$f$n<-parseNum(rz[[i+j]][1]); j<-j+1;
    dims$f$nms<-rz[[i+j]][1:dims$f$n]; j<-j+1;
    cat("fisheries = ",addQuotes(dims$f$nms),'\n')
    
    #surveys
    dims$v$n<-parseNum(rz[[i+j]][1]); j<-j+1;
    dims$v$nms<-rz[[i+j]][1:dims$v$n]; j<-j+1;
    cat("surveys = ",addQuotes(dims$v$nms),'\n')
    
    return(list(dims=dims,i=i+j));
}

#mc<-readModelConfiguration();
