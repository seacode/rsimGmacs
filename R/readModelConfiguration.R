#'
#'@title Select and read a model configuration file.
#'
#'@description Function to select and read a model configuration file.
#'
#'@param fn - the file to read (NULL brings up a file chooser dialog)
#'@param ext - extension of type of file to select (if fn is NULL)
#'@param verbose - flag (T/F) to write parsing information to screen
#'
#'@return model configuration list object
#'
#'@export
#'
readModelConfiguration<-function(fn=NULL,ext='*',verbose=FALSE){
    #get file name to read
    if (is.null(fn)){
        fn<-selectFile(ext)
    }
    
    conn<-file(fn,open='r');
    res<-readLines(con=conn);    
    close(conn);
    
    #parse res to remove comments and collect quoted and unquoted text elements
    rsp<-parseText(res,stripComments=TRUE);
    
    if (verbose){
        cat('Parsed file:\n')
        for (i in 1:length(rsp)){cat(rsp[[i]],'\n');}
        cat('\n\n')
    }
    
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
    cat("Parsing model dimensions\n")
    lst.dims<-parseMC.Dims(rsp,i);  i<-lst.dims$i;
    dims<-lst.dims$dims;    
    
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
    cat("Parsing weight-at-size info\n")
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
    cat("Parsing natural mortality info\n")
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
    cat("Parsing molting\n")
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
    cat("Parsing growth \n")
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
    cat("Parsing maturity\n")
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
    cat("Parsing recruitment\n")
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
#     chk<-rsp[[i]][1]; i<-i+1;
#     checkKeyword(chk,'Fisheries');
#     fisheries<-list();
#     for (fp in 1:dims$f$n){
#         f<-rsp[[i]][1]; i<-i+1;
#         blocks<-list();
#         nt<-parseNum(rsp[[i]][1]); i<-i+1;
#         for (tp in 1:nt){
#             t<-rsp[[i]][1];
#             eval(parse(text=paste('years<-',t)));
#             hm   <-parseNum(rsp[[i]][2]);
#             mnF  <-parseNum(rsp[[i]][3]);
#             sdF  <-parseNum(rsp[[i]][4]);
#             offFX<-parseNum(rsp[[i]][5]);
#             i<-i+1;
#             sel<-list();
#             ret<-list();
#             nc<-parseNum(rsp[[i]][1]); i<-i+1;
#             for (ic in 1:nc){
#                 x <-rsp[[i]][1];               #sex
#                 ct<-rsp[[i]][2];               #curve type (sel or ret)
#                 ft<-rsp[[i]][3];               #function type
#                 np<-parseNum(rsp[[i]][4]);     #number of parameters for function
#                 ps<-parseNum(rsp[[i]][4+1:np]);#parameter values
#                 if (ct=='selectivity'){
#                     sel[[x]]<-list(type=ft,params=ps);
#                 } else if (ct=='retention'){
#                     ret[[x]]<-list(type=ft,params=ps);
#                 } else {
#                     cat('Unrecognized curve type "',ct,'"\n');
#                     cat('Should be "selectivity" or "retention" \n');
#                     cat('Aborting...');
#                     stop();
#                 }
#                 i<-i+1;
#             }#ic
#             block<-list(years=years,hm=hm,mnF=mnF,sdF=sdF,offFX=offFX,
#                         sel=sel,ret=ret);
#             blocks[[t]]<-block;
#         }#t
#         fisheries[[f]]<-list(name=f,blocks=blocks);
#     }#fp
    cat("Parsing fisheries\n")
    lstF<-parseMC.Fisheries(rsp,i,dims); i<-lstF$i;
    params$fisheries<-lstF$fisheries;
    
    #surveys
#     chk<-rsp[[i]][1]; i<-i+1;
#     checkKeyword(chk,'Surveys');
#     surveys<-list();
#     for (vp in 1:dims$v$n){
#         v<-rsp[[i]][1]; i<-i+1;
#         blocks<-list();
#         nt<-parseNum(rsp[[i]][1]); i<-i+1;
#         for (tp in 1:nt){
#             t<-rsp[[i]][1];
#             eval(parse(text=paste('years<-',t)));
#             mnQ  <-parseNum(rsp[[i]][2]);
#             sdQ  <-parseNum(rsp[[i]][3]);
#             offQX<-parseNum(rsp[[i]][4]);
#             i<-i+1;
#             sel<-list();
#             nc<-parseNum(rsp[[i]][1]); i<-i+1;
#             for (ic in 1:nc){
#                 x <-rsp[[i]][1];               #sex
#                 ct<-rsp[[i]][2];               #curve type
#                 ft<-rsp[[i]][3];               #function type
#                 np<-parseNum(rsp[[i]][4]);     #number of parameters for function
#                 ps<-parseNum(rsp[[i]][4+1:np]);#parameter values
#                 if (ct=='selectivity'){
#                     sel[[x]]<-list(type=ft,params=ps);
#                 } else {
#                     cat('Unrecognized curve type "',ct,'"\n');
#                     cat('Should be "selectivity"\n');
#                     cat('Aborting...');
#                     stop();
#                 }
#                 i<-i+1;
#             }#ic
#             block<-list(years=years,mnQ=mnQ,sdQ=sdQ,offQX=offQX,
#                         sel=sel);
#             blocks[[t]]<-block;
#         }#t
#         surveys[[v]]<-list(name=v,blocks=blocks);
#     }#vp
    cat("Parsing surveys data\n")
    lstS<-parseMC.Surveys(rsp,i,dims); i<-lstS$i;
    params$surveys<-lstS$surveys;

    #growth increment data
    cat("Parsing growth increment data\n")
    lstGI<-parseMC.giData(rsp,i,dims); i<-lstGI$i;
    params$giData<-lstGI$giData;
    
    #-----model configuration
    mc<-list(type=modelType,dims=dims,params=params)
    return(mc);
}


#mc<-readModelConfiguration();
