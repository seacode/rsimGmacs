# Notes on model configuration object
# 
# mc$dims
#     y
#         mny - min year
#         mxy - max year
#         n   - number of model years to simulate
#         nms - vector of years as names
#         vls - vector of years as values
#     x
#         n   - number of sexes
#         nms - names of sexes
#     m
#         n   - number of maturity states
#         nms - names of maturity states
#     s
#         n   - number of shell conditions
#         nms - names of shell conditions
#     z
#         n - number of size bins
#         nms - size bins (midpoints) as names
#         vls - size bins as values
#     zp   (same as z, but used for post-molt bins in growth matrices)
#         n - number of size bins
#         nms - size bins (midpoints) as names
#         vls - size bins as values
#     zc
#         n - number of size cutpoints
#         nms - sizebin cutpoints as names
#         vls - sizebin cutpoints as values
#     f
#         n - number of fisheries
#         nms - names of fisheries
#     v
#         n - number of surveys
#         nms - names of surveys
# mc$params
#     recruitment
#         lnR  - ln-scale mean recruitment
#         sigR - ln-scale recruitment standard deviation
#         lnXR - nominal sex ratio
#         sigXR - ln-scale standard deviation for sex ratio deviations
#         aZ    - alpha parameter for rec. size distribution
#         bZ    - beta parameter for rec. size distribution
#-----------------------------------------------------------------------------------
#'
#'@title Model configuration for Tanner crab.
#'
#'@export
#'
ModelConfiguration.TC<-function(){
    #-----type
    type<-'TC';
    #-----dimensions
    mny=1950;mxy=2014;yrs<-mny:mxy;
    zcs<-seq(from=25,to=185,by=5);
    zbs<-midpoints(zcs);
    dims<-list(y=list(n=length(yrs),nms=as.character(yrs),vls=yrs,mny=mny,mxy=mxy),
               x=list(n=2,nms=c('male','female')),
               m=list(n=2,nms=c('immature','mature')),
               s=list(n=2,nms=c('new shell','old shell')),
               z=list(n=length(zbs),nms=as.character(zbs),vls=zbs),
               zp=list(n=length(zbs),nms=as.character(zbs),vls=zbs),
               zc=list(n=length(zcs),nms=as.character(zcs),vls=zcs)
               );
    #---------------------------------------------------------------
    #-----parameters
    #weight-at-size
    a<-dimArray(list(dims=dims),'x.m')
    a['female','immature']<-0.000637;
    a['female',  'mature']<-0.000344;
    a[  'male','immature']<-0.000163;
    a[  'male',  'mature']<-0.000163;
    b<-dimArray(list(dims=dims),'x.m')
    b['female','immature']<-2.794;
    b['female',  'mature']<-2.956;
    b[  'male','immature']<-3.136;
    b[  'male',  'mature']<-3.136;
    wAtZ<-list(a_xz=a,b_xz=b);

    #natural mortality
    nm<-list(lnM=log(0.23)   #base natural mortality
             )

    #molting: probability of molting, at size by sex, shell condition. NOTE: Different from that for king crab .
    prMolt_xmsz<-dimArray(list(dims=dims),'x.m.s.z',val=0);
    for (x in dims$x$nms) {
        prMolt_xmsz[x,'immature',,]<-1;#immature crab molt, mature don't
    }
    molting<-list(prMolt_xmsz=prMolt_xmsz);
    
    #maturity: probability of molt-to-maturity, at size. NOTE: different interpretation from that for king crab!!
    mat_xz<-dimArray(list(dims=dims),'x.z')
    mat_xz[  'male',]<-c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1);                              
    mat_xz['female',]<-c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1);  
    maturity<-list(mat_xz=mat_xz);
    
    #recruitment
    rec<-list(lnR=5,    # ln-scale mean recruitment
              lnSigR  =log(0.5), #ln-scale value for ln-scale recruitment standard deviation
              lnXR    =0,     #ln-scale nominal sex ratio
              lnSigXR =NA,    #ln-scale standard deviation for sex ratio deviations
              lnAlphaZ= 4.283586562, #ln-scale alpha parameter for rec. size distribution
              lnBetaZ =-0.916290732  #ln-scale beta parameter for rec. size distribution
              )
    
    params <- list(nm=nm,rec=rec);

    #-----model configuration
    mc<-list(type=type,dims=dims,params=params)
    return(mc)
}
#-----------------------------------------------------------------------------------
#'
#'@title Model configuration for BBRKC.
#'
#'@export
#'
ModelConfiguration.BBRKC<-function(){
    #-----type
    type<-'KC';
    #-----dimensions
    mny=1975;mxy=2014;yrs<-mny:mxy;
    zcs<-seq(from=65,to=165,by=5);
    zbs<-midpoints(zcs);
    dims<-list(y=list(n=length(yrs),nms=as.character(yrs),vls=yrs,mny=mny,mxy=mxy),
               x=list(n=2,nms=c('male','female')),
               m=list(n=1,nms=c('all')),
               s=list(n=2,nms=c('new shell','old shell')),
               z=list(n=length(zbs),nms=as.character(zbs),vls=zbs),
               zp=list(n=length(zbs),nms=as.character(zbs),vls=zbs),
               zc=list(n=length(zcs),nms=as.character(zcs),vls=zcs),
               f=list(n=2,nms=c('BBRKC directed fishery','bycatch fishery')),
               v=list(n=2,nms=c('NMFS trawl survey','BSFRF survey'))
               );
    #---------------------------------------------------------------
    #-----parameters
    #weight-at-size
    a<-dimArray(list(dims=dims),'x.z')
    a['female',zbs>89]<-3.593e-7;
    a['female',zbs<90]<-4.08e-7;
    a[  'male',      ]<-4.03e-7;
    b<-dimArray(list(dims=dims),'x.z')
    b['female',zbs>89]<-2.666076;
    b['female',zbs<90]<-3.127956;
    b[  'male',      ]<-3.141334;
    wAtZ<-list(a_xz=a,
               b_xz=b);

    #natural mortality
    nm<-list(M0=0.18   #base natural mortality
             );

    #molting: probability of molting, at size. NOTE: interpretation is different from that for Tanner crab 
    mu<-dimArray(list(dims=dims),'x')
    mu[]<-c(115,159);
    cv<-dimArray(list(dims=dims),'x')
    cv[]<-c(0.2,0.01);
    molting<-list(mu=mu,
                  cv=cv
                  );
    
    #growth
    a<-dimArray(list(dims=dims),'x');
    a[c('male','female')] <- 17.5;
    b<-dimArray(list(dims=dims),'x');
    b[c('male','female')] <- 0.10;
    scale<-dimArray(list(dims=dims),'x');
    scale[c('male','female')] <- 6.0;
    growth<-list(a_x=a,
                 b_x=b,
                 scale_x=scale
                 );
    
    #maturity: probability of being mature, at size. NOTE: interpretation is different from that for Tanner crab!!
    mat_xz<-dimArray(list(dims=dims),'x.z')
    mat_xz[  'male',]<-c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1);                              
    mat_xz['female',]<-c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1);  
    maturity<-list(mat_xz=mat_xz);

    #recruitment
    rec<-list(lnR     = 7.0,  # ln-scale mean recruitment
              lnSigR  =-0.51, #ln-scale value for ln-scale recruitment standard deviation
              lnXR    =0,     #ln-scale nominal sex ratio
              lnSigXR =NA,    #ln-scale standard deviation for sex ratio deviations
              lnAlphaZ= 4.283586562, #ln-scale alpha parameter for rec. size distribution
              lnBetaZ =-0.916290732  #ln-scale beta parameter for rec. size distribution
              )
    
    #fisheries
    f1<-list(name='BBRKC directed fishery',
             blocks=list(
                        list(years=1975:2014,hm=0.3,mnF=0.3,sdF=0.4,offFX=0.1,
                             sel=list(male  =list(type='logistic',params=list(mu=100,sd=20)),
                                      female=list(type='logistic',params=list(mu= 60,sd=20))),
                             ret=list(male  =list(type='logistic',params=list(mu=120,sd=5)))
                        )
                    )
            );
    f2<-list(name='bycatch fishery',
             blocks=list(
                        list(years=1975:2014,hm=0.8,mnF=0.1,sdF=0.2,offFX=0.1,
                             sel=list(male  =list(type='logistic',params=list(mu=60,sd=20)),
                                      female=list(type='logistic',params=list(mu=40,sd=20))),
                             ret=NULL
                        )
                    )
            );
    fisheries<-list(`BBRKC directed fishery`=f1,
                    `bycatch fishery`=f2);
    
    #surveys
    s1<-list(name='NMFS trawl survey',
             blocks=list(
                        list(years=1975:2014,mnQ=0.7,sdQ=0.1,offQX=0.8,
                             sel=list(male  =list(type='logistic',params=list(mu=50,sd=20)),
                                      female=list(type='logistic',params=list(mu=40,sd=20)))
                        )
                    )
            );
    s2<-list(name='BSFRF survey',
             blocks=list(
                        list(years=c(2010, 2014),mnQ=1.0,sdQ=0.0,offQX=0.1,
                             sel=list(male  =list(type='logistic',params=list(mu=60,sd=20)),
                                      female=list(type='logistic',params=list(mu=40,sd=20)))
                        )
                    )
            );
    surveys<-list(`NMFS trawl survey`=s1,
                  `BSFRF survey`=s2);
    
    params <- list(wAtZ=wAtZ,
                   nm=nm,
                   molting=molting,
                   growth=growth,
                   maturity=maturity,
                   rec=rec,
                   fisheries=fisheries,
                   surveys=surveys);

    #-----model configuration
    mc<-list(type=type,dims=dims,params=params)
    return(mc)
}
#mc<-ModelConfiguration.BBRKC();
