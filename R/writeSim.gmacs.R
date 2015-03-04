#'
#'@title Write simulation results to gmacs input file.
#'
#'@description Function to write simulation results to gmacs input file.
#'
#'@param mc - model configuration list object
#'@param mp - model processes list object
#'@param mr - model results list object
#'@param fn - output file name
#'@param showPlot - flag to show plots
#'
#'@import ggplot2
#'@import reshape2
#'
#'@export
#'
writeSim.gmacs<-function(mc,mp,mr,fn='gmacs.input.dat',showPlot=TRUE){
    d<-mc$dims;
    mxyc<-as.character(d$y$mxy);
    
    conn<-file(fn,open="w");
    str<-c("#========================================================================================================",                                       
           "#========================================================================================================",                                                                              
           "# Gmacs Main  Data  File  Version 1.1:  rsimGmacs BBRKC Example",
           "# GEAR_INDEX   DESCRIPTION");
    cat(str,sep='\n',file=conn);
    for (f in 1:d$f$n){
        cat('#   ',f,'     ',d$f$nms[f],'\n',sep='',file=conn)
    }
    for (v in 1:d$v$n){
        cat('#   ',d$v$n+v,'     ',d$v$nms[v],'\n',sep='',file=conn)
    }
    cat('\n',sep='',file=conn);
    cat('# Fisheries: ',sep='',file=conn); cat(paste(1:d$f$n,      d$f$nms,sep=' ',collapse=', '),file=conn); cat('\n',sep='',file=conn);
    cat('# Surveys  : ',sep='',file=conn); cat(paste(d$v$n+1:d$v$n,d$v$nms,sep=' ',collapse=', '),file=conn); cat('\n',sep='',file=conn);
    cat("#========================================================================================================\n",file=conn);                                                                               
                                                                                
    cat(d$y$mny,"  # Start year","\n",file=conn);
    cat(d$y$mxy,"  # End year","\n",file=conn);
    cat(1      ,"  # Time-step (years)","\n",file=conn); 
    ng<-d$f$n+d$v$n;
    cat(ng,     "  # Number  of  distinct  data  groups  (among  fishing fleets  and surveys)","\n",file=conn);
    cat(d$x$n,  "  # Number  of  sexes","\n",file=conn);
    cat(d$s$n,  "  # Number  of  shell condition types","\n",file=conn);
    cat(d$m$n,  "  # Number  of  maturity types","\n",file=conn);
    cat(d$z$n,  "  # Number  of  size classes in the model","\n",file=conn);
    cat('##\n# size_breaks (a  vector  giving  the break points  between size  intervals,  dim=nclass+1) \n',file=conn);
    cat(d$zc$vls,"\n",sep=' ',file=conn);
    
    p<-mc$params$wAtZ$blocks[[1]];
    cat("# weight-at-length  allometry w_l = a•l^b","\n",sep=' ',file=conn);
    cat("##  a(",paste(d$x$nms,collapse=', '),")","\n",sep='',file=conn);
    cat(p$a_xz[,1],'\n',sep=' ',file=conn);
    cat("##  b(",paste(d$x$nms,collapse=', '),")","\n",sep='',file=conn);
    cat(p$b_xz[,1],'\n',sep=' ',file=conn);
    
    cat('# Male  mature  weight-at-length  (weight * proportion  mature)\n',file=conn);
    wAtZ.male<-mp$W_yxmsz[mxyc,'male',1,1,]*mp$prMat_yxz[mxyc,'male',];
    cat(wAtZ.male,'\n',sep=' ',file=conn);
    cat('# Proportion  mature  by  sex.\n',file=conn);
    prMat<-mp$prMat_yxz[mxyc,,];
    for (x in 1:d$x$n) {cat(prMat[x,],'\n',sep=' ',file=conn);}
    cat('# Fishing fleet names (delimited  with  : no  spaces  in  names)\n',file=conn);
    cat(paste(gsub('[[:blank:]]','_',d$f$nms),collapse=':'),'\n',file=conn);
    cat('# Survey  names (delimited  with  : no  spaces  in  names)\n',file=conn);
    cat(paste(gsub('[[:blank:]]','_',d$v$nms),collapse=':'),'\n',file=conn);
    
    #aggregated fishery data
    #--Retained catch biomass (1000s mt)
    BR_fy<-dimArray(mc,'f.y');     #retained biomass by f, y
    W_yxmsz<-mp$W_yxmsz;           #weight-at-size retained
    NR_fyxmsz<-mr$F_list$NR_fyxmsz;#numbers retained
    for (f in d$f$nms){
        for (y in d$y$nms){
            for (x in d$x$nms){
                for (m in d$m$nms){
                    for (s in d$s$nms){
                        BR_fy[f,y]<-BR_fy[f,y]+drop(W_yxmsz[y,x,m,s,]%*%NR_fyxmsz[f,y,x,m,s,]);
                    }#s
                }#m
            }#x
        }#y
    }#f
    #calc number of data rows that will be output
    nrrf<-0*(1:d$f$n);
    for (f in 1:d$f$n){
        for (y in d$y$nms) {if (sum(BR_fy[f,y])>0){nrrf[f]<-nrrf[f]+1;}}
    }
    nrrf<-nrrf[nrrf>0];#only data frames with non-zero rows will be output
    if (showPlot){
        mdfr<-melt(BR_fy,value.name='val');
        p <- ggplot(aes(x=y,y=val,color=f,shape=f),data=mdfr);
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + labs(x='year',y="Retained Catch Biomass (1000's mt)")
        p <- p + guides(color=guide_legend('fishery',order=1),shape=guide_legend(''))
        print(p);
    }
    #--Discard catch numbers
    ND_fyx<-dimArray(mc,'f.y.x');  #discard numbers by f, x, y (NOT mortality)
    NC_fyxmsz<-mr$F_list$NC_fyxmsz;#numbers captured
    for (f in d$f$nms){
        for (y in d$y$nms){
            for (x in d$x$nms){
                for (m in d$m$nms){
                    for (s in d$s$nms){
                        ND_fyx[f,y,x]<-ND_fyx[f,y,x]+sum(NC_fyxmsz[f,y,x,m,s,]-NR_fyxmsz[f,y,x,m,s,]);
                        #BD_fyx[f,y,x]<-BD_fyx[f,y,x]+drop(W_yxmsz[y,x,m,s,]%*%(NC_fyxmsz[f,y,x,m,s,]-NR_fyxmsz[f,y,x,m,s,]));
                    }#s
                }#m
            }#x
        }#y
    }#f
    if (showPlot){
        mdfr<-melt(ND_fyx,value.name='val');
        p <- ggplot(aes(x=y,y=val,color=f,shape=x),data=mdfr);
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + labs(x='year',y="Discard Catch Numbers (millions)")
        p <- p + guides(color=guide_legend('fishery',order=1),shape=guide_legend('sex'))
        print(p);
    }
    #calc number of rows to be output
    nrdf<-0*(1:(d$f$n*d$x$n));
    for (f in 1:d$f$n){
        for (y in d$y$nms) {
            for (x in 1:d$x$n){
                if (sum(ND_fyx[v,y,])>0){
                    idx<-(f-1)*d$f$n+x
                    nrdf[idx]<-nrdf[idx]+1;
                }
            }
        }
    }
    nrdf<-nrdf[nrdf>0];#only data frames with non-zero rows will be output
    cat('# Number  of  catch data  frames\n',file=conn);
    cat(length(nrrf)+length(nrdf),"\n",file=conn);                                                                               
    cat('# Number  of  rows  in  each  data  frame.\n',file=conn);
    cat(nrrf,nrdf,"\n",sep=' ',file=conn);                                                                          
    cat('##  ————————————————————————————————————————————————————————————————————————————————————  ##\n',file=conn);
    cat('##  CATCH DATA\n',file=conn);
    cat('##  Type  of  catch:  1 = retained, 2 = discard,  3 =\n',file=conn);
    cat('##  Units of  catch:  1 = biomass,  2 = numbers\n',file=conn);
    cat('##  for BBRKC Units are in  1000  mt  for landed  & million crabs for discards.\n',file=conn);
    cat('##  ————————————————————————————————————————————————————————————————————————————————————  ##\n',file=conn);
    #write out retained data
    sez<-1;sex<-1;fcv<-0.05;tpe<-1;unt<-1;mlt<-1;eff<-0;dm <-0;
    for (f in 1:d$f$n){
        if (sum(BR_fy[f,])>0){
            cat('##',d$f$nms[f],'retained catch\n',file=conn);
            cat('##  year  seas  fleet sex obs cv  type  units mult  effort  discard_mortality\n',file=conn);
            flt<-f;
            for (y in d$y$nms){
                if (BR_fy[f,y]>0){
                    cat(y,sez,flt,sex,BR_fy[f,y],fcv,tpe,unt,mlt,eff,dm,'\n',file=conn)
                }
            }#y
        }
    }#f
    #write out dsicard data
    sez<-1;fcv<-0.05;tpe<-2;unt<-2;mlt<-1;eff<-0;
    for (f in 1:d$f$n){
        if (sum(ND_fyx[f,,])>0){
            for (x in 1:d$x$n){
                cat('##',d$f$nms[f],d$x$nms[x],'discard catch\n',file=conn);
                cat('##  year  seas  fleet sex obs cv  type  units mult  effort  discard_mortality\n',file=conn);
                for (y in d$y$nms){
                    hm<-mp$F_list$hm_fy[f,y];
                    if (ND_fyx[f,y,x]>0){
                        cat(y,sez,f,x,ND_fyx[f,y,x],fcv,tpe,unt,mlt,eff,hm,'\n',file=conn)
                    }
                }#y
            }#x
        }
    }#f
    
    #aggregated survey data
    cat('##  ————————————————————————————————————————————————————————————————————————————————————  ##\n',file=conn);
    cat('##  RELATIVE  ABUNDANCE DATA\n',file=conn);
    cat('##  Units of  Abundance:  1 = biomass,  2 = numbers\n',file=conn);
    cat('##  TODO: add column for maturity for terminal molt life-histories\n',file=conn);
    cat('##  for BBRKC Units are in  million crabs for Abundance.\n',file=conn);
    cat('##  ————————————————————————————————————————————————————————————————————————————————————  ##\n',file=conn);
    cat('##  Number  of  relative  abundance indicies\n',file=conn);
    cat(d$v$n,'\n',file=conn);
    #--Survey abundance numbers
    N_vyx<-dimArray(mc,'v.y.x'); #survey numbers by y, x
    N_vyxmsz<-mr$N_vyxmsz;       #survey abundance by y,x,m,s,z
    for (v in d$v$nms){
        for (y in d$y$nms){
            for (x in d$x$nms){
                for (m in d$m$nms){
                    for (s in d$s$nms){
                        N_vyx[v,y,x]<-N_vyx[v,y,x]+sum(N_vyxmsz[v,y,x,m,s,]);
                        #B_vyx[v,y,x]<-B_vyx[v,y,x]+drop(W_yxmsz[y,x,m,s,]%*%Q_vyxmsz[f,y,x,m,s,]);
                    }#s
                }#m
            }#x
        }#y
    }#v
    if (showPlot){
        mdfr<-melt(N_vyx,value.name='val');
        p <- ggplot(aes(x=y,y=val,color=v,shape=x),data=mdfr);
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + labs(x='year',y="Survey Abundance (millions)")
        p <- p + guides(color=guide_legend('survey',order=1),shape=guide_legend('sex'))
        print(p);
    }
    nrv<-0*(1:d$v$n);
    for (v in 1:d$v$n){
        for (y in d$y$nms) {
            for (x in 1:d$x$n){if (sum(N_vyx[v,y,])>0){nrv[v]<-nrv[v]+1;}}
        }
    }
    cat('##  Number  of  rows  in  each  index\n',file=conn);
    cat(nrv,'\n',sep=' ',file=conn);
    cat('# Survey  data  (abundance  indices,  units are millions  of  crabs)\n',file=conn);                                                              
    sez<-1;vcv<-0.15;unt<-1;
    for (v in 1:d$v$n){
        if (sum(N_vyx[v,,])>0){
            for (x in 1:d$x$n){
                cat('##',d$v$nms[v],d$x$nms[x],'survey abundance data\n',file=conn);
                cat('##  Year, Seas, Fleet,  Sex,  Abundance,  CV     units \n',file=conn);
                for (y in d$y$nms){
                    if (N_vyx[v,y,x]>0){
                        cat(y,sez,v+d$f$n,x,N_vyx[v,y,x],vcv,unt,'\n',file=conn)
                    }
                }#y
            }#x
        }
    }#v

    #size frequencies
    #calc number of "data frames" and rows/data frame to be output for retained, discarded, survey catch data
    nrrf_fxms<-dimArray(mc,'f.x.m.s');
    nrdf_fxms<-dimArray(mc,'f.x.m.s');
    nrv_vxms <-dimArray(mc,'v.x.m.s');
    for (x in 1:d$x$n){
        for (m in 1:d$m$n){
            for (s in 1:d$s$n){
                for (y in d$y$nms) {
                    for (f in 1:d$f$n){
                        if (sum(NR_fyxmsz[f,y,x,m,s,])>0){
                            nrrf_fxms[f,x,m,s]<-nrrf_fxms[f,x,m,s]+1;
                        }
                        if (sum(NC_fyxmsz[f,y,x,m,s,]-NR_fyxmsz[f,y,x,m,s,])>0){
                            nrdf_fxms[f,x,m,s]<-nrdf_fxms[f,x,m,s]+1;
                        }
                    }#f
                    for (v in 1:d$v$n){
                        if (sum(N_vyxmsz[v,y,x,m,s,])>0){
                            nrv_vxms[v,x,m,s]<-nrv_vxms[v,x,m,s]+1;
                        }
                    }#v
                }#y
            }#s
        }#m
    }#x
    cat('##  Number  of  length  frequency matrices\n',file=conn);
    nzfs<-length(nrrf_fxms[nrrf_fxms>0])+length(nrdf_fxms[nrdf_fxms>0])+length(nrv_vxms[nrv_vxms>0]);
    cat(nzfs,'\n',file=conn);#TODO: is this correct?!
    cat('##  Number  of  rows  in  each  matrix\n',file=conn);
    for (f in 1:d$f$n){
        for (x in 1:d$x$n){
            for (m in 1:d$m$n){
                for (s in 1:d$s$n){
                    if (nrrf_fxms[f,x,m,s]>0) cat(nrrf_fxms[f,x,m,s],' ',file=conn);
                }#s
            }#m
        }#x
    }#f
    for (f in 1:d$f$n){
        for (x in 1:d$x$n){
            for (m in 1:d$m$n){
                for (s in 1:d$s$n){
                    if (nrdf_fxms[f,x,m,s]>0) cat(nrdf_fxms[f,x,m,s],' ',file=conn);
                }#s
            }#m
        }#x
    }#f
    for (v in 1:d$v$n){
        for (x in 1:d$x$n){
            for (m in 1:d$m$n){
                for (s in 1:d$s$n){
                    if (nrv_vxms[v,x,m,s]>0) cat(nrv_vxms[v,x,m,s],' ',file=conn);
                }#s
            }#m
        }#x
    }#v
    cat('\n',file=conn);#end of output line  TODO: is this fixed??!!
    cat('##  Number  of  bins  in  each  matrix  (columns  of  size  data)\n',file=conn);
    nzbs<-d$z$n+0*(1:nzfs);
    cat(nzbs,'\n',file=conn);#TODO: is this fixed??!!
    cat('##  SIZE  COMPOSITION DATA  FOR ALL FLEETS\n',file=conn)
    cat('##  ————————————————————————————————————————————————————————————————————————————————————  ##\n',file=conn)
    cat('##  SIZE  COMP  LEGEND\n',file=conn)
    cat('##  Sex:  1 = male, 2 = female, 0 = both  sexes combined\n',file=conn)
    cat('##  Type  of  composition:  1 = retained, 2 = discard,  0 = total composition\n',file=conn)
    cat('##  Maturity  state:  1 = immature, 2 = mature, 0 = both  states  combined\n',file=conn)
    cat('##  Shell condition:  1 = new shell,  2 = old shell,  0 = both  shell types combined\n',file=conn)
    cat('##  ————————————————————————————————————————————————————————————————————————————————————  ##\n',file=conn);
    #write out retained catch size frequencies
    sez<-1;tpe<-1;
    for (f in 1:d$f$n){
        for (x in 1){#males only
            for (m in 1:d$m$n){
                for (s in 1:d$s$n){
                    if (sum(BR_fy[f,])>0){
                        cat('##',d$f$nms[f],'retained catch size frequencies for',d$x$nms[x],d$m$nms[m],d$s$nms[s],'\n',file=conn);
                        cat('##  Year, Seas, Fleet,  Sex,  Type, Shell,  Maturity, Nsamp,  DataVec\n',file=conn);
                        for (y in d$y$nms){                
                            if (sum(NR_fyxmsz[f,y,x,m,s,])>0){
                                cat(y,sez,f,x,tpe,s,m,200,NR_fyxmsz[f,y,x,m,s,],'\n',file=conn)
                            }
                        }#y
                    }#s
                }#m
            }#x
        }
    }#f
    #write out discard catch size frequencies
    sez<-1;tpe<-2;
    for (f in 1:d$f$n){
        for (x in 1:d$x$n){
            for (m in 1:d$m$n){
                for (s in 1:d$s$n){
                    if (sum(ND_fyx[f,,])>0){
                        cat('##',d$f$nms[f],'discard catch size frequencies for',d$x$nms[x],d$m$nms[m],d$s$nms[s],'\n',file=conn);
                        cat('##  Year, Seas, Fleet,  Sex,  Type, Shell,  Maturity, Nsamp,  DataVec\n',file=conn);
                        for (y in d$y$nms){                
                            if (sum(NC_fyxmsz[f,y,x,m,s,]-NR_fyxmsz[f,y,x,m,s,])>0){
                                cat(y,sez,f,x,tpe,s,m,200,NC_fyxmsz[f,y,x,m,s,]-NR_fyxmsz[f,y,x,m,s,],'\n',file=conn)
                            }
                        }#y
                    }#s
                }#m
            }#x
        }
    }#f
    #write out survey size frequencies
    sez<-1;tpe<-1;#TODO: fix tpe
    for (v in 1:d$v$n){
        for (x in 1:d$x$n){
            for (m in 1:d$m$n){
                for (s in 1:d$s$n){
                    if (sum(N_vyx[v,,])>0){
                        cat('##',d$v$nms[v],'survey catch size frequencies for',d$x$nms[x],d$m$nms[m],d$s$nms[s],'\n',file=conn);
                        cat('##  Year, Seas, Fleet,  Sex,  Type, Shell,  Maturity, Nsamp,  DataVec\n',file=conn);
                        for (y in d$y$nms){                
                            if (sum(N_vyxmsz[v,y,x,m,s,])>0){
                                cat(y,sez,v+d$f$n,x,tpe,s,m,200,N_vyxmsz[v,y,x,m,s,],'\n',file=conn)
                            }
                        }#s
                    }#m
                }#x
            }#y
        }
    }#v
    
    #growth increment data (TODO: add GI data as an option)
    cat('##  Growth increment data (currently none)\n',file=conn);
    cat('##  nobs_growth\n',file=conn);
    cat('0\n',file=conn);
    cat('##  eof\n',file=conn);
    cat('9999\n',file=conn);       
    close(conn);    
}