#'
#'@title Parse text in character vector into individual elements by quotes and whitespace, 
#'keeping quoted text together in same element (with whitespace but without quotation marks).
#'
#'@description Function to parse text in character vector into individual elements by quotes and whitespace, 
#'keeping quoted text together in same element (with whitespace but without quotation marks).
#'
#'@param strv - chracter vector to parse
#'@param qt - the character indicating quotation (' or ")
#'@param keepQuotes - flag to keep the quotation marks in the output (does nothing)
#'@param removeComments - flag to remove comments prior to parsing
#'@param comment - character used to indicate the start of a comment
#'@param verbose - flag (T/F) to print details of parsing operations to screen
#'
#'@return numbered list, with elements corresponding to the equivalent vector element in strv
#'AFTER comments have been removed.
#'
#'@details Each element in the returned list is a character vector whose elements consist of
#'1) quoted text blocks and 2) unquoted text elements split by white space in the order they occur in the
#'element of the character vector.
#'
#'@export
#'
parseText<-function(strv,qt="'",
                    keepQuotes=FALSE,stripComments=TRUE,comment='#',
                    verbose=FALSE){
    kQ<-ifelse(keepQuotes,0,1);
    #strip coments, if required
    if (stripComments) {strv <- stripComments(strv,comment=comment);}
    #identify locations of quotation marks
    iv<-gregexpr(paste('[',qt,']',sep=''),strv,fixed=FALSE);
    #parse character vector by element
    lst<-list();
    for (id in 1:length(strv)){
        qs<-iv[[id]];
        nq<-length(qs);
        sublst<-list(); ctr<-1; strt<-1;
        if (verbose){
            cat("id = ",id,'\n',sep='')
            cat('stripping "',strv[id],'"\n',sep='')
            cat('qs = ',qs,'\n')
            cat('length(iq) = ',nq,'\n')
        }
        if (nq==1){
            #no quotes, so extract elements by whitespace
            if (verbose) cat('no quotes, so extract elements by whitespace!\n')
            splitstr<-strsplit(strv[id],'[[:space:]]',fixed=FALSE);
            sublst[[ctr]]<-splitstr[[1]];
            if (verbose) cat(paste('sublst[[',ctr,']]=',sep=''),sublst[[ctr]],'\n',sep='|')
            ctr<-ctr+1;
        } else {
            #quotes detected
            if (verbose) cat('quotes detected!\n')
            for (iq in 1:nq){
                inQ<-2*floor(iq/2)==iq;#flag for processing w/in quotes
                end<-qs[iq]-1;
                sub<-substr(strv[id],strt,end);
                if (verbose) cat('iq = ',iq,', inQ = ',inQ,', strt = ',strt,', end = ',end,', sub = "',sub,'"\n',sep='')
                if (inQ) {
                    #inside quoted text, so take entire string
                    if (verbose) cat('inside quoted text, so taking entire string\n')
                    sublst[[ctr]]<-sub;
                    if (verbose) cat(paste('sublst[[',ctr,']]=',sep=''),sublst[[ctr]],'\n',sep='|')
                    ctr<-ctr+1;
                } else {
                    if (verbose) cat('outside quoted text\n');
                    if (sub==''){
                        #do nothing: skip empty strings
                        if (verbose) cat('Empty string...skipping!\n')
                    } else if (gsub('[[:space:]]','',sub)!=''){
                        #don't skip non-whitespace strings
                        if (verbose) cat('non-whitespace string: "',sub,'"\n',sep='');
                        splitstr<-strsplit(sub,'[[:space:]]',fixed=FALSE);
                        sublst[[ctr]]<-splitstr[[1]];
                        if (verbose) cat(paste('sublst[[',ctr,']]=',sep=''),sublst[[ctr]],'\n',sep='|')
                        ctr<-ctr+1;
                    }
                }
                strt<-qs[iq]+1;
            }#iq
            if (verbose) cat('out of iq loop. strt =',strt,'\n')
            if (strt<=nchar(strv[id])){
                if (verbose) cat('processing final unquoted section\n')
                end<-nchar(strv[id]);
                sub<-substr(strv[id],strt,end);
                if (verbose) cat('sub: "',sub,'"\n',sep='')
                #only process non-empty strings
                if (gsub('[[:space:]]','',sub)!=''){
                    if (verbose) cat('sub not whitespace only, so split by whitespace\n')
                    splitstr<-strsplit(sub,'[[:space:]]',fixed=FALSE);
                    sublst[[ctr]]<-splitstr[[1]];
                    if (verbose) cat(paste('sublst[[',ctr,']]=',sep=''),sublst[[ctr]],'\n',sep='|')
                    ctr<-ctr+1;
                }
            }
        }
        ne<-0;
        ns<-length(sublst);
        for (i in 1:ns) {ne<-ne+length(sublst[[i]]);}
        if (ne>0){
            vc<-vector(mode='character',length=ne);
            ne<-0;
            for (i in 1:ns) {
                for (j in 1:length(sublst[[i]])){
                    ne<-ne+1;
                    vc[ne]<-sublst[[i]][j];
                }
            }
            if (length(vc[vc!=''])>0) {
                lst[[id]]<-vc[vc!=''];
            } else {
                lst[[id]]<-NA;
            }
        } else {
            lst[[id]]<-NA;
        }
        if (verbose) cat('\n')
    }#id
    return(lst);
}
