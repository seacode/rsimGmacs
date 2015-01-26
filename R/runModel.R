#'
#'@title Run the simulation model
#'
#'@description Function to run the simulation model
#'
#'@param mc - model configuration object
#'@param mp - model processes object
#'@param showPlot - flag to show plots
#'
#'@return List consisting of:
#'R_yxz - recruitment by year/sex/size
#'iN_xmsz - initial population abundance by sex/maturity/shell condition/size
#'N_yxmsz - population abundance by year/sex/maturity/shell condition/size
#'F_list - list of fisheries results (see calcNatZ.Fisheries)
#'N_vyxmsz - survey catches (numbers) by survey/year/sex/maturity/shell condition/size
#'
#'@export
#'
runModel<-function(mc,mp,showPlot=TRUE){
    
    #calculate recruitment
    R_yxz <- calcRecruitment(mc,showPlot=showPlot);
    
    #calculate initial numbers-at-size
    iN_xmsz<-calcInitSizeDist(mc,mp,showPlot=showPlot);
    
    #calculate time series of population abundance
    N_yxmsz<-calcNatZ(mc,mp,iN_xmsz,R_yxz,showPlot=showPlot);
    
    #calculate fishery catches
    F_list<-calcNatZ.Fisheries(mc,mp,N_yxmsz,showPlot=showPlot);
    
    #calculate survey catches
    N_vyxmsz<-calcNatZ.Surveys(mc,mp,N_yxmsz,showPlot=showPlot);
    
    return(list(R_yxz=R_yxz,iN_xmsz=iN_xmsz,N_yxmsz=N_yxmsz,F_list=F_list,N_vyxmsz=N_vyxmsz))
}