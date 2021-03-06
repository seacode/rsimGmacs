% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/calcNatZ.Fisheries.R
\name{calcNatZ.Fisheries}
\alias{calcNatZ.Fisheries}
\title{Calculate fishery catches, mortality at size through the model time interval}
\usage{
calcNatZ.Fisheries(mc, mp, N_yxmsz, showPlot = TRUE)
}
\arguments{
\item{mc}{- model configuration list object}

\item{mp}{- model processes list object}

\item{N_yxmsz}{- numbers at size array}
}
\value{
list with the following elements:
NC_fyxmsz: 6-d array of fishery catches (NOT MORTALITY) by year/sex/maturity/shell condition/size
NM_fyxmsz: 6-d array of fishery mortality as numbers by year/sex/maturity/shell condition/size
NR_fyxmsz: 6-d array of retention mortality by year/sex/maturity/shell condition/size
ND_fyxmsz: 6-d array of discard mortality by year/sex/maturity/shell condition/size
}
\description{
Function to calculate fishery catches, mortality at size through the model time interval
}
\details{

}

