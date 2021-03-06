% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growthIntegrationFunctions.R
\name{integrateGrowthSL}
\alias{integrateGrowthSL}
\title{Integrate SL based on a temperature time series}
\usage{
integrateGrowthSL(dt, SL, T, growthFcn)
}
\arguments{
\item{dt}{- integration time step (in days)}

\item{SL}{- initial standard length (mm)}

\item{T}{- vector of temperatures (deg C)}

\item{growthFcn}{- function calculating temperature-dependent growth rates}
}
\value{
tibble with columns age, T, and SL
}
\description{
Function to integrate SL based on a temperature time series.
}
\details{
None.
}
\examples{
## T = 5;
## D = ysl_YolkSacAbsorption(T);#--calculate stage duration at T
## vTs = 5+0*(1:100);           #--time series of temperatures
## tbl1 = integrateGrowthSL(D/100,0,vTs,ysl_GrowthRateSL)

}
