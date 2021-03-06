% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/juveniles_BioEnObservedRates.R
\name{juv_CT}
\alias{juv_CT}
\title{Calculate consumed energy (J/g fish/day) of a (nominal) 6.8 g fish as function of temperature}
\usage{
juv_CT(T, W = 6.8)
}
\arguments{
\item{T}{- temperature (deg C)}

\item{W}{- weight of fish (nominally 6.8 g)}
}
\value{
value of consumed energy in [J/g fish/day]
}
\description{
Function to calculate consumed energy (J/g fish/day) of (nominal) 6.8 g fish as function of temperature.
}
\details{
Based on eq. S9 (in J/day) in Supplement to Hurst et al. 2018:\cr
\eqn{  CT = juv_GT(T)  + juv_Ra(W)*juv_fR(T)}  [J/g fish/day], \cr
where W=6.8 g.
}
\seealso{
Other juvenile bioenergetic functions: 
\code{\link{juvBioenergetics_CompareRelationships}()},
\code{\link{juvBioenergetics_PlotRelationships}()},
\code{\link{juv_BioEnGrowth}()},
\code{\link{juv_Ca}()},
\code{\link{juv_GT}()},
\code{\link{juv_MT}()},
\code{\link{juv_Ra}()},
\code{\link{juv_fC}()},
\code{\link{juv_fR}()}
}
\concept{juvenile bioenergetic functions}
