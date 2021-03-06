% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/juveniles_BioEnGrowth.R
\name{juv_BioEnGrowth}
\alias{juv_BioEnGrowth}
\title{Calculate growth (J/g fish/d) by weight and temperature}
\usage{
juv_BioEnGrowth(W, T, DSa = 0.125, Fa = 0.15, Ua = 0.11, EDp = 4113)
}
\arguments{
\item{W}{- weight(s) of individual fish}

\item{T}{- temperature(s)}

\item{DSa}{- fraction of assimilated energy lost to specific dynamic action}

\item{Fa}{- fraction of consumed energy lost to egestion}

\item{Ua}{- fraction of assimilated energy lost to execretion}

\item{EDp}{- energy denisty of prey (J/g prey)}
}
\value{
dataframe with columns W (weight), T (temperature),
G (growth rate), C (consumption rate), R (total respiration rate), F (egestion rate),
U (excretion rate), Ra (non-SDA repsiration rate), and SDA (specific dynamic action rate).
}
\description{
Function to calculate growth (J/g fish/d) by weight and temperature.
}
\details{
Based on eq.s S1, S2, and S3 in Supplement to Hurst et al. 2018 as corrected by
those in Cianelli et al, 1998:
\itemize{
\item{\eqn{G(W,T)   = C(W,T) - [R(W,T) + F(W,T) + U(W,T)] }, growth [J/g fish/d]}
\item{\eqn{C(W,T)   = C(W,EDp)*fC(T) }, consumption [J/g fish/d]}
\item{\eqn{R(W,T)   = Ra(W,T) + SDA(W,T) }, total respiration [J/g fish/d]}
\item{\eqn{Ra(W,T)  = R(W)*fR(T) }, non-SDA respiration}
\item{\eqn{F(W,T)   = Fa*C }, egestion [J/g fish/d]}
\item{\eqn{U(W,T)   = Ua*(C-F) }, excretion [J/g fish/d]}
\item{\eqn{SDA = DSa*(C-F) }, specific dynamic action [J/g fish/d]}
}
}
\seealso{
Other juvenile bioenergetic functions: 
\code{\link{juvBioenergetics_CompareRelationships}()},
\code{\link{juvBioenergetics_PlotRelationships}()},
\code{\link{juv_CT}()},
\code{\link{juv_Ca}()},
\code{\link{juv_GT}()},
\code{\link{juv_MT}()},
\code{\link{juv_Ra}()},
\code{\link{juv_fC}()},
\code{\link{juv_fR}()}
}
\concept{juvenile bioenergetic functions}
