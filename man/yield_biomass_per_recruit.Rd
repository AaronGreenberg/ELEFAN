\name{yield_biomass_per_recruit}
\alias{yield_biomass_per_recruit}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
Yield per recruit and Biomass per recruit computations using the knife-edge and not knife-edge methods
}
\description{
yield_biomass_per_recruit is used to create a table of Yield per recruit and Biomass per recruit in function of the exploitation rate. Two methods are used: one classic using the Knife-edge method, and a new one not using Knife-edge.
}
\usage{
yield_biomass_per_recruit(M,K,Lc,Linf,Pi,pas=NULL,pas2=NULL)
}
%- maybe also 'usage' for other objects documented here. ????
\arguments{
  \item{M}{
is the natural mortality.
}
  \item{K}{
is the growth coefficient of the von Bertalanffy function. 
}
  \item{Lc}{
is the mean length at first capture.  
}
  \item{Linf}{
is the asymptotic length.
}
  \item{Pi}{
is the probability of capture for length i (used in not knife-edge method).
}
  \item{pas}{
defines the splitting of the exploitation rate vector (from 0 to 1 by pas).
}
  \item{pas2}{
defines the splitting of the length vectors (from 0 to Linf by pas2).
}
}
\details{
The user has to define M, K, Lc, Linf and Pi to run the function. Using these parameters, the function enables the computation of Yield per recruit and Biomass per recruit assessments using two distinct methods (Knife-edge, not Knife-edge).
}
\value{
yield_biomass_per_recruit returns a table of Yield per recruit and Biomass per recruit assessments for each exploitation rate class and method used.
}
\references{

}
\author{
Colleter Mathieu, Palomares Deng, Pauly Daniel.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
plot_yield_biomass_per_recruit for plotting the yield_biomass_per_recruit function results.
}
\examples{
M <- 1.2
K <- 0.6
Lc <- 12
Linf <- 25
pas <- 0.05
Pi <- c(rep(0,times=6),rep(1,times=19))
yield_biomass_per_recruit(M,K,Lc,Linf,Pi,pas)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Y/R }
\keyword{ ~B/R }
