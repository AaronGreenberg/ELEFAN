\name{plot_yield_biomass_per_recruit}
\alias{plot_yield_biomass_per_recruit}
%- Also NEED an '\alias' for EACH other topic documented here. ????
\title{
Plot function for Yield per recruit and Biomass per recruit computations using the knife-edge and not knife-edge methods
}
\description{
plot_yield_biomass_per_recruit is used to plot results from the yield_biomass_per_recruit function.
}
\usage{
plot_yield_biomass_per_recruit(tab_final)
}
%- maybe also 'usage' for other objects documented here. ????
\arguments{
  \item{tab_final}{
is the table returned by the yield_biomass_per_recruit function.
}
}
\details{
The function yield_biomass_per_recruit enables the computation of Yield per recruit and Biomass per recruit assessments using two distinct methods (Knife-edge, not Knife-edge). plot_yield_biomass_per_recruit enables to plot results from this function and analyze impact of using non Knife-edge method.
}
\value{
plot_yield_biomass_per_recruit returns two plots of Yield per recruit (Y/R) and Biomass per recruit (B/R) in function of the exploitation rate (E) (Knife-edge method with full lines, not with dashed lines).
}
\references{

}
\author{
Colleter Mathieu, Palomares Deng, Pauly Daniel.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
yield_biomass_per_recruit to compute the input table of this plot function.
}
\examples{
M <- 1.2
K <- 0.6
Lc <- 12
Linf <- 25
pas <- 0.05
Pi <- c(rep(0,times=6),rep(1,times=19))
plot_yield_biomass_per_recruit(yield_biomass_per_recruit(M,K,Lc,Linf,Pi,pas))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Y/R }
\keyword{ ~B/R }
