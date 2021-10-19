\name{my_lime}
\alias{my_lime}

\title{
Experimental lime requirement formula
}

\description{
Empirical formula to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
my_lime(TAS, exch_ac, ECEC, a = 0.8, b = 0.2, clay = NULL)
}

\arguments{
  \item{TAS}{Target aluminum saturation after liming (\%)}
  \item{exch_ac}{Exchangeable acidity (meq/100g of soil)}
  \item{ECEC}{Effective cation exchange capacity (meq/100g of soil)}
  \item{a}{Lime factor. See Details}
  \item{b}{Lime factor. See Details}
  \item{clay}{Clay fraction of the soil {0,1} (optional)}
}

\details{
Exchangeable acidity (Al + H + ...) as extracted with a neutral unbuffered salt (e.g. 1N KCl). 

Lime factor \code{a} is the increase in sum of bases (e.g. exch_Ca + exch_Mg, meq/100g) per unit of lime added to the soil (meq/100g). Lime factor \code{b} is the decay rate at which an increase in exchangeable bases is less effective in reducing the exchangeable acidity (because of a higher ECEC) at increasing lime rates (expressing the lime rate in units of initial exchangeable acidity). 

Clay fraction is optional and might be used to adjust factor \code{a}. There is some evidence that factor \code{a} might be lower (i.e. lime is less effective to increase the sum of bases) in soils with very low clay-based ECEC. 

Note that meq/100g of soil is the same as cmol of charge per kilogram of soil.
}

\value{
Recommended lime rate in milliequivalents of Calcium per 100 g of soil. 
}

\references{
Aramburu Merlos et al., (unpublished)
}

\author{
Fernando Aramburu Merlos
}


\seealso{
\code{\link{limer}, \link{ka_lime}, \link{co_lime}, \link{nu_lime}, \link{bv_lime}, \link{gt_lime}}, 
}

\examples{
TAS <- 20
exch_ac <- 2:6
ECEC <- 10
clay <- 0.2

my_lime(TAS, exch_ac, ECEC)

TAS <- 10
my_lime(TAS, exch_ac, ECEC)

TAS <- 5
my_lime(TAS, exch_ac, ECEC)


# reducing a and b factors can result in NaN (no solution)
my_lime(TAS, exch_ac, ECEC, a = 0.7)
my_lime(TAS, exch_ac, ECEC, a = 0.5)

my_lime(TAS, exch_ac, ECEC, b = 0.1)
my_lime(TAS, exch_ac, ECEC, b = 0.22)
my_lime(TAS, exch_ac, ECEC, b = 0.25)
my_lime(TAS, exch_ac, ECEC, b = 0.3)


# clay effect (only when very low ECEC per clay)
TAS <- 20
clay <- 0.2
my_lime(TAS, exch_ac, ECEC, clay = clay)

clay <- 0.5
my_lime(TAS, exch_ac, ECEC, clay = clay)

clay <- 0.9
my_lime(TAS, exch_ac, ECEC, clay = clay)

exch_ac <- seq(.2,.6,.1)
ECEC <- 1
clay <- 0.2
my_lime(TAS, exch_ac, ECEC, clay = clay)

clay <- 0.5
my_lime(TAS, exch_ac, ECEC, clay = clay)

clay <- 0.9
my_lime(TAS, exch_ac, ECEC, clay = clay)

}
