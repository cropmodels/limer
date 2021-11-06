\name{lr_co}
\alias{lr_co}

\title{
Cochrane et al. (1980) lime requirement formula
}

\description{
Empirical formula to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
lr_co(TAS, exch_ac, ECEC)
}

\arguments{
  \item{TAS}{Target aluminum saturation after liming (\%)}
  \item{exch_ac}{Exchangeable Aluminum (meq/100g of soil)}
  \item{ECEC}{Effective cation exchange capacity (meq/100g of soil)}
}

\details{
Exchangeable acidity and cation exchange capacity as extracted with a neutral unbuffered salt (e.g. 1N KCl). Note that meq/100g of soil is the same as cmol of charge per kilogram of soil. ECEC is equal to the sum of all exchangeable cations (e.g. Al, Ca, Mg, K, Na), but only Al, Ca and Mg were actually used in the original formula.   
}

\value{
Recommended lime rate in milliequivalents of Calcium per 100 g of soil. 
}

\references{
Cochrane, T. T., Salinas, J. G., & Sanchez, P. A. (1980). An equation for liming acid mineral soils to compensate crop aluminium tolerance. Tropical Agriculture, Trinidad and Tobago, 57(2), 133-140.
}

\author{
Fernando Aramburu Merlos
}


\seealso{
\code{\link{lr_ka}, \link{lr}, \link{lr_nu}, \link{lr_my}, \link{lr_bv}, \link{lr_gt}, \link{convert}}, 
}


\examples{
TAS <- 20
exch_ac <- 2:6
exch_Ca <- 5:1
exch_Mg <- 3
ECEC <- exch_Ca + exch_Mg + exch_ac


lr_co(TAS, exch_ac, ECEC)

TAS <- 5
lr_co(TAS, exch_ac, ECEC)

}
