\name{lr_co}
\alias{lr_co}

\title{
Cochrane et al. (1980) lime requirement formula
}

\description{
Empirical formula to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
lr_co(TAS, exch_Al, exch_Ca, exch_Mg)
}

\arguments{
  \item{TAS}{Target aluminum saturation after liming (\%)}
  \item{exch_Al}{Exchangeable Aluminum (meq/100g of soil)}
  \item{exch_Ca}{Exchangeable Calcium (meq/100g of soil)}
  \item{exch_Mg}{Exchangeable Magnesium (meq/100g of soil)}
}

\details{
Exchangeable aluminum, calcium, and magnesium as extracted with a neutral unbuffered salt (e.g. 1N KCl). Note that meq/100g of soil is the same as cmol of charge per kilogram of soil.  
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
\code{\link{lr}, \link{ka_lr}, \link{nu_lr}, \link{my_lr}, \link{bv_lr}, \link{gt_lr}}, 
}

\examples{
TAS <- 20
exch_Al <- 2:6
exch_Ca <- 5:1
exch_Mg <- 3

lr_co(TAS, exch_Al, exch_Ca, exch_Mg)

TAS <- 5
lr_co(TAS, exch_Al, exch_Ca, exch_Mg)

}
