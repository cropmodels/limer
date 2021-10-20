\name{lr_ka}
\alias{lr_ka}

\title{
Kamprath (1970) lime requirement formula
}

\description{
Empirical formula to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
lr_ka(exch_Al, lf = 1.5)
}

\arguments{
  \item{exch_Al}{Exchangeable Aluminum (meq/100g of soil)}
  \item{lf}{Lime factor (see details)}
}

\details{
Exchangeable aluminum as extracted with a neutral unbuffered salt (e.g. 1N KCl). Note that meq/100g of soil is the same as cmol of charge per kilogram of soil. The default lime factor value is 1.5, but a lime factor of 2 is recommended for sensitive crops. 
}

\value{
Recommended lime rate in milliequivalents of Calcium per 100 g of soil. 
}

\references{
Kamprath, E. J. (1970). Exchangeable aluminum as a criterion for liming leached mineral soils. Soil Science Society of America Journal, 34(2), 252-254.
}

\author{
Fernando Aramburu Merlos
}


\seealso{
\code{\link{lr}, \link{lr_co}, \link{lr_nu}, \link{lr_my}, \link{lr_bv}, \link{lr_gt}, \link{convert}}, 
}


\examples{
exch_Al <- 0:4
lr_ka(exch_Al)
lr_ka(exch_Al, lf = 2)
}
