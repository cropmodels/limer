\name{lr_nu}
\alias{lr_nu}

\title{
NuMaSS lime requirement formula
}

\description{
Empirical formula to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
lr_nu(TAS, exch_ac, ECEC, clay)
}

\arguments{
  \item{TAS}{Target aluminum saturation after liming (\%)}
  \item{exch_ac}{Exchangeable acidity (meq/100g of soil)}
  \item{ECEC}{Effective cation exchange capacity (meq/100g of soil)}
  \item{clay}{Clay fraction of the soil {0,1}}
}

\details{
Exchangeable acidity (Al + H + ...) as extracted with a neutral unbuffered salt (e.g. 1N KCl). Note that meq/100g of soil is the same as cmol of charge per kilogram of soil. The original formula was modified to get lime recommendations in meq/100g of soil. 
}

\value{
Recommended lime rate in milliequivalents of Calcium per 100 g of soil. 
}

\references{
Osmond, D. L., Smyth, T. J., Yost, R. S., Reid, W. S., Hoag, D. L., Branch, W., & Li, H. (2002). Nutrient Management Support System (NuMaSS) v. 2. Soil Management Collaborative Research Support Program. USA: United States Agency for International Development, Soil Management Collaborative Research Support Program. http://Pdf.Usaid.Gov/Pdf_docs/Pnada473.Pdf.
}

\author{
Fernando Aramburu Merlos
}


\seealso{
\code{\link{lr_ka}, \link{lr_co}, \link{lr}, \link{lr_my}, \link{lr_bv}, \link{lr_gt}, \link{convert}}, 
}

\examples{
TAS <- 20
exch_ac <- 2:6
ECEC <- 10
clay <- 0.2

lr_nu(TAS, exch_ac, ECEC, clay)

TAS <- 10
lr_nu(TAS, exch_ac, ECEC, clay)

TAS <- 5
lr_nu(TAS, exch_ac, ECEC, clay)


# clay effect (only when very low ECEC per clay)
TAS <- 20
clay <- 0.2
lr_nu(TAS, exch_ac, ECEC, clay)

clay <- 0.99
lr_nu(TAS, exch_ac, ECEC, clay)


exch_ac <- seq(.2,.6,.1)
ECEC <- 1
clay <- 0.2
lr_nu(TAS, exch_ac, ECEC, clay)

clay <- 0.4
lr_nu(TAS, exch_ac, ECEC, clay)

}
