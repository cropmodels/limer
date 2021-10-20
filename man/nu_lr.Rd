\name{nu_lr}
\alias{nu_lr}

\title{
NuMaSS lime requirement formula
}

\description{
Empirical formula to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
nu_lr(TAS, exch_ac, ECEC, clay)
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
\code{\link{lr}, \link{ka_lr}, \link{co_lr}, \link{my_lr}, \link{bv_lr}, \link{gt_lr}}, 
}

\examples{
TAS <- 20
exch_ac <- 2:6
ECEC <- 10
clay <- 0.2

nu_lr(TAS, exch_ac, ECEC, clay)

TAS <- 10
nu_lr(TAS, exch_ac, ECEC, clay)

TAS <- 5
nu_lr(TAS, exch_ac, ECEC, clay)


# clay effect (only when very low ECEC per clay)
TAS <- 20
clay <- 0.2
nu_lr(TAS, exch_ac, ECEC, clay)

clay <- 0.99
nu_lr(TAS, exch_ac, ECEC, clay)


exch_ac <- seq(.2,.6,.1)
ECEC <- 1
clay <- 0.2
nu_lr(TAS, exch_ac, ECEC, clay)

clay <- 0.4
nu_lr(TAS, exch_ac, ECEC, clay)

}
