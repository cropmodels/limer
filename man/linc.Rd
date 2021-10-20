\name{linc}
\alias{linc}
\docType{linc}
\title{
Soil response to liming in incubation studies
}

\description{
Soil response to increasing lime rate doses from different incubation studies  
}

\usage{data(linc)}

\format{
  A data frame with 36 observations on the following 9 variables:
  \describe{
    \item{\code{source}}{Source of the original data (see References)}
    \item{\code{soil_id}}{A unique identification code for each soil}
    \item{\code{LR}}{Lime rate in mili-equivalents of Ca per 100g of soil}
    \item{\code{exch_ac}}{Initial (before liming) exchangeable acidity (meq/100g)}
    \item{\code{ECEC}}{Effective cation exchange capacity (meq/100g of soil)}
    \item{\code{Al_sat}}{Aluminum saturation (\%) after liming \code{Al_sat = 100 * exch_ac/ECEC}}
    \item{\code{pH}}{Soil pH in water (either 1:1 v/v or 1:2.5 v/v)}
    \item{\code{OM}}{Soil organic matter (\%)}
    \item{\code{clay}}{Clay fraction in the first horizon for the given soil type}
  }
}

\details{
In many old publications (> 40y), exchangeable acidity was reported as exchangeable Aluminum. Lime rates were transformed to meq/100g of soil considering the liming material Effective Calcium Carbonate Equivalent (ECCE). When not available in the original publication, clay fraction data were obtained from USDA soil series descriptions or other sources.

Note that meq/100g of soil is the same as cmol of charge per kilogram of soil.  
}

\references{
Cochrane, T. T., Salinas, J. G., & Sanchez, P. A. (1980). An equation for liming acid mineral soils to compensate crop aluminium tolerance. Tropical Agriculture, Trinidad and Tobago, 57(2), 133-140.

Kamprath, E. J. (1970). Exchangeable aluminum as a criterion for liming leached mineral soils. Soil Science Society of America Journal, 34(2), 252-254.

Salinas, J. (1978). Differential response of some cereal and bean cultivars to Al and P stress in an Oxisol of Central Brazil. Thesis (Ph. D.).
}

\examples{

}

\keyword{datasets}
