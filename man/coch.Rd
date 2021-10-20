\name{coch}
\alias{coch}
\docType{data}
\title{
Soil response to liming by Cochrane (1980)
}

\description{
Soil response to increasing lime rate doses from incubation and field studies.  
}

\usage{data(coch)}

\format{
  A data frame with 40 observations on the following 16 variables:
  \describe{
    \item{\code{source}}{Source of the original data}
    \item{\code{soil_id}}{A unique identification code for each soil}
    \item{\code{LR_meq}}{Lime rate in mili-equivalents fo Ca per 100g of soil (unit used in incubation studies)}
    \item{\code{LR_kg.ha}}{Lime rate in kg per hectare (unit used in field studies)}
    \item{\code{Al_sat}}{Aluminum saturation (\%) after liming}
    \item{\code{LR_hat}}{Estimated lime rate as reported by Cochrane (1980). Units are meq/100g of soil for incubation studies and kg/ha for field studies.}
    \item{\code{soil.type}}{Soil type (USDA classification)}
    \item{\code{pH}}{Initial (before liming) soil pH}
    \item{\code{exch_Al}}{Initial (before liming) exchangeable Aluminum (meq/100g)}
    \item{\code{exch_Ca_Mg}}{Initial (before liming) exchangeable Calcium + Magnesium (meq/100g)}
    \item{\code{exch_Ca}}{Initial (before liming) exchangeable Calcium (meq/100g)}
    \item{\code{exch_Mg}}{Initial (before liming) exchangeable Mgnesium (meq/100g)}
    \item{\code{clay}}{Clay fraction in the first horizon for the given soil type, derived from USDA soil series description}
    \item{\code{clay_l}}{Clay fraction lower limit in the first horizon for the given soil type, derived from USDA soil series description}
    \item{\code{clay_h}}{Clay fraction upper limit in the first horizon for the given soil type, derived from USDA soil series description}
    \item{\code{method}}{Type of liming experiment: Soil incubation studies or Field experiments.}
  }
}

\details{
The exchangeable aluminum values reported by Cochrane (1980) are, actually, a measurement of the exchangeable acidity of the soil (i.e. acidity of a 1N KCl extract measured by a potentiometric titration). Estimated lime rates are provided as reported in the paper because they not always match the Cochrane (1980) formula. 
Clay fraction data were not available in Cochrane (1970), so it was obtained from USDA soil series descriptions and the original papers.

}

\references{
Cochrane, T. T., Salinas, J. G., & Sanchez, P. A. (1980). An equation for liming acid mineral soils to compensate crop aluminium tolerance. Tropical Agriculture, Trinidad and Tobago, 57(2), 133-140.

Leon, L. A., & Fenster, W. E. (1979). Management of phosphorus in the Andean countries of tropical Latin America. 

Salinas, J. (1978). Differential response of some cereal and bean cultivars to Al and P stress in an Oxisol of Central Brazil. Thesis (Ph. D.).

USDA soil series decription: Norfolk. https://soilseries.sc.egov.usda.gov/OSD_Docs/N/NORFOLK.html

USDA soil series decription: Rains. https://soilseries.sc.egov.usda.gov/OSD_Docs/R/RAINS.html

USDA soil series decription: Dunbar. https://soilseries.sc.egov.usda.gov/OSD_Docs/D/DUNBAR.html

USDA soil series decription: Portsmouth. https://soilseries.sc.egov.usda.gov/OSD_Docs/P/PORTSMOUTH.html
}

\examples{
data(coch)
head(coch)
summary(coch)
}

\keyword{datasets}
