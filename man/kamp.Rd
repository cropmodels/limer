\name{kamp}
\alias{kamp}
\docType{data}
\title{
Soil response to liming by Kamprath (1970)
}

\description{
Response of 4 North Carolina soils to increasing lime rate doses as measured in an incubation study. 
}

\usage{data(kamp)}

\format{
  A data frame with 20 observations on the following 9 variables:
  \describe{
    \item{\code{soil}}{Soil type name (series)}
    \item{\code{lr}}{Lime rate (no units) expressed as fraction of the initial exchangeable aluminum. For each soil, lime rates in meq/100g of soil can be computed as \code{lr} times the initial Al}
    \item{\code{OM}}{Inital organic matter content (\%)}
    \item{\code{pH}}{Soil pH determined on a 1:1 soil-solution ratio}
    \item{\code{Al}}{Exchangeable aluminum (meq/100g of soil) or exchangeable acidity (see Details) extracted with a neutral unbuffered salt (1N KCl).}
    \item{\code{Ca_Mg}}{Exchangeable calcium + exchangeable magnesium (meq/100g) extracted with a neutral unbuffered salt (1N KCl)}
    \item{\code{clay}}{Clay fraction in the first horizon for the given soil type, derived from USDA soil series description}
    \item{\code{clay_l}}{Clay fraction lower limit in the first horizon for the given soil type, derived from USDA soil series description}
    \item{\code{clay_h}}{Clay fraction upper limit in the first horizon for the given soil type, derived from USDA soil series description}
  }
}

\details{
The exchangeable aluminum values reported by Kamprath (1970) are, actually, a measurement of the exchangeable acidity of the soil (i.e. acidity of a 1N KCl extract measured by a potentiometric titration). Clay fraction data were not available in Kamprath (1970), so it was obtained from USDA soil series descriptions.

}

\references{
Kamprath, E. J. (1970). Exchangeable aluminum as a criterion for liming leached mineral soils. Soil Science Society of America Journal, 34(2), 252-254.

USDA soil series decription: Norfolk. https://soilseries.sc.egov.usda.gov/OSD_Docs/N/NORFOLK.html

USDA soil series decription: Rains. https://soilseries.sc.egov.usda.gov/OSD_Docs/R/RAINS.html

USDA soil series decription: Dunbar. https://soilseries.sc.egov.usda.gov/OSD_Docs/D/DUNBAR.html

USDA soil series decription: Portsmouth. https://soilseries.sc.egov.usda.gov/OSD_Docs/P/PORTSMOUTH.html
}

\examples{
data(kamp)

kamp$Al_sat <- kamp$Al/(kamp$Al + kamp$Ca_Mg) * 100
soils <- unique(kamp$soil)
col <- c('red', 'dark green', 'blue', 'gold')

plot(Al_sat ~ lr, data = kamp, type = "n")
for(i in 1:length(soils)){
  si <- kamp[kamp$soil == soils[i],]
  lines(si$lr, si$Al_sat, col = col[i])
}
legend(2.1, 85, legend = soils, col = col, 
       lty = 1, cex = .8, xjust = 1, yjust = 1, bty = "n")  
}

\keyword{datasets}
