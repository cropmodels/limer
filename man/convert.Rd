\name{convert}
\alias{convert}

\title{
Convert lime rate units
}
\description{
Unit convertion of lime rates from meq of Ca per 100g of soils to metric tons per hectare or vice versa. Note that meq/100g is the same as cmol_c/kg.
}

\usage{
convert(x, sbd, sd, to_t_ha = TRUE)
}

\arguments{
  \item{x}{lime rate, either in meq/100g of soil (if \code{to_t_ha = TRUE}) or in t/ha (if to_t_ha = FALSE)}
  \item{sbd}{soil bulk density (g/cm3, kg/dm3, or kg/l)}
  \item{sd}{soil depth (cm)}
  \item{to_t_ha}{if \code{TRUE}, lime rate is transformed from meq/100g of soil to tons per hectare, if \code{FALSE}, from t/ha to meq/100g of soil.}
}

\details{
It is assumed that Ca+2 provides 2 mmol of positive charges per mmol. 
}

\value{
Converted lime rate, either in t/ha (if \code{to_t_ha = TRUE}) or in meq/100g (if to_t_ha = FALSE)
}

\author{
Fernando Aramburu Merlos
}

\examples{
lime_rate <- 2
sbd <- 1.4
sd <- 15

convert(lime_rate, sbd, sd, to_t_ha = TRUE)


}

