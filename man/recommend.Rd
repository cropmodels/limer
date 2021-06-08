\name{recommendations}

\alias{nc_lime}
\alias{nc_acidity}

\title{
Effect of lime on pH
}

\description{
Empirical formulas to compute the effect of lime on pH 
}

\usage{
nc_lime(lime, CCE, pH, Ac)
nc_acidity(BpH)
}
 
\arguments{
  \item{lime}{lime amount in kg/ha}
  \item{CCE}{Calcium carbonate equivalent in \%}
  \item{pH}{Soil pH}
  \item{Ac}{Acidity, the sum of exchangeable aluminum and hydrogen cations in meq/100 cm³, estimated with \code{nc_acidity}}
  \item{BpH}{Buffer (which?) pH reading}
}


\value{
pH change or acidity in meq/100^3
}


\examples{
nc_lime(2471*.75, 90, 5, 1.2)
}
