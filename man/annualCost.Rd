\name{annualCost}

\alias{annualCost}
\alias{annualCost,numeric,numeric-method}
\alias{annualCost,SpatRaster,ANY-method}

\title{
Annualized cost of liming
}


\description{
Annual cost of liming for the initial number of years in which you pay off the initial amount of lime needed. After that period you only have the maintenance cost.
}

\usage{
\S4method{annualCost}{numeric, numeric}(initial, maintenance, interest_rate, nyears)

\S4method{annualCost}{SpatRaster, ANY}(initial, maintenance, interest_rate, nyears, filename="", overwrite=FALSE, ...)
}

\arguments{
  \item{initial}{numeric or SpatRaster. The initial cost of liming to bring the soil to a target aluminum saturation. The cost can be in any currency and unit}
  \item{maintenance}{numeric or SpatRaster. The annual maintenance cost of liming to keep the soil to a target aluminum saturation} 
  
  \item{interst_raste}{numeric. The interest rate (\%)}

  \item{nyears}{positive integer. The number of years over which to pay off the initial lime rate}

  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{...}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}
}


\value{
numeric or SpatRaster
}


\examples{
annualCost(initial=c(50,100), maintenance=20, interest=10, nyears=5)

library(terra)
x <- data.frame(exch_ac=c(2:4,NA), ECEC=5)
r <- rast(nrow=2, ncol=2, nl=2, vals=x, names=names(x))
rr <- maintenanceRate(r, check_Ca=FALSE, TAS=15)

z <- annualCost(initial=rr[[1]], maintenance=rr[[2]], interest=10, nyears=5)

}

