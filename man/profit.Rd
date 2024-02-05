\name{profit}
\alias{profit}
\alias{profit,numeric,numeric-method}
\alias{profit,SpatRaster,SpatRaster-method}

\title{
Profit of liming
}


\description{
Profit of liming considering the number of years over which it has an effect. The cost is in year 1, the benefit is over all years, declining linearly such that the benefit in \code{nyears + 1} would be zero
}

\usage{
\S4method{profit}{numeric, numeric}(cost, benefit, nyears, interest_rate)

\S4method{profit}{SpatRaster, SpatRaster}(cost, benefit, nyears, interest_rate, filename="", overwrite=FALSE, ...)
}

\arguments{
  \item{cost}{numeric or SpatRaster. The cost of liming to bring the soil to a target. The cost can be in any currency and unit}
  \item{benefit}{numeric or SpatRaster. The benefit in terms of the value of crop production when the soil is brought to the target} 
  
  \item{nyears}{same class as \code{cost} and \code{benefit}. The number of years over which liming has an effect}

  \item{interst_rate}{numeric. The interest rate (\%)}

  \item{maintain}{logical. Apply a yearly maintenance lime rate?}

  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{...}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}
}


\value{
numeric or SpatRaster
}


\examples{
profit(cost=c(50,100), benefit=50, nyears=5, interest=10)

library(terra)
x <- data.frame(cost=rep(c(50,100), 8), benefit=75, nyears=rep(c(3,5), each=8))
r <- rast(nrow=4, ncol=4, nl=3, vals=x, names=names(x))

z <- profit(r$cost, r$benefit, r$nyears, interest=10)
}

