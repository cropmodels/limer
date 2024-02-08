\name{NPV}
\alias{NPV_lime}
\alias{NPV_lime,numeric,numeric-method}
\alias{NPV_lime,SpatRaster,SpatRaster-method}

\title{
Net present value of liming
}


\description{
Net present value (NPV) of liming considering the number of years over which it has a (linearly decreasing) effect and a discount rate. 
}

\usage{
\S4method{NPV_lime}{numeric, numeric}(x, nyears, discount_rate)

\S4method{NPV_lime}{SpatRaster, SpatRaster}(x, nyears, discount_rate, filename="", overwrite=FALSE, ...)
}

\arguments{
  \item{x}{numeric or SpatRaster. The benefit in terms of the value of crop production when the soil is brought to the target (in year 1)} 
  
  \item{nyears}{same class as \code{cost} and \code{benefit}. The number of years over which liming has an effect (linearly decreasing and zero at (nyears+1)}

  \item{discount_rate}{numeric. The annual discount rate (\%) used to reduce future income}

  \item{maintain}{logical. Apply a yearly maintenance lime rate?}

  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{...}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}
}


\value{
numeric or SpatRaster
}


\examples{
NPV(cost=c(50,100), benefit=50, nyears=5, interest=10)

library(terra)
x <- data.frame(cost=rep(c(50,100), 8), benefit=75, nyears=rep(c(3,5), each=8))
r <- rast(nrow=4, ncol=4, nl=3, vals=x, names=names(x))

z <- NPV(r$cost, r$benefit, r$nyears, interest=10)
}

