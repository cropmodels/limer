\name{maintenanceRate}
\alias{maintenanceRate}
\alias{maintenanceRate,data.frame-method}
\alias{maintenanceRate,matrix-method}
\alias{maintenanceRate,SpatRaster-method}

\title{
Initial and maintenance lime rates
}

\description{
Compute the initial and the annual maintenance lime rate to keep the soil at the target Al saturation (TAS). 

See \code{\link{limeRate}} for details about the lime requirement method used. 
}

\usage{
\S4method{maintenanceRate}{data.frame}(x, TAS, decay=0.25, acidification=0, method="LiTAS", ...)

\S4method{maintenanceRate}{SpatRaster}(x, TAS, decay=0.25, acidification=0, method="LiTAS", ..., filename="", overwrite=FALSE, wopt=list())
}

\arguments{
  \item{x}{SpatRaster, data.frame or matrix (with column names) with soil data. SpatRaster layer names or data.frame/matrix column names must match the names of the variables used by the lime requirement \code{method} used}

  \item{TAS}{numeric. Target aluminum saturation after liming (\%)}

  \item{decay}{numeric. The re-acidification parameter indicating the annual increase of Exch-Al after liming up to reaching the amount of Exch-Al before liming}

  \item{acidification}{numeric. The annual increase of Exch-Al from fertilizer use or other sources of soil acidification}

  \item{method}{character. The method used to compute lime recommendation with \code{\link{limeRate}}. This must be a method that uses target Al saturation (TAS), that is, one of \code{c("LiTAS", "Cochrane", "NuMASS")}}

  \item{...}{additional arguments passed to \code{\{limeRate}}

  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{wopt}{list with additional arguments for writing files as in \code{\link[terra]{writeRaster}}}
}


\value{
matrix or SpatRaster with the initial and maintenance lime rates. See \code{\link{limeRate}} for details.
}


\examples{
x <- data.frame(exch_ac=c(2:4,NA), ECEC=5)
limeRate(x, method="LiTAS", check_Ca=FALSE, TAS=15)
maintenanceRate(x, check_Ca=FALSE, TAS=15)

library(terra)
r <- rast(nrow=2, ncol=2, nl=2, vals=x, names=names(x))
rr <- maintenanceRate(r, check_Ca=FALSE, TAS=15)
values(rr)
}

