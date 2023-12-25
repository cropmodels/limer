\name{acidification}
\alias{acidification}
\alias{acidification,data.frame-method}
\alias{acidification,matrix-method}
\alias{acidification,SpatRaster-method}

\title{
Change in sol acidity over time after liming
}

\description{
Compute the change in reacification of soil after liming. 

See \code{\link{limeRate}} for details about the lime requirement method used. 
}

\usage{
\S4method{acidification}{data.frame}(x, TAS, decay=0.25, acidification=0, years=10, method="LiTAS", ...)

\S4method{acidification}{SpatRaster}(x, TAS, decay=0.25, acidification=0, years=10, method="LiTAS", ..., filename="", overwrite=FALSE, wopt=list())
}

\arguments{
  \item{x}{SpatRaster, data.frame or matrix (with column names) with soil data. SpatRaster layer names or data.frame/matrix column names must match the names of the variables used by the lime requirement \code{method} used}
  
  \item{TAS}{numeric. Target aluminum saturation after liming (\%)}
  
  \item{decay}{numeric. The re-acidification parameter indicating the annual increase of Exch-Al after liming up to reaching the amount of Exch-Al before liming}

  \item{acidification}{numeric. The annual increase of Exch-Al from fertilizer use or other sources of soil acidification}

  \item{years}{positive integer indicating the years for which to compute soil Exch-Al}

  \item{method}{character. The method used to compute lime recommendation with \code{\link{limeRate}}. This must be a method that uses target Al saturation (TAS), that is, one of \code{c("LiTAS", "Cochrane", "NuMASS")}}

  \item{...}{additional arguments passed to \code{\{limeRate}}


  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{wopt}{list with additional arguments for writing files as in \code{\link[terra]{writeRaster}}}

}


\value{
data.frame or SpatRaster with Exch-Al. See \code{\link{limeRate}} for details.
}


\examples{
x <- data.frame(exch_ac=c(2:4,NA), ECEC=5)
limeRate(x, method="LiTAS", check_Ca=FALSE, TAS=15)
ra <- acidification(x, check_Ca=FALSE, TAS=15, years=15)
plot(ra[,c(1,3)], type="b", ylab="Exch-Al", las=1)

library(terra)
r <- rast(nrow=2, ncol=2, nl=2, vals=x, names=names(x))
rr <- acidification(r, check_Ca=FALSE, TAS=15, years=seq(1, 15, 3))
values(rr)
}