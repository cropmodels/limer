\name{reacidification}
\alias{reacidification}
\alias{reacidification,data.frame-method}
\alias{reacidification,matrix-method}
\alias{reacidification,SpatRaster-method}

\title{
Change in sol acidity over time after liming
}

\description{
Compute the change in reacification of soil after liming. 

See \code{\link{limeRate}} for details about the lime requirement method used. 
}

\usage{
\S4method{reacidification}{data.frame}(x, method="LiTAS", decay=0.25, acidification=0, years=10, ...)

\S4method{reacidification}{SpatRaster}(x, method="LiTAS", decay=0.25, acidification=0, years=10, ..., filename="", overwrite=FALSE, wopt=list())
}

\arguments{
  \item{x}{SpatRaster, data.frame or matrix (with column names) with soil data. SpatRaster layer names or data.frame/matrix column names must match the names of the variables used by the lime requirement \code{method} used}
  \item{method}{character. The method used to compute lime recommendation with \code{\link{limeRate}}. This must be a method that uses target Al saturation (TAS), that is, one of \code{c("LiTAS", "Cochrane", "NuMASS")}}
  \item{decay}{numeric. The re-acidification parameter indicating the annual increase of Exch-Al after liming up to reaching the amount of Exch-Al before liming}
  \item{acidification}{numeric. The annual increase of Exch-Al from fertilizer use or other sources of soil acidification}
  \item{years}{positive integer indicating the years for which to compute soil Exch-Al}
  \item{...}{additional arguments passed to \code{\{limeRate}}

  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{wopt}{list with additional arguments for writing files as in \code{\link[terra]{writeRaster}}}

}


\value{
data.frame or SpatRaster with Exch-Al. See \code{\link{limeRate}} for details.
}


