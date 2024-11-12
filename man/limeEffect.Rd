\name{limeEffect}
\alias{limeEffect}
\alias{limeEffect,matrix-method}
\alias{limeEffect,data.frame-method}
\alias{limeEffect,SpatRaster-method}

\title{
Effect of lime on Al saturation
}

\description{
Compute the effect of lime on Al saturation. This is computed by inverting the LiTAS model.
}

\usage{

\S4method{limeEffect}{SpatRaster}(x, lime_rate, ..., filename="", overwrite=FALSE, wopt = list())

\S4method{limeEffect}{data.frame}(x, lime_rate, ...)
}

\arguments{
  \item{x}{SpatRaster, data.frame or matrix (with column names) with soil data. SpatRaster layer names or data.frame/matrix column names must match the names of the variables used by each specific method. See details.}
  \item{lime_rate}{lime applied in t/ha}
  \item{...}{additional arguments for the LiTAS model}
  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{wopt}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}
}


\details{
  Variable names and description:
  \describe{
  
    \item{\code{lime_rate}}{in ton/ha}

    \item{\code{exch_ac}}{Exchangeable acidity (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl). If not provided, it can be estimated as \code{exch_Al + exch_H} (or equal to \code{exch_Al} if \code{exch_H} is missing).}
    
    \item{\code{ECEC}}{Effective cation exchange capacity (meq/100g of soil). If not provided, it is estimated as the sum of exchangeable bases plus the exchangeable acidity.} 

    \item{\code{SBD}}{Soil bulk density (g/cm3, kg/dm3, or kg/l)}

    \item{\code{SD}}{Soil depth (cm). If \code{x} is a SpatRaster, this should not be a layer, but a constant that is passed seperately as an additional argument}

}
}


\references{
Aramburu Merlos, F., Silva, J.V., Baudron, F., Hijmans, R.J. (2023). Estimating lime requirements for tropical soils: Model comparison and development. Geoderma 432: 116421
}


\value{
Numeric or SpatRaster with exchangeable acidity after liming. 
}


\seealso{
\code{\link{limeRate}} 
}


\examples{
x <- data.frame(exch_ac = c(2:4,NA), ECEC = 5, SBD=1.1)
limeEffect(x, lime_rate=1)

library(terra)
r <- rast(nrow=2, ncol=2, nl=3, vals=x, names=names(x))
rr <- limeEffect(r, lime_rate=1)
values(rr)
}

