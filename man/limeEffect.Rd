\name{limeEffect}
\alias{limeEffect}
\alias{limeEffect,matrix-method}
\alias{limeEffect,data.frame-method}
\alias{limeEffect,SpatRaster-method}

\title{
Effect of lime on Al saturation
}

\description{
Compute the effect of lime on Al saturation
}

\usage{
\S4method{limeEffect}{SpatRaster}(x, filename="", overwrite=FALSE, ...)

\S4method{limeEffect}{data.frame}(x)
}

\details{

  Variable names and description:
  \describe{
  
    \item{\code{lime}}{Exchangeable bases, sum of exchangeable Ca, Mg, K, and Na (meq/100g of soil).}

    \item{\code{exch_ac}}{Exchangeable acidity (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl). If not provided, it can be estimated as \code{exch_Al + exch_H} (or equal to \code{exch_Al} if \code{exch_H} is missing).}
    
    \item{\code{ECEC}}{Effective cation exchange capacity (meq/100g of soil). If not provided, it is estimated as the sum of exchangeable bases plus the exchangeable acidity.} 
}
}


\arguments{
  \item{x}{SpatRaster, data.frame or matrix (with column names) with soil data. SpatRaster layer names or data.frame/matrix column names must match the names of the variables used by each specific method. See details.}
  
  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{...}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}

}



\value{
Recommended lime rates. A vector if \code{x} is a matrix or data.frame, a SpatRaster if \code{x} is a SpatRaster. 

Default lime rate unit is meq/100g of soil, which is the same as cmol of charge per kg of soil. 

Lime rate refers to the amount of pure fine-textured calcitic limestone (i.e., 100\% CaCO3). See function \code{\link{CCE}} for equivalencies with other liming materials and how to adjust for particle size.
}


\references{
Aramburu Merlos, F., Silva, J.V., Baudron, F., Hijmans, R.J. (2023). Estimating lime requirements for tropical soils: Model comparison and development. Geoderma 432: 116421
}

\seealso{
\code{\link{convert}} 
}


\examples{
x <- data.frame(exch_ac = c(2:4,NA), ECEC = 5)

limeEffect(x, method = "Kamprath", check_Ca = FALSE)
limeEffect(x, method = "Cochrane", check_Ca = FALSE, TAS = 15)
limeEffect(x, method = "LiTAS", check_Ca = FALSE, TAS = 15)

x$CEC_7 <- 10
limeEffect(x, method = "BrazilV", check_Ca = FALSE, target_Ve = 50)

library(terra)
r <- rast(nrow=2, ncol=2, nl=3, vals=x, names=names(x))
rr <- limeEffect(r, method = "Kamprath", check_Ca = FALSE)
values(rr)
rr <- limeEffect(r, method = "Cochrane", check_Ca = FALSE, TAS = 15)
values(rr)
rr <- limeEffect(r, method = "LiTAS", check_Ca = FALSE, TAS = 15)
values(rr)
}

