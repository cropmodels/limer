\name{limeRate}
\alias{limeRate}
\alias{limeRate,matrix-method}
\alias{limeRate,data.frame-method}
\alias{limeRate,SpatRaster-method}

\title{
Lime rate recommendation
}
\description{
Methods to compute lime rates from different inputs. The function calls function \code{\link{lr}}.
}

\usage{
limeRate(x, method, unit = "meq/100g", check_Ca=TRUE)
}


\arguments{
  \item{x}{SpatRaster, data.frame or matrix (with column names). SpatRaster layers name or data.frame/matrix column names must match the arguments required in \code{\link{lr}}.}
   \item{method}{Method (formula) used to compute lime recommendation. One of \code{c("my", "ka", "co", "nu", "bv", "gt")}. See \code{\link{lr}}.}
  
  \item{unit}{Lime rate unit of the output. If \code{unit = "t/ha"} or \code{unit = "kg/ha"} lime rate is transformed with function \code{\link{convert}} and soil bulk density (\code{SBD}) and soil depth (\code{SD}) must be specified. See \code{\link{lr}}.} 
  
  \item{check_Ca}{logical. If \code{TRUE}, lime rate is adjusted to fix Calcium deficiencies and \code{exch_Ca} is required. Lime rate is set to a minimum of 150 kg/ha when Ca saturation is lower than 25\% (Sanchez, 2019)}

}

\value{
A vector with lime rates if \code{x} is a matrix or data.frame. A SpatRaster if \code{x} is a SpatRaster
}

\author{
Robert J. Hijmans
}

\seealso{
\code{\link{lr}}
}

\examples{
TAS <- 20
exch_ac <- c(2:5,NA)
ECEC <- 10
clay <- 0.2

lr(method = "my", TAS = TAS, exch_ac = exch_ac, ECEC = ECEC, check_Ca = FALSE)

x <- data.frame(TAS=TAS, exch_ac=exch_ac, ECEC =ECEC, clay=clay)
limeRate(x, method = "my", check_Ca = FALSE)

library(terra)
r <- rast(nrow=2, ncol=3, nl=4)
values(r) <- as.matrix(rbind(x, x[1,]))
names(r) <- names(x)
rr <- limeRate(r, method = "my", check_Ca = FALSE)
values(rr)
}

