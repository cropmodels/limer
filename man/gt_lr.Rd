\name{gt_lr}
\alias{gt_lr}

\title{
Teixeira et al. (2020) lime requirement algorithm
}

\description{
Empirical algorithm to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
gt_lr(pH, OM, pot_ac, X, exch_Ca, exch_Mg)
}

\arguments{
  \item{pH}{Soil pH(H2O), determined in a 1:2.5 (v:v) ratio}
  \item{OM}{Soil organic matter content (g/kg)}
  \item{pot_ac}{Potencial acidity (see Details)}
  \item{X}{Crop nutrient requirement for Calcium and Magnesium (optional)}
  \item{exch_Ca}{Exchangeable Calcium (meq/100g of soil) (optional)}
  \item{exch_Mg}{Exchangeable Magnesium (meq/100g of soil) (optional)}
}

\details{
Potencial acidity equals the cation exchange capacity at pH 7 minus the sum of exchangeable bases (\code{exch_Ca + exch_Mg + exch_K + exch_Na}). 

If \code{X} is provided, the lime recommendation is adjuisted to meet crop Ca and Mg requirements and \code{exch_Ca} and \code{exch_Mg} are required. Current implementation pools Ca and Mg requirements and assumes that lime can provide either cation.   

Exchangeable cations (Ca, Mg, K and Na) as extracted with a neutral unbuffered salt (e.g. 1N KCl). Note that meq/100g of soil is the same as cmol of charge per kilogram of soil. 
}

\value{
Recommended lime rate in (t/ha? or milliequivalents of Calcium per 100 g of soil)
}

\references{
Teixeira, W. G., Victor Hugo Alvarez, V., & Neves, J. C. L. (2020). New methods for estimating lime requirement to attain desirable pH values in Brazilian soils. Revista Brasileira de Ciencia Do Solo, 44.
}

\author{
Fernando Aramburu Merlos
}


\seealso{
\code{\link{lr}, \link{ka_lr}, \link{co_lr}, \link{nu_lr}, \link{my_lr}, \link{bv_lr}}, 
}

\examples{
pH <- seq(4, 5.5, .5)
OM <- 40
pot_ac <- 6
  
gt_lr(pH, OM, pot_ac)

pot_ac <- 12
gt_lr(pH, OM, pot_ac)

pot_ac <- 6
OM <- 80
gt_lr(pH, OM, pot_ac)

pot_ac <- 12
OM <- 80
gt_lr(pH, OM, pot_ac)


# X effect
OM <- 40
pot_ac <- 6
exch_Ca <- 0.5
exch_Mg <- 0.2
X <- 2

gt_lr(pH, OM, pot_ac)
gt_lr(pH, OM, pot_ac, X, exch_Ca, exch_Mg)

exch_Ca <- 0.05
exch_Mg <- 0.05
gt_lr(pH, OM, pot_ac, X, exch_Ca, exch_Mg)
}
