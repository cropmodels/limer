\name{bv_lr}
\alias{bv_lr}

\title{
Brazil V method to estimate lime requirements
}

\description{
Empirical formula to estimate the lime rate required to alleviate or neutralize soil acidity problems.
}

\usage{
bv_lr(exch_Ca, exch_Mg, exch_K, exch_Na, CEC_7, target_Ve, crop_type)
}

\arguments{
  \item{exch_Ca}{Exchangeable Calcium (meq/100g of soil)}
  \item{exch_Mg}{Exchangeable Magnesium (meq/100g of soil)}
  \item{exch_K}{Exchangeable Potassium (meq/100g of soil)}
  \item{exch_Na}{Exchangeable Sodium (meq/100g of soil)}
  \item{CEC_7}{Cation exchange capacity buffered at pH 7 (meq/100g of soil)}
  \item{target_Ve}{Target base saturation after liming (\%) (optional)}
  \item{crop_type}{One of \code{c("pasture", "cereal", "legume", "vegetable", "fruit")} (optional)}
}

\details{
Either \code{target_Ve} or \code{crop_type} must be specified. If \code{target_Ve} is specified, \code{crop_type} is deprecated.

Exchangeable cations (Ca, Mg, K and Na) as extracted with a neutral unbuffered salt (e.g. 1N KCl). Note that meq/100g of soil is the same as cmol of charge per kilogram of soil. 
}

\value{
Recommended lime rate in milliequivalents of Calcium per 100 g of soil. 
}

\references{
van Raij B, Cantarella H, Quaggio JA, Furlani AMC. Recomendações de adubação e
calagem para o Estado de São Paulo. 2. ed. Campinas: IAC; 1996.

Sanchez, P. A. (2019). Properties and Management of Soils in the Tropics. Cambridge University Press.
}

\author{
Fernando Aramburu Merlos
}


\seealso{
\code{\link{lr}, \link{ka_lr}, \link{co_lr}, \link{nu_lr}, \link{my_lr}, \link{gt_lr}}, 
}

\examples{
exch_Ca <- 2
exch_Mg <- 1
exch_K <- 0.5
exch_Na <- 0.5
CEC_7 <- 12

target_Ve <- 60
bv_lr(exch_Ca, exch_Mg, exch_K, exch_Na, CEC_7, target_Ve)

target_Ve <- 80
bv_lr(exch_Ca, exch_Mg, exch_K, exch_Na, CEC_7, target_Ve)

bv_lr(exch_Ca, exch_Mg, exch_K, exch_Na, CEC_7, crop_type = "cereal")
bv_lr(exch_Ca, exch_Mg, exch_K, exch_Na, CEC_7, crop_type = "fruit")

}
