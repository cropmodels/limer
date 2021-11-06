\name{lr}
\alias{lr}

\title{
Lime rate recommendation
}

\description{
\code{lr} is used to compute recommended lime rates based on one of several methods. 
}

\usage{
lr(exch_ac = NULL, ECEC = NULL, method, unit = "meq/100g", 
   SBD = NULL, SD = NULL, check_Ca = TRUE, ...)
}
 
\arguments{
  \item{exch_ac}{Exchangeable acidity (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl). If not provided, it can be estimated as \code{exch_Al + exch_H} (or equal to \code{exch_Al} if \code{exch_H} is missing).}
  
  \item{ECEC}{Effective cation exchange capacity (meq/100g of soil) as extracted with a neutral unbuffered salt. If not provided, it is estimated as the sum of all available exchangeable bases (e.g. \code{exch_Ca, exch_Mg, exch_K, exch_Na}) plus the exchangeable acidity (either \code{exch_ac} or \code{exch_Al + exch_H})} 
  
  \item{method}{Method (formula) used to compute lime recommendation. One of \code{c("my", "ka", "co", "nu", "bv", "gt")}. See Details.}
  
  \item{unit}{Lime rate unit of the output. If \code{unit = "t/ha"} or \code{unit = "kg/ha"} lime rate is transformed with function \code{\link{convert}} and soil bulk density (\code{SBD}) and soil depth (\code{SD}) must be specified.} 
  
  \item{SBD}{Soil bulk density (g/cm3, kg/dm3, or kg/l)}
  
  \item{SD}{Soil depth (cm) for lime incorporation.}
  
  \item{check_Ca}{logical. If \code{TRUE}, lime rate is adjusted to fix Calcium deficiencies and \code{exch_Ca} is required. Lime rate is set to a minimum of 150 kg/ha when Ca saturation is lower than 25\%}
  
  \item{...}{Additional arguments to be passed to the lower level functions.  \code{lr} tries hard to get all the required inputs for the given formula from the available data (see Details).}
}


\details{
  The available methods are:
  \itemize{
    \item{\code{my}}: My method (still experimental). See \code{\link{lr_my}}
    \item{\code{ka}}: Kamprath (1970). See \code{\link{lr_ka}}
    \item{\code{co}}: Cochrane et al. (1980). See \code{\link{lr_co}}
    \item{\code{nu}}: NuMaSS. See \code{\link{lr_nu}}
    \item{\code{bv}}: Brazil V. See \code{\link{lr_bv}}
    \item{\code{gt}}: Teixeira et al. (2020). See \code{\link{lr_gt}}
  }
  
  The minimum set of arguments needed for each method are shown in the following table. (Argument description and   alternatives are listed below) 
  
  \tabular{ccccccc}{ 
  method    \tab    \code{exch_ac}   \tab   \code{ECEC}   \tab \code{TAS} \tab \code{CEC_7} \tab \code{pH}   \tab   \code{OM}    \cr
  \code{lr_ka} \tab X   \cr
  \code{lr_co} \tab X \tab X \tab X  \cr
  \code{lr_nu} \tab X \tab X \tab X  \cr
  \code{lr_my} \tab X \tab X \tab X  \cr
  \code{lr_bv} \tab X \tab X \tab \tab X  \cr
  \code{lr_gt} \tab X \tab X \tab \tab X \tab X \tab X \cr
  
  }  
  
  Additional arguments:
  \describe{
    \item{\code{exch_Al, exch_H}}{Exchangeable Al and exchangeable H (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl)}
    
    \item{\code{exch_Ca, exch_Mg, exch_K, exch_Na, ...}}{Exchangeable bases as extracted with a neutral unbuffered salt (e.g. 1N KCl). \strong{All exchangeable cation arguments starting with \code{exch_} are considered bases except for \code{exch_Al} and \code{exch_H}}.}
    
    \item{\code{TAS}}{Target aluminum saturation after liming (\%). Required for methods \code{\link{lr_my}}, \code{\link{lr_co}}, \code{\link{lr_nu}}}
    
    \item{\code{CEC_7}}{Cation exchange capacity buffered at pH 7 (meq/100g of soil). If not provided, it can be estimated from \code{exch_ac}, \code{ECEC}, \code{pot_ac}. Needed for methods \code{\link{lr_bv}} and \code{\link{lr_gt}}}
    
    \item{\code{pot_ac}}{Potencial acidity. If not provided, it can be estimated from \code{exch_ac}, \code{ECEC}, \code{CEC_7}. Needed for methods \code{\link{lr_bv}} and \code{\link{lr_gt}}}

    \item{\code{pH}}{Soil pH(H2O), determined in a 1:2.5 (v:v) ratio. Required for method \code{\link{lr_gt}}}
    
    \item{\code{OM}}{Soil organic matter content (g/kg). Required for method \code{\link{lr_gt}}}
    
    \item{\code{clay}}{Clay fraction of the soil {0,1}. Optional for methods \code{\link{lr_my}} and \code{\link{lr_nu}}} 

    \item{\code{a}}{Lime factor for my method. Default value is 0.8. See \code{\link{lr_my}}}
    
    \item{\code{b}}{Lime factor for my method. Default value is 0.2. See \code{\link{lr_my}}}
    
    \item{\code{X}}{Crop nutrient requirement for Calcium and Magnesium, optional for method \code{\link{lr_gt}}}
    
    \item{\code{target_Ve}}{Target base saturation after liming (\%). Optional ethod \code{\link{lr_bv}}}
    
    \item{\code{crop_type}}{One of \code{c("pasture", "cereal", "legume", "vegetable", "fruit")}. Optional for method \code{\link{lr_bv}}}

  }
}



\value{
Recommended lime rate.
}


\references{
(references available in lower level functions)
}

\seealso{
\code{\link{lr_ka}, \link{lr_co}, \link{lr_nu}, \link{lr_my}, \link{lr_bv}, \link{lr_gt}, \link{convert}}, 
}


\examples{
TAS <- 20
exch_ac <- 2:6
ECEC <- 10
clay <- 0.2

lr(method = "my", TAS = TAS, exch_ac = exch_ac, ECEC = ECEC, check_Ca = FALSE)

}
