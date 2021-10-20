\name{lr}
\alias{lr}

\title{
Lime rate recommendation
}

\description{
\code{lr} is used to compute recommended lime rates based on one of several methods. 
}

\usage{
lr(method, units = "meq/100g", ...)
}
 
\arguments{
  \item{method}{Method (formula) used to compute lime recommendation. One of \code{c("my", "ka", "co", "nu", "bv", "gt")}. See Details.}
  \item{units}{Lime rate units of the output. If \code{units = "t/ha"} or \code{units = "kg/ha"} lime rate is transformed with function \code{\link{convert}} and soil bulk density (\code{sbd}) and soil depth (\code{sd}) must be specified.} 
  \item{...}{Additional arguments to be passed to the lower level functions.  \code{limer} tries hard to get all the required inputs for the given formula from the available data (see Details).}
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
    \item{\code{exch_ac}}{Exchangeable acidity (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl). If not provided it is estimated as \code{exch_Al + exch_H} (or equal to \code{exch_Al} if \code{exch_H} is missing). If \code{exch_Al} is also missing, it is estimated as \code{ECEC * Al_sat/100}}
    
    \item{\code{ECEC}}{Effective cation exchange capacity (meq/100g of soil). If not provided it is estimated as the sum of all available exchangeable bases (e.g. \code{exch_Ca, exch_Mg, exch_K, exch_Na}) plus the exchangeable acidity (either \code{exch_ac} or \code{exch_Al + exch_H})}
    
    \item{\code{exch_Al, exch_H}}{Exchangeable Al and exchangeable H (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl)}
    
    \item{\code{exch_Ca, exch_Mg, exch_K, exch_Na, ...}}{Exchangeable bases as extracted with a neutral unbuffered salt (e.g. 1N KCl). \strong{All exchangeable cation arguments starting with \code{exch_} are considered bases except for \code{exch_Al} and \code{exch_H}}.}
    
    \item{\code{TAS}}{Target aluminum saturation after liming (\%)}
    
    \item{\code{CEC_7}}{Cation exchange capacity buffered at pH 7 (meq/100g of soil). If not provided it can be estimated from \code{exch_ac}, \code{ECEC}, \code{pot_ac}.}
    
    \item{\code{pH}}{Soil pH(H2O), determined in a 1:2.5 (v:v) ratio}
    
    \item{\code{OM}}{Soil organic matter content (g/kg)}
    
    \item{\code{pot_ac}}{Potencial acidity. If not provided it can be estimated from \code{exch_ac}, \code{ECEC}, \code{CEC_7}.}

    \item{\code{clay}}{Clay fraction of the soil {0,1}. Optional for NuMaSS and my methods. See \code{\link{lr_nu}, \link{lr_my}}}

    \item{\code{a}}{Lime factor for my method. Default value is 0.8. See \code{\link{lr_my}}}
    
    \item{\code{b}}{Lime factor for my method. Default value is 0.2. See \code{\link{lr_my}}}
    
    \item{\code{X}}{Crop nutrient requirement for Calcium and Magnesium, optional for Teixeira method (\code{\link{lr_gt}})}
    
    \item{target_Ve}{Target base saturation after liming (\%). Optional for Brazil V method. See \code{\link{lr_bv}}}
    
    \item{crop_type}{One of \code{c("pasture", "cereal", "legume", "vegetable", "fruit")}. Needed for Brazil V method (if \code{target_Ve} is missing). Defualt value is \code{"cereal"}}

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

}
