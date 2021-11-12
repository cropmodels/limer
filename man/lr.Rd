\name{lr}
\alias{lr}

\title{
Lime rate recommendation
}

\description{
\code{lr} is used to compute recommended lime rates based on one of several methods. 
}

\usage{
lr(method, unit = "meq/100g", check_Ca = TRUE, ...)
}
 
\arguments{
   \item{method}{Method (formula) used to compute lime recommendation. One of \code{c("my", "ka", "co", "nu", "bv", "gt")}. See Details.}
  
  \item{unit}{Lime rate unit of the output. If \code{unit = "t/ha"} or \code{unit = "kg/ha"} lime rate is transformed with function \code{\link{convert}} and soil bulk density (\code{SBD}) and soil depth (\code{SD}) must be specified.} 
  
  \item{check_Ca}{logical. If \code{TRUE}, lime rate is adjusted to fix Calcium deficiencies and \code{exch_Ca} is required. Lime rate is set to a minimum of 150 kg/ha when Ca saturation is lower than 25\% (Sanchez, 2019)}
  
  \item{...}{Additional arguments to be passed to the lower level functions.  \code{lr} tries hard to get all the required inputs for the given formula from the available data (see Details).}
}


\details{
  The available methods are:
  \itemize{
    \item{\code{"my"}: My method (still experimental).}
    \item{\code{"ka"}: Kamprath (1970).}
    \item{\code{"co"}: Cochrane et al. (1980).}
    \item{\code{"nu"}: NuMaSS.}
    \item{\code{"bv"}: Brazil V.}
    \item{\code{"gt"}: Teixeira et al. (2020).}
  }
  
  The minimum set of arguments needed for each method are shown in the following table. (Argument description and   alternatives are listed below) 
  
  \tabular{ccccccc}{ 
  method    \tab    \code{exch_ac}   \tab   \code{ECEC}   \tab \code{TAS} \tab \code{CEC_7} \tab \code{pH}   \tab   \code{OM}    \cr
  \code{"ka"} \tab X   \cr
  \code{"co"} \tab X \tab X \tab X  \cr
  \code{"nu"} \tab X \tab X \tab X  \cr
  \code{"my"} \tab X \tab X \tab X  \cr
  \code{"bv"} \tab X \tab X \tab \tab X  \cr
  \code{"gt"} \tab X \tab X \tab \tab X \tab X \tab X \cr
  
  }  
  
  Additional arguments:
  \describe{
  
    \item{\code{exch_ac}}{Exchangeable acidity (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl). If not provided, it can be estimated as \code{exch_Al + exch_H} (or equal to \code{exch_Al} if \code{exch_H} is missing).}
    
    \item{\code{ECEC}}{Effective cation exchange capacity (meq/100g of soil) as extracted with a neutral unbuffered salt. If not provided, it is estimated as the sum of all available exchangeable bases (e.g. \code{exch_Ca, exch_Mg, exch_K, exch_Na}) plus the exchangeable acidity (either \code{exch_ac} or \code{exch_Al + exch_H})} 
    
    \item{\code{SBD}}{Soil bulk density (g/cm3, kg/dm3, or kg/l). Required if \code{unit = "kg/ha"} or \code{unit = "t/ha"}, or if \code{check_Ca = TRUE}}
  
    \item{\code{SD}}{Soil depth (cm) for lime incorporation. Required if \code{unit = "kg/ha"} or \code{unit = "t/ha"}, or if \code{check_Ca = TRUE}}
  
    \item{\code{exch_Al, exch_H}}{Exchangeable Al and exchangeable H (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl)}
    
    \item{\code{exch_Ca, exch_Mg, exch_K, exch_Na, ...}}{Exchangeable bases as extracted with a neutral unbuffered salt (e.g. 1N KCl). \strong{All exchangeable cation arguments starting with \code{exch_} are considered bases except for \code{exch_Al} and \code{exch_H}}.}
    
    \item{\code{TAS}}{Target aluminum saturation after liming (\%). Required for methods \code{"my"}, \code{"co"}, \code{"nu"}}
    
    \item{\code{CEC_7}}{Cation exchange capacity buffered at pH 7 (meq/100g of soil). If not provided, it can be estimated from \code{exch_ac}, \code{ECEC}, \code{pot_ac}. Needed for methods \code{"bv"} and \code{"gt"}}
    
    \item{\code{pot_ac}}{Potencial acidity. If not provided, it can be estimated from \code{exch_ac}, \code{ECEC}, \code{CEC_7}. Needed for methods \code{"bv"} and \code{"gt"}}

    \item{\code{target_Ve}}{Target base saturation after liming (\%). Needed for method \code{"bv"}. If not provided, default values can be retrieved by \code{crop_type}}
    
    \item{\code{crop_type}}{One of \code{c("pasture", "cereal", "legume", "vegetable", "fruit")}. Optional to get default target_Ve for method \code{"bv"} when target_Ve is not provided.}
    
    \item{\code{clay}}{Clay fraction of the soil {0,1}. Optional for methods \code{"my"} and \code{"nu"}} 

    \item{\code{pH}}{Soil pH(H2O), determined in a 1:2.5 (v:v) ratio. Required for method \code{"gt"}}
    
    \item{\code{OM}}{Soil organic matter content (g/kg). Required for method \code{"gt"}}    
    
    \item{\code{X}}{Crop nutrient requirement for Calcium and Magnesium, optional for method \code{"gt"}}
    
  }
}



\value{
Recommended lime rate. Default lime rate unit is meq/100g of soil, which is the same as cmol of charge per kg of soil. 
Lime rate refers to the amount of pure fine-textured calcitic limestone (i.e., 100\% CaCO3). See function \code{\link{CCE}} for equivalencies with other liming materials and how to adjust for particle size.
}


\references{
Kamprath, E. J. (1970). Exchangeable aluminum as a criterion for liming leached mineral soils. Soil Science Society of America Journal, 34(2), 252-254.

Cochrane, T. T., Salinas, J. G., & Sanchez, P. A. (1980). An equation for liming acid mineral soils to compensate crop aluminium tolerance. Tropical Agriculture, Trinidad and Tobago, 57(2), 133-140.

Osmond, D. L., Smyth, T. J., Yost, R. S., Reid, W. S., Hoag, D. L., Branch, W., & Li, H. (2002). Nutrient Management Support System (NuMaSS) v. 2. Soil Management Collaborative Research Support Program. USA: United States Agency for International Development, Soil Management Collaborative Research Support Program. http://Pdf.Usaid.Gov/Pdf_docs/Pnada473.Pdf.

van Raij B, Cantarella H, Quaggio JA, Furlani AMC. Recomendacoes de adubacao e
calagem para o Estado de Sao Paulo. 2. ed. Campinas: IAC; 1996.

Sanchez, P. A. (2019). Properties and Management of Soils in the Tropics. Cambridge University Press.

Teixeira, W. G., Victor Hugo Alvarez, V., & Neves, J. C. L. (2020). New methods for estimating lime requirement to attain desirable pH values in Brazilian soils. Revista Brasileira de Ciencia Do Solo, 44.
}

\seealso{
\code{\link{convert}}, 
}


\examples{
TAS <- 20
exch_ac <- 2:6
ECEC <- 10
clay <- 0.2

lr(method = "my", TAS = TAS, exch_ac = exch_ac, ECEC = ECEC, check_Ca = FALSE)

}
