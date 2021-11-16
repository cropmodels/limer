\name{limeRate}
\alias{limeRate}
\alias{limeRate,matrix-method}
\alias{limeRate,data.frame-method}
\alias{limeRate,SpatRaster-method}

\title{
Lime rate recommendation
}
\description{
Compute lime rate recommendations using one of several methods.
}

\usage{
\S4method{limeRate}{SpatRaster}(x, method, unit = "meq/100g", check_Ca=TRUE, SD = 20,
	         TAS = NULL, target_Ve = NULL, X = NULL, crop_type = NULL, 
			 filename="", overwrite=FALSE, ...)

\S4method{limeRate}{data.frame}(x, method, unit = "meq/100g", check_Ca=TRUE, SD = 20,
	         TAS = NULL, target_Ve = NULL, X = NULL, crop_type = NULL)

}


\arguments{
  \item{x}{SpatRaster, data.frame or matrix (with column names) with soil data. SpatRaster layer names or data.frame/matrix column names must match the names of the variables used by each specific method. See details.}
  
   \item{method}{Method (formula) used to compute lime recommendation. One of \code{c("my", "ka", "co", "nu", "bv", "gt")}.}
  
  \item{unit}{Lime rate unit of the output. If \code{unit = "t/ha"} or \code{unit = "kg/ha"} lime rate is transformed with function \code{\link{convert}} and soil bulk density (\code{SBD}) must be specified.} 
  
  \item{check_Ca}{logical. If \code{TRUE}, lime rate is adjusted to fix Calcium deficiencies and \code{exch_Ca} and \code{SBD} are required. Lime rate is set to a minimum of 150 kg/ha when Ca saturation is lower than 25\% (Sanchez, 2019)}
  
  \item{SD}{Soil depth (cm) of lime incorporation. Default value is 20cm. It does not affect the result if \code{unit = "meq/100g"} (default value) and \code{check_Ca = FALSE}.}
  
  \item{TAS}{Target aluminum saturation after liming (\%). Required for methods \code{"my"}, \code{"co"}, \code{"nu"}}
  
  \item{target_Ve}{Target base saturation after liming (\%). Needed for method \code{"bv"}. If not provided, default values can be retrieved by \code{crop_type}}
     
  \item{X}{Crop nutrient requirement for Calcium and Magnesium. Optional for method \code{"gt"}}
    
  \item{crop_type}{One of \code{c("pasture", "cereal", "legume", "vegetable", "fruit")}. Optional to get default \code{target_Ve} values for method \code{"bv"} (when target_Ve is not provided).}
  
  \item{filename}{character. Output filename (optional)}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{...}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}

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
  
  The minimum set of variables in \code{x} (SpatRaster layers or matrix/data.frame columns) needed for each method are shown in the following table. (Variable description and alternatives are listed below) 
  
  \tabular{cccccc}{ 
  method    \tab    \code{exch_ac}   \tab   \code{ECEC} \tab \code{CEC_7} \tab \code{pH} \tab   \code{OM}    \cr
  \code{"ka"} \tab X   \cr
  \code{"co"} \tab X \tab X \cr
  \code{"nu"} \tab X \tab X \cr
  \code{"my"} \tab X \tab X \cr
  \code{"bv"} \tab X \tab X \tab X  \cr
  \code{"gt"} \tab X \tab X \tab X \tab X \tab X \cr
  
  }  
  
  Variable names and description:
  \describe{
  
    \item{\code{exch_ac}}{Exchangeable acidity (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl). If not provided, it can be estimated as \code{exch_Al + exch_H} (or equal to \code{exch_Al} if \code{exch_H} is missing).}
    
    \item{\code{ECEC}}{Effective cation exchange capacity (meq/100g of soil) as extracted with a neutral unbuffered salt. If not provided, it is estimated as the sum of all available exchangeable bases (e.g. \code{exch_Ca, exch_Mg, exch_K, exch_Na}) plus the exchangeable acidity (either \code{exch_ac} or \code{exch_Al + exch_H})} 
    
    \item{\code{SBD}}{Soil bulk density (g/cm3, kg/dm3, or kg/l). Required if \code{unit = "kg/ha"} or \code{unit = "t/ha"}, or if \code{check_Ca = TRUE}}
  
    \item{\code{exch_Al, exch_H}}{Exchangeable Al and exchangeable H (meq/100g of soil) as extracted with a neutral unbuffered salt (e.g. 1N KCl)}
    
    \item{\code{exch_Ca, exch_Mg, exch_K, exch_Na}}{Exchangeable bases as extracted with a neutral unbuffered salt (e.g. 1N KCl).}
    
    \item{\code{CEC_7}}{Cation exchange capacity buffered at pH 7 (meq/100g of soil). If not provided, it can be estimated from \code{exch_ac}, \code{ECEC}, \code{pot_ac}. Needed for methods \code{"bv"} and \code{"gt"}}
    
    \item{\code{pot_ac}}{Potencial acidity. If not provided, it can be estimated from \code{exch_ac}, \code{ECEC}, \code{CEC_7}. Needed for methods \code{"bv"} and \code{"gt"}}

    \item{\code{clay}}{Clay fraction of the soil {0,1}. Optional for methods \code{"my"} and \code{"nu"}} 

    \item{\code{pH}}{Soil pH(H2O), determined in a 1:2.5 (v:v) ratio. Required for method \code{"gt"}}
    
    \item{\code{OM}}{Soil organic matter content (g/kg). Required for method \code{"gt"}}    
    
    \item{\code{X}}{Crop nutrient requirement for Calcium and Magnesium, optional for method \code{"gt"}}
    
  }
}


\value{
Recommended lime rates. A vector if \code{x} is a matrix or data.frame, a SpatRaster if \code{x} is a SpatRaster. 

Default lime rate unit is meq/100g of soil, which is the same as cmol of charge per kg of soil. 

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
\code{\link{convert}} 
}


\examples{
x <- data.frame(exch_ac = c(2:4,NA), ECEC = 5)

limeRate(x, method = "ka", check_Ca = FALSE)
limeRate(x, method = "co", check_Ca = FALSE, TAS = 15)
limeRate(x, method = "my", check_Ca = FALSE, TAS = 15)

x$CEC_7 <- 10
limeRate(x, method = "bv", check_Ca = FALSE, target_Ve = 50)

library(terra)
r <- rast(nrow=2, ncol=2, nl=3)
values(r) <- as.matrix(x)
names(r) <- names(x)
rr <- limeRate(r, method = "ka", check_Ca = FALSE)
values(rr)
rr <- limeRate(r, method = "co", check_Ca = FALSE, TAS = 15)
values(rr)
rr <- limeRate(r, method = "my", check_Ca = FALSE, TAS = 15)
values(rr)
}

