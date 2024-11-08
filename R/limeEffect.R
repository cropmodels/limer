
# Aramburu Merlos et al. 2023
.lr_litas_convert <- function(exch_ac, ECEC, TAS, SBD, SD=20, a = 0.6, b = 0.2){
  tas <- TAS/100
  lf <- 1/(a + tas * (b-a))
  x <- lf * (exch_ac - tas * ECEC)
  (x * SD * SBD)/20
}


.inverse_litas <- function(lime_rate, exch_ac, ECEC, SBD, ...) {
	TAS <- seq(0,100,2)
	LR <- .lr_litas_convert(exch_ac=exch_ac, ECEC=ECEC, TAS=TAS, SBD=SBD, ...)
	if (any(is.na(LR))) {
		return(NA)
	} else {
		stats::approx(LR, TAS, xout=lime_rate, rule=2)$y 
	}
	#AS <- approx(LR, TAS, xout=lime_tha) 
	#AS$y * ECEC/100 
}



if (!isGeneric("limeEffect")) {setGeneric("limeEffect", function(x, ...) standardGeneric("limeEffect"))}

setMethod("limeEffect", signature(x="SpatRaster"), 
	function(x, lime_rate, ..., filename="", overwrite=FALSE, wopt=list())  {
		if (filename != "") {
			if (!overwrite) {
				if (file.exists(filename)) stop("file exists")
			}
		}
		out <- terra::rast(x, nlyr=1)
		terra::values(out) <- limeEffect(data.frame(x), lime_rate=lime_rate, ...)
		if (filename != "") {
			out <- writeRaster(out, filename, overwrite=overwrite, wopt=wopt)
		}
		out
		
#		terra::lapp(x, fun = .inverse_litas, usenames=TRUE, lime_rate=lime_rate, ..., filename=filename, overwrite=overwrite, wopt=wopt )
	}
)

setMethod("limeEffect", signature(x="data.frame"), 
	function(x, lime_rate, ...) {
		sapply(1:nrow(x), function(i) .inverse_litas(lime_rate[1], x$exch_ac[i], x$ECEC[i], x$SBD[i], ...))
	}
)

setMethod("limeEffect", signature(x="matrix"), 
	function(x)  {
		limeEffect(as.data.frame(x))
	}
)
