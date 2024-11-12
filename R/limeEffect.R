
.inverse_litas <- function(lime_rate, exch_ac, ECEC, SBD, SD=20, a = 0.6, b = 0.2) {
	TAS <- seq(0, 100, 1)
	tas <- TAS/100
	lf <- 1/(a + tas * (b-a))
	x <- lf * (exch_ac - tas * ECEC)
	LR <- (x * SD * SBD)/20
	if (any(is.na(LR))) {
		return(NA)
	} else {
		stats::approx(LR, TAS, xout=lime_rate, rule=2)$y 
	}
}


if (!isGeneric("limeEffect")) {setGeneric("limeEffect", function(x, ...) standardGeneric("limeEffect"))}

setMethod("limeEffect", signature(x="SpatRaster"), 
	function(x, lime_rate, ..., filename="", overwrite=FALSE, wopt=list()) {
		.inv_lapp <- function(exch_ac, ECEC, SBD, lime_rate, SD=20, a=0.6, b=0.2) {
			TAS <- seq(0, 100, 1)
			tas <- TAS/100
			lf <- 1/(a + tas * (b-a))

			sapply(1:length(exch_ac), function(i) {
					x <- lf * (exch_ac[i] - tas * ECEC[i])
					LR <- (x * SD * SBD[i])/20
					if (any(is.na(LR))) {
						return(NA)
					} else {
						stats::approx(LR, TAS, xout=lime_rate, rule=2)$y 
					}
				} 
			)
		}		
		terra::lapp(r, fun = .inv_lapp, usenames=TRUE, lime_rate=lime_rate[1], ..., filename="", overwrite=FALSE, wopt=list())
	}
)


setMethod("limeEffect", signature(x="data.frame"), 
	function(x, lime_rate, ...) {
		if (!is.null(x$SD)) {
			sapply(1:nrow(x), function(i) .inverse_litas(lime_rate, x$exch_ac[i], x$ECEC[i], x$SBD[i], x$SD[i], ...))		
		} else {
			sapply(1:nrow(x), function(i) .inverse_litas(lime_rate, x$exch_ac[i], x$ECEC[i], x$SBD[i], ...))
		}
	}
)

setMethod("limeEffect", signature(x="matrix"), 
	function(x, lime_rate, ...)  {
		limeEffect(as.data.frame(x), lime_rate=lime_rate, ...)
	}
)

