
acidification_equivalents <- function() {
	x <- matrix(c("Potassium Nitrate", "13", "0", "Calcium Nitrate", "15.5", "0", "Ammonium Nitrate", "34", "3.6", "Anhydrous Ammonia", "80-82", "3.6", "Urea", "46", "3.6", "UAN Solutions", "28-32", "3.6", "Diammonium Phosphate (DAP)", "18", "5.4", "Ammonium Sulfate", "21", "7.2", "Monoammonium Phosphate (MAP)", "10-11", "7.2"), ncol=3, byrow=TRUE)
	colnames(x) <- c("Fertilizer", "N content (%)", "lime needed (kg(ECC)/kg(N))")
	x <- data.frame(x, check.names=FALSE)
	x[, 3] <- as.numeric(x[,3])
	x
}



if (!isGeneric("acidification")) {setGeneric("acidification", function(x, ...) standardGeneric("acidification"))}

setMethod("acidification", signature(x="data.frame"), 
	function(x, TAS, decay=0.25, acidification=0, years=10, method="LiTAS", ...) {
		yrs <- if (length(years)==1) 1:years else years
		rate <- limeRate(x, method=method, TAS=TAS, ...)
		exal <- x$ECEC * TAS / 100 
		if (acidification == 0) {
			exal <- sapply(exal, \(i) i + yrs * (decay + acidification))
			exal <- t(pmin(t(exal), x$exch_ac))
		} else {
			exal <- sapply(1:length(exal), \(i) {
				if (is.na(x$exch_ac[i])) return(rep(NA, length(yrs)))
				d <- exal[i] + yrs * (decay + acidification)
				j <- d > x$exch_ac[i]
				if (any(j)) {
					d[j] <- x$exch_ac[i] + (1:sum(j))*acidification
				}
				d
			})
		}
		out <- data.frame(yrs, exal)
		colnames(out) <- c("year", apply(x, 1, \(i) paste(i, collapse="_")))
		out
	}
)



setMethod("acidification", signature(x="matrix"), 
	function(x, TAS, decay=0.25, acidification=0, years=10, method="LiTAS", ...) {
		acidification(data.frame(x), TAS, decay=decay, acidification=acidification, years=years, method=method, ...)
	}
)


setMethod("acidification", signature(x="SpatRaster"), 
	function(x, TAS, decay=0.25, acidification=0, years=10, method="LiTAS", ..., filename="", overwrite=FALSE, wopt=list()) {
		yrs <- if (length(years)==1) 1:years else years
		rate <- limeRate(x, method=method, TAS=TAS, ...)
		exal <- x$ECEC * TAS / 100 
		if (acidification == 0) {
			exal <- exal + (yrs * (decay + acidification))
			exal <- min(exal, x$exch_ac)
		} else {
			exal <- exal + (yrs * (decay + acidification))
			w <- terra::which.lyr(exal > x$exch_ac)
			exal <- min(exal, x$exch_ac)
			add  <- terra::init(terra::rast(exal), acidification)
			add  <- terra::mask(add, w, updatevalue=0)
			add  <- cumsum(add)
			exal <- terra::cover(exal, add)
		}
		names(exal) <- paste0("y_", yrs)
		if (filename != "") {
			exal <- terra::writeRaster(exal, filename, overwrite=overwrite, wopt=wopt)
		}
		exal
	}
)

