
setMethod("reacidification", signature(x="matrix"), 
	function(x, TAS, decay=0.25, acidification=0, years=10, method="LiTAS", ...) {
		reacidification(data.frame(x), TAS, decay=decay, acidification=acidification, years=years, method=method, ...)
	}
)


setMethod("reacidification", signature(x="SpatRaster"), 
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

