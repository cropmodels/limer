
if (!isGeneric("maintenanceRate")) {setGeneric("maintenanceRate", function(x, ...) standardGeneric("maintenanceRate"))}


setMethod("maintenanceRate", signature(x="data.frame"), 
	function(x, TAS, decay=0.25, acidification=0, method="LiTAS", ...) {
		initial <- limeRate(x, method=method, TAS=TAS, ...)
		soils <- apply(x, 1, \(i) paste(i, collapse="_"))
		exal <- x$ECEC * TAS / 100 
#		if (adjECEC) {
#			# ecec increases with (.92 - .6) * limerate
#			x$ECEC <- x$ECEC + initial * 0.32
#			exal <- x$ECEC * TAS / 100 
#		}
		x$exch_ac <- pmin(x$exch_ac, exal + decay + acidification)
		maintenance <- limeRate(x, method=method, TAS=TAS, ...)
		out <- rbind(initial, maintenance)
		colnames(out) <- soils
		out
	}
)


setMethod("maintenanceRate", signature(x="matrix"), 
	function(x, TAS, decay=0.25, acidification=0, method="LiTAS", ...) {
		maintenanceRate(data.frame(x), method=method, decay=decay, acidification=acidification, ...)
	}
)

setMethod("maintenanceRate", signature(x="SpatRaster"), 
	function(x, TAS, decay=0.25, acidification=0, method="LiTAS", ..., filename="", overwrite=FALSE, wopt=list()) {
		rate1 <- limeRate(x, method=method, TAS=TAS, ...)
		exal <- x$ECEC * TAS / 100 
		x$exch_ac <- min(x$exch_ac, exal + decay + acidification)
		rate2 <- limeRate(x, method=method, TAS=TAS, ...)
		out <- c(rate1, rate2)
		names(out) <- c("initial", "maintenance")
		if (filename != "") {
			out <- terra::writeRaster(out, filename, overwrite=overwrite, wopt=wopt)
		}
		out
	}
)


