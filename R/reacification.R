
if (!isGeneric("reacidification")) {setGeneric("reacidification", function(x, ...) standardGeneric("reacidification"))}

setMethod("reacidification", signature(x="data.frame"), 
	function(x, TAS, decay=0.25, acidification=0, years=10, method="LiTAS", ...) {
		yrs <- if (length(years)==1) 1:years else years
		rate <- limeRate(x, method=method, TAS=TAS, ...)
		exal <- x$ECEC * TAS / 100 
		if (acidification == 0) {
			exal <- sapply(exal, \(i) i + yrs * (decay + acidification))
			exal <- t(pmin(t(exal), x$exch_ac))
		} else {
			# can go over initial exch_ac 
			# the transition year is tricky
			exal <- sapply(1:nrow(x), \(i) {
				a <- exal[i] + yrs * (decay + acidification)
				b <- a > x$exch_ac[i]
				sb <- sum(b)
				if (sb > 0) {
					w <- which(b)
					a[w[0]] <- min(a[w[0]-1], x$exch_ac[i]) + acidification
					if (sb > 1) {
						w <- w[-1]
						a[w] <- a[w-1] + cumsum(rep(acidification, sb-1))
					}
				}
			})
		}
		out <- data.frame(yrs, exal)
		colnames(out) <- c("year", apply(x, 1, \(i) paste(i, collapse="_")))
		out
	}
)



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

