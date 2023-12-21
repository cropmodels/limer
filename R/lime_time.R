
if (!isGeneric("maintenanceRate")) {setGeneric("maintenanceRate", function(x, ...) standardGeneric("maintenanceRate"))}


setMethod("maintenanceRate", signature(x="data.frame"), 
	function(x, method="LiTAS", decay=0.25, acidification=0, ...) {
		rate1 <- limeRate(x, method=method, ...)
		exal <- x$ECEC * x$TAS / 100 
		x$exch_ac <- pmin(x$exch_ac, exal + decay + acidification)
		rate2 <- limeRate(x, method=method, ...)
		out <- cbind(c(0, 1), rbind(rate1, rate2))
		colnames(out) <- c("year", apply(x, 1, \(i) paste(i, collapse="_")))
		out
	} 
)

setMethod("maintenanceRate", signature(x="matrix"), 
	function(x, method="LiTAS", decay=0.25, acidification=0, ...) {
		maintenanceRate(data.frame(x), method=method, decay=decay, acidification=acidification, ...)
	}
)

setMethod("maintenanceRate", signature(x="SpatRaster"), 
	function(x, method="LiTAS", decay=0.25, acidification=0, ..., filename="", overwrite=FALSE, wopt=list()) {
		rate1 <- limeRate(x, method=method, ..., wopt=list())
		exal <- x$ECEC * x$TAS / 100 
		x$exch_ac <- min(x$exch_ac, exal + decay + acidification)
		rate2 <- limeRate(x, method=method, ...)
		out <- c(rate1, rate2)
		names(out) <- c("initial", "maintenance")
		if (filename != "") {
			out <- terra::writeRaster(out, filename, overwrite=overwrite, wopt=wopt)
		}
		out
	}
)



if (!isGeneric("reacidification")) {setGeneric("reacidification", function(x, ...) standardGeneric("reacidification"))}

setMethod("reacidification", signature(x="data.frame"), 
	function(x, method="LiTAS", decay=0.25, acidification=0, years=10, ...) {
		yrs <- if (length(years)==1) 1:years else years
		rate <- limeRate(x, method=method, ...)
		exal <- x$ECEC * x$TAS / 100 
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
	function(x, method="LiTAS", decay=0.25, acidification=0, years=10, ...) {
		reacidification(data.frame(x), method=method, decay=decay, acidification=acidification, years=years, ...)
	}
)


setMethod("reacidification", signature(x="SpatRaster"), 
	function(x, method="LiTAS", decay=0.25, acidification=0, years=10, ..., filename="", overwrite=FALSE, wopt=list()) {
		yrs <- if (length(years)==1) 1:years else years
		rate <- limeRate(x, method=method, ...)
		exal <- x$ECEC * x$TAS / 100 
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
		names(out) <- paste0("y_", yrs)
		if (filename != "") {
			out <- terra::writeRaster(out, filename, overwrite=overwrite, wopt=wopt)
		}
		out
	}
)


.lime_time <- function(years=15, exch_ac, ECEC, TAS, acidification=0, decay=0.22, rates=FALSE) {
	y <- data.frame(exch_ac=exch_ac, ECEC=ECEC, TAS=TAS)
	nms <- c("year", apply(y, 1, \(i) paste(i, collapse="_")))
	rate <- limeRate(y, method="my", check_Ca=FALSE, TAS=y$TAS)
	exal <- y$ECEC * y$TAS / 100 
	if (rates) {
		y$exch_ac <- pmin(y$exch_ac, exal + decay + acidification)
		rate2 <- limeRate(y, method="my", check_Ca=FALSE, TAS=y$TAS)
		out <- data.frame(c(0, 1), rbind(rate, rate2))	
		rownames(out) <- c("initial", "maintenance")
	} else {
		yrs <- if (length(years)==1) 1:years else years
		exal <- sapply(exal, \(i) i + yrs * decay + acidification)
		exal <- t(pmin(t(exal), y$exch_ac))
		out <- data.frame(yrs, exal)
	}
	colnames(out) <- nms
	out
}
