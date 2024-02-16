

.npv <- function(benefit, nyears, discount_rate) {
	if (is.na(nyears)) return(NA)
	decay <- rev(seq(0, 1, 1/nyears)[-1])
	discount <- ((1 + discount_rate)^(1:nyears))
	sum(decay * benefit/discount)
}



if (!isGeneric("NPV_lime")) {setGeneric("NPV_lime", function(x, ...) standardGeneric("NPV_lime"))}


setMethod("NPV_lime", signature(x="numeric"), 
	function(x, nyears, discount_rate) {
		stopifnot(discount_rate >= 0)
		discount_rate <- discount_rate / 100
		stopifnot((x >= 0) & (nyears > 0))
		nyears <- pmax(1, round(nyears))
		.npv(x, nyears, discount_rate)
	}
)

.npvSR <- function(benefit, nyears, discount_rate) {
	sapply(1:length(benefit), \(i) .npv(benefit[i], nyears[i], discount_rate=discount_rate))
}


setMethod("NPV_lime", signature(x="SpatRaster"), 
	function(x, nyears, discount_rate, filename="", overwrite=FALSE, ...) {
		stopifnot(inherits(nyears, "SpatRaster"))
		stopifnot(inherits(discount_rate, "numeric"))
		stopifnot(discount_rate >= 0)
		discount_rate <- discount_rate / 100
		nyears <- max(round(nyears), 1)
		npv <- terra::lapp(c(x, nyears), .npvSR, discount_rate=discount_rate)
		
		if (filename != "") {
			npv <- terra::writeRaster(npv, filename, overwrite=overwrite, ...)
		}
		npv
	}
)




