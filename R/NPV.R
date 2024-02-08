

.npv <- function(benefit, nyears, discount_rate, maintain=FALSE) {
	decay <- rev(seq(0, 1, 1/nyears)[-1])
	discount <- ((1 + discount_rate)^(1:nyears))
	sum(decay * benefit/discount)
}



if (!isGeneric("NPV_lime")) {setGeneric("NPV", function(x, ...) standardGeneric("NPV"))}


setMethod("NPV_lime", signature(x="numeric"), 
	function(x, nyears, discount_rate) {
		stopifnot(discount_rate >= 0)
		discount_rate <- discount_rate / 100
		stopifnot((benefit >= 0) & (nyears > 0))
		nyears <- pmax(1, round(nyears))
		.npv(benefit, nyears, discount_rate, maintain)
	}
)

.npvSR <- function(benefit, nyears, discount_rate, maintain=FALSE) {
	sapply(1:length(benefit), \(i) .npv(benefit[i], nyears[i], discount_rate=discount_rate, maintain=maintain))
}


setMethod("NPV_lime", signature(x="SpatRaster"), 
	function(x, nyears, discount_rate, maintain=FALSE, filename="", overwrite=FALSE, ...) {
		stopifnot(inherits(nyears, "SpatRaster"))
		stopifnot(inherits(discount_rate, "numeric"))
		stopifnot(discount_rate >= 0)
		discount_rate <- discount_rate / 100
		nyears <- max(round(nyears), 1)
		npv <- lapp(c(benefit, nyears), .npvSR, discount_rate=discount_rate, maintain=maintain)
		
		if (filename != "") {
			npv <- writeRaster(npv, filename, overwrite=overwrite, ...)
		}
		npv
	}
)




