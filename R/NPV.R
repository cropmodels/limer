

.npv <- function(benefit, nyears, interest_rate, maintain=FALSE) {
	decay <- rev(seq(0, 1, 1/nyears)[-1])
	discount <- ((1 + interest_rate)^(1:nyears))
	sum(decay * benefit/discount)
}



if (!isGeneric("NPV")) {setGeneric("NPV", function(x, ...) standardGeneric("NPV"))}


setMethod("NPV", signature(x="numeric"), 
	function(x, nyears, interest_rate) {
		stopifnot(interest_rate >= 0)
		interest_rate <- interest_rate / 100
		stopifnot((benefit >= 0) & (nyears > 0))
		nyears <- pmax(1, round(nyears))
		.npv(benefit, nyears, interest_rate, maintain)
	}
)

.npvSR <- function(benefit, nyears, interest_rate, maintain=FALSE) {
	sapply(1:length(benefit), \(i) .npv(benefit[i], nyears[i], interest_rate=interest_rate, maintain=maintain))
}


setMethod("NPV", signature(x="SpatRaster"), 
	function(x, nyears, interest_rate, maintain=FALSE, filename="", overwrite=FALSE, ...) {
		stopifnot(inherits(nyears, "SpatRaster"))
		stopifnot(inherits(interest_rate, "numeric"))
		stopifnot(interest_rate >= 0)
		interest_rate <- interest_rate / 100
		nyears <- max(round(nyears), 1)
		npv <- lapp(c(benefit, nyears), .npvSR, interest_rate=interest_rate, maintain=maintain)
		
		if (filename != "") {
			npv <- writeRaster(npv, filename, overwrite=overwrite, ...)
		}
		npv
	}
)




