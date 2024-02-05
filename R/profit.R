
if (!isGeneric("profit")) {setGeneric("profit", function(cost, benefit, ...) standardGeneric("profit"))}


setMethod("profit", signature(cost="numeric", benefit="numeric"), 
	function(cost, benefit, nyears, interest_rate, maintain=FALSE) {

		stopifnot(interest_rate >= 0)
		interest_rate <- interest_rate / 100
		stopifnot((cost >= 0) & (benefit >= 0) & (nyears > 0))
		# only compute for the first year, as the total amount will be the same
		# for all years (while the balance between principal and interest changes)
		nyears <- pmax(1, round(nyears))
		if (interest_rate == 0) {
			pay <- cost / nyears
		} else {
			interest_pay <- cost * interest_rate
			principal_pay <- interest_rate * cost / ((1+interest_rate)^nyears - 1)
			pay <- principal_pay + interest_pay
		}
		if (maintain) {
			benefit - (pay + pay / nyears)		
		} else {
			# average proportion of benefit 
			avgben <- sapply(nyears, \(n) mean(seq(0, 1, 1/n)[-1]))
			avgben * benefit - pay
		}
	}
)

 
setMethod("profit", signature(cost="SpatRaster", benefit="SpatRaster"), 
	function(cost, benefit, nyears, interest_rate, maintain=FALSE, filename="", overwrite=FALSE, ...) {
		stopifnot(inherits(nyears, "SpatRaster"))
		stopifnot(inherits(interest_rate, "numeric"))
		stopifnot(interest_rate >= 0)
		interest_rate <- interest_rate / 100
		
		nyears <- max(round(nyears), 1)
		if (interest_rate == 0) {
			pay <- cost / nyears
		} else {
			interest_pay <- cost * interest_rate
			principal_pay <- interest_rate * cost / ((1+interest_rate)^nyears - 1)
			pay <- principal_pay + interest_pay
		}
		if (maintain) {
			out <- benefit - (pay + pay / nyears)		
		} else {
			s <- sapply(1:50, \(n) mean(seq(0, 1, 1/n)[-1]))
			avgben <- subst(nyears, 1:50, s, 0.5)
			out <- avgben * benefit - pay
		}
		if (filename != "") {
			out <- writeRaster(out, filename, overwrite=overwrite, ...)
		}
		out
	}
)

