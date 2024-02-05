
if (!isGeneric("profit")) {setGeneric("profit", function(cost, benefit, ...) standardGeneric("profit"))}


setMethod("profit", signature(cost="numeric", benefit="numeric"), 
	function(cost, benefit, nyears, interest_rate) {

		stopifnot(interest_rate >= 0)
		interest_rate <- interest_rate / 100
		stopifnot((cost >= 0) & (benefit >= 0) & (nyears > 0))
		# only compute for the first year, as the total amount will be the same
		# for all years (while the balance between principal and interest changes)
		if (interest_rate == 0) {
			pay <- cost / nyears
		} else {
			interest_pay <- cost * interest_rate
			principal_pay <- interest_rate * cost / ((1+interest_rate)^nyears - 1)
			pay <- principal_pay + interest_pay
		}
		# average proportion of benefit 
		avgben <- mean(seq(0, 1, 1/(nyears+1))[-1])
		avgben * benefit - pay
	}
)

 
setMethod("profit", signature(cost="SpatRaster", benefit="SpatRaster"), 
	function(cost, benefit, nyears, interest_rate, filename="", overwrite=FALSE, ...) {
		stopifnot(inherits(nyears, "SpatRaster"))
		stopifnot(inherits(interest_rate, "numeric"))
		stopifnot(interest_rate >= 0)
		interest_rate <- interest_rate / 100
		if (interest_rate == 0) {
			pay <- cost / nyears
		} else {
			interest_pay <- cost * interest_rate
			principal_pay <- interest_rate * cost / ((1+interest_rate)^nyears - 1)
			pay <- principal_pay + interest_pay
		}
		avgben <- mean(seq(0, 1, 1/(nyears+1))[-1])
		out <- avgben * benefit / 2  - pay
		if (filename != "") {
			out <- writeRaster(out, filename, overwrite=overwrite, ...)
		}
		out
	}
)

