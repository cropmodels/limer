

if (!isGeneric("annualCost")) {setGeneric("annualCost", function(initial, maintenance, ...) standardGeneric("annualCost"))}


setMethod("annualCost", signature(initial="numeric", maintenance="numeric"), 
	function(initial, maintenance, interest_rate, nyears) {

		stopifnot((interest_rate >= 0) & (interest_rate < 100))
		interest_rate <- interest_rate / 100
		stopifnot((initial >= 0) & (maintenance >= 0) & (nyears > 0))

		# subtract maintenance such that in the first year you have the
		# (reduced) initial rate _and_ the maintenance rate. That makes conceptual sense
		# and it also assures that the cost is the same in all years.
		initial <- pmax(0, initial - maintenance)

		# only compute for the first year, as the total amount will be the same
		# for all years (while the balance between principal and interest changes)
		if (interest_rate == 0) {
			maintenance + initial / nyears
		} else {
			interest_pay <- initial * interest_rate
			principal_pay <- interest_rate * initial / ((1+interest_rate)^nyears - 1)
		# paying interest for half a year on the maintenance application
			principal_pay + interest_pay + maintenance * (1 + interest_rate/2)
		}
	}
)

 
setMethod("annualCost", signature(initial="SpatRaster", maintenance="ANY"), 
	function(initial, maintenance, interest_rate, nyears) {
		stopifnot((interest_rate >= 0) & (interest_rate < 1))
		interest_rate <- interest_rate / 100
		stopifnot(nyears > 0)
		stopifnot(inherits(maintenance, "SpatRaster") || inherits(maintenance, "numeric"))
		if (inherits(maintenance, "numeric")) {
			stopifnot(maintenance >= 0)
		}
		initial <- max(initial - maintenance, 0)
		if (interest_rate == 0) {
			maintenance + initial / nyears
		} else {
			interest_pay <- initial * interest_rate
			principal_pay <- interest_rate * initial / ((1+interest_rate)^nyears - 1)
			principal_pay + interest_pay + maintenance * (1 + interest_rate/2)
		}
	}
)

