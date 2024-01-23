# annualized cost of lime investments for the initial number of years in which 
# you pay off the initial amount of lime needed.
# After that period you only have the maintenance cost.
# - "initial" and "maintenance" in some currency unit
# - "interest_rate" is a fraction (0.1 = 10%)
# - "years" is the number of years to pay off the initial amount.

annualized_cost <- function(initial, maintenance, interest_rate, years) {

	stopifnot((interest_rate > 0) & (interest_rate < 1))
	stopifnot((initial >= 0) & (maintenance >= 0) & (years > 0))

    # subtract maintenance such that in the first year you have the
	# (reduced) initial rate _and_ the maintenance rate. That makes conceptual sense
	# and it also assures that the cost is the same in all years.
    initial <- pmax(0, initial - maintenance)

    # only compute for the first year, as the total amount will be the same
    # for all years (while the balance between principal and interest changes)
    interest_pay <- initial * interest_rate

    principal_pay <- interest_rate * initial / ((1+interest_rate)^years - 1)
	
    # paying interest for half a year on the maintenance application
    principal_pay + interest_pay + maintenance * (1 + interest_rate/2)
}
 
