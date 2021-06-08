
# https://content.ces.ncsu.edu/soil-acidity-and-liming-basic-information-for-farmers-and-gardeners
nc_lime <- function(lime, CCE, pH, Ac) {
	lime <- lime / 2471    # kg/ha
	lime <- lime * CCE/90  # standard CCE = 90%
	lime * (6.6 - pH) / Ac 
}
# https://www.ncagr.gov/agronomi/obt21.htm
# Estimtes Ac in meq/100 cm3

nc_acidity <- function(BpH) {
	4 * (6.6 - BpH)
}


